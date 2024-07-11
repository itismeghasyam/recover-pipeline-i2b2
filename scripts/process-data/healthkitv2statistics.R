dataset <- "healthkitv2statistics"

cat(glue::glue("Transforming data for {dataset}"),"\n")

# Get variables for this dataset
vars <- 
  selected_vars %>% 
  filter(grepl(dataset, Export, ignore.case = TRUE)) %>% 
  pull(Variable)

# Exclude participants who have i2b2 summaries from fitbit data
participants_to_exclude <- 
  read.csv(file.path(outputConceptsDir, "fitbit_participants.csv")) %>% 
  pull(participantidentifier)

# Load the desired subset of this dataset in memory
df <- 
  arrow::open_dataset(s3$path(str_subset(dataset_paths, dataset))) %>% 
  select(all_of(vars)) %>% 
  dplyr::filter(Type=="DailySteps") %>% 
  dplyr::filter(!(ParticipantIdentifier %in% participants_to_exclude)) %>% 
  rename(concept = Type) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  collect()

colnames(df) <- tolower(colnames(df))

# Get QA/QC ranges for variables and exclude values outside the ranges
criteria <- selected_vars$Variable=="Steps" & selected_vars$Export=="fitbitdailydata"

bounds <- data.frame(Lower_Bound = selected_vars$Lower_Bound[criteria], 
                     Upper_Bound = selected_vars$Upper_Bound[criteria])

df_filtered <- df
df_filtered$value <- ifelse(df_filtered$value < bounds$Lower_Bound | 
                              df_filtered$value > bounds$Upper_Bound,
                            NA,
                            df_filtered$value)

# Pivot data frame from long to wide
df_melted_filtered <- 
  df_filtered %>% 
  select(if("participantidentifier" %in% colnames(.)) "participantidentifier",
         dplyr::matches("(?<!_)date(?!_)", perl = T),
         if("concept" %in% colnames(.)) "concept",
         if("value" %in% colnames(.)) "value") %>% 
  tidyr::drop_na("value") %>% 
  mutate(value = as.numeric(value))
cat("Melt and filtering step completed.\n")

# Generate i2b2 summaries
df_summarized <- 
  df_melted_filtered %>% 
  rename(enddate = "date") %>% 
  select(all_of(c("participantidentifier", "startdate", "enddate", "concept", "value"))) %>% 
  recoverutils::stat_summarize() %>% 
  distinct()
cat("recoverutils::stat_summarize() completed.\n")

tmp_concept_replacements <- c("dailysteps" = "steps")

# Add i2b2 columns from concept map (ontology file) and clean the output
output_concepts <- 
  recoverutils::process_df(df_summarized, 
             concept_map, 
             concept_replacements_reversed = tmp_concept_replacements, 
             concept_map_concepts = "CONCEPT_CD", 
             concept_map_units = "UNITS_CD") %>% 
  dplyr::mutate(nval_num = signif(nval_num, 9)) %>% 
  dplyr::arrange(concept) %>% 
  dplyr::mutate(dplyr::across(.cols = dplyr::everything(), .fns = as.character)) %>% 
  replace(is.na(.), "<null>") %>% 
  dplyr::filter(nval_num != "<null>" | tval_char != "<null>")
cat("recoverutils::process_df() completed.\n")

# Identify the participants who have output concepts derived from healthkit variables
curr_hk_participants <- 
  sort(unique(output_concepts$participantidentifier)) %>% 
  as.data.frame() %>% 
  dplyr::rename(participantidentifier = ".")

prev_hk_participants <- 
  read.csv(file.path(outputConceptsDir, "hk_participants.csv"))

hk_participants <- 
  dplyr::bind_rows(prev_hk_participants, 
                   curr_hk_participants) %>% 
  distinct()

hk_participants %>% 
  write.csv(file.path(outputConceptsDir, "hk_participants.csv"), 
            row.names = F)

# Write the output
output_concepts %>% 
  write.csv(file.path(outputConceptsDir, glue::glue("{dataset}.csv")), row.names = F)
cat(glue::glue("output_concepts written to {file.path(outputConceptsDir, paste0(dataset, '.csv'))}"),"\n")

cat(glue::glue("Finished transforming data for {dataset}"),"\n\n")

# Remove objects created here from the global environment
rm(dataset,
   vars,
   df,
   participants_to_exclude,
   criteria,
   bounds,
   df_filtered,
   df_melted_filtered,
   df_summarized,
   tmp_concept_replacements,
   output_concepts,
   curr_hk_participants,
   prev_hk_participants,
   hk_participants)
