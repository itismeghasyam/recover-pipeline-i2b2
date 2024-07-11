library(dplyr)

dataset <- "fitbitactivitylogs"

cat(glue::glue("Transforming data for {dataset}"),"\n")

# Get variables for this dataset
vars <- 
  selected_vars %>% 
  filter(grepl(dataset, Export, ignore.case = TRUE)) %>% 
  pull(Variable)

# Load the desired subset of this dataset in memory
df <- 
  arrow::open_dataset(file.path(downloadLocation, glue::glue("dataset_{dataset}"))) %>% 
  select(all_of(vars)) %>% 
  collect()

colnames(df) <- tolower(colnames(df))

# Create lists for ID variables and i2b2 concept variables
excluded_concepts <- c("participantidentifier", "startdate", "enddate")

approved_concepts_summarized <- 
  setdiff(
    tolower(selected_vars$Variable[selected_vars$Export==dataset]),
    excluded_concepts
  )

df[approved_concepts_summarized] <- lapply(df[approved_concepts_summarized], as.numeric)

# Get QA/QC ranges for variables and exclude values outside the ranges
bounds <- 
  selected_vars %>% 
  filter(grepl(dataset, Export, ignore.case = TRUE),
         tolower(Variable) %in% approved_concepts_summarized) %>% 
  select(Variable, Lower_Bound, Upper_Bound) %>% 
  mutate(Variable = tolower(Variable))

df_filtered <- df
for (col_name in names(df_filtered)) {
  if (col_name %in% bounds$Variable) {
    lower_bound <- bounds$Lower_Bound[bounds$Variable == col_name]
    upper_bound <- bounds$Upper_Bound[bounds$Variable == col_name]
    
    df_filtered[[col_name]] <- ifelse(df_filtered[[col_name]] < lower_bound |
                                        df_filtered[[col_name]] > upper_bound,
                                      NA,
                                      df_filtered[[col_name]])
  }
}

# Pivot data frame from long to wide
df_melted_filtered <- 
  df_filtered %>% 
  recoverutils::melt_df(excluded_concepts = excluded_concepts) %>% 
  select(if("participantidentifier" %in% colnames(.)) "participantidentifier",
         dplyr::matches("(?<!_)date(?!_)", perl = T),
         if("concept" %in% colnames(.)) "concept",
         if("value" %in% colnames(.)) "value") %>% 
  tidyr::drop_na("value") %>% 
  mutate(value = as.numeric(value))
cat("recoverutils::melt_df() completed.\n")

# Generate i2b2 summaries
df_summarized <- 
  df_melted_filtered %>% 
  select(all_of(c("participantidentifier", "startdate", "enddate", "concept", "value"))) %>% 
  recoverutils::stat_summarize() %>% 
  distinct()
cat("recoverutils::stat_summarize() completed.\n")

# Add i2b2 columns from concept map (ontology file) and clean the output
output_concepts <- 
  process_df(df_summarized, concept_map, concept_replacements_reversed, concept_map_concepts = "CONCEPT_CD", concept_map_units = "UNITS_CD") %>% 
  dplyr::mutate(nval_num = signif(nval_num, 9)) %>% 
  dplyr::arrange(concept) %>% 
  dplyr::mutate(dplyr::across(.cols = dplyr::everything(), .fns = as.character)) %>% 
  replace(is.na(.), "<null>") %>% 
  dplyr::filter(nval_num != "<null>" | tval_char != "<null>")
cat("recoverutils::process_df() completed.\n")

# Identify the participants who have output concepts derived from fitbit variables
fitbit_participants <- 
  sort(unique(output_concepts$participantidentifier)) %>% 
  as.data.frame() %>% 
  dplyr::rename(participantidentifier = ".")

fitbit_participants %>% 
  write.csv(file.path(outputConceptsDir, "fitbit_participants.csv"), 
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
   excluded_concepts, 
   approved_concepts_summarized, 
   bounds,
   df_filtered,
   col_name,
   lower_bound,
   upper_bound,
   df_melted_filtered, 
   df_summarized, 
   output_concepts,
   fitbit_participants)
