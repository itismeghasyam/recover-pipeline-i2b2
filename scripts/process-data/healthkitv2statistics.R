library(dplyr)

dataset <- "healthkitv2statistics"

vars <- 
  selected_vars %>% 
  filter(grepl(dataset, Export, ignore.case = TRUE)) %>% 
  pull(Variable)

df <- 
  arrow::open_dataset(file.path(downloadLocation, glue::glue("dataset_{dataset}"))) %>% 
  select(all_of(vars)) %>% 
  dplyr::filter(Type=="DailySteps") %>% 
  rename(concept = Type) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  collect()

colnames(df) <- tolower(colnames(df))

criteria <- selected_vars$Variable=="Steps" & selected_vars$Export=="fitbitdailydata"

bounds <- data.frame(Lower_Bound = selected_vars$Lower_Bound[criteria], 
                     Upper_Bound = selected_vars$Upper_Bound[criteria])

df_filtered <- df
df_filtered$value <- ifelse(df_filtered$value < bounds$Lower_Bound | 
                              df_filtered$value > bounds$Upper_Bound,
                            NA,
                            df_filtered$value)

df_melted_filtered <- 
  df_filtered %>% 
  select(if("participantidentifier" %in% colnames(.)) "participantidentifier",
         dplyr::matches("(?<!_)date(?!_)", perl = T),
         if("concept" %in% colnames(.)) "concept",
         if("value" %in% colnames(.)) "value") %>% 
  tidyr::drop_na("value") %>% 
  mutate(value = as.numeric(value))
cat("Melt and filtering step completed.\n")

df_summarized <- 
  df_melted_filtered %>% 
  rename(enddate = "date") %>% 
  select(all_of(c("participantidentifier", "startdate", "enddate", "concept", "value"))) %>% 
  recoverSummarizeR::stat_summarize() %>% 
  distinct()
cat("recoverSummarizeR::stat_summarize() completed.\n")

tmp_concept_replacements <- c("dailysteps" = "steps")

output_concepts <- 
  process_df(df_summarized, 
             concept_map, 
             concept_replacements_reversed = tmp_concept_replacements, 
             concept_map_concepts = "CONCEPT_CD", 
             concept_map_units = "UNITS_CD") %>% 
  dplyr::mutate(nval_num = signif(nval_num, 9)) %>% 
  dplyr::arrange(concept) %>% 
  dplyr::mutate(dplyr::across(.cols = dplyr::everything(), .fns = as.character)) %>% 
  replace(is.na(.), "<null>") %>% 
  dplyr::filter(nval_num != "<null>" | tval_char != "<null>")
cat("recoverSummarizeR::process_df() completed.\n")

output_concepts %>% 
  write.csv(file.path(outputConceptsDir, glue::glue("{dataset}.csv")), row.names = F)
cat(glue::glue("output_concepts written to {file.path(outputConceptsDir, paste0(dataset, '.csv'))}"),"\n")

rm(dataset,
   vars,
   df,
   criteria,
   bounds,
   df_filtered,
   df_melted_filtered,
   df_summarized,
   tmp_concept_replacements,
   output_concepts)
