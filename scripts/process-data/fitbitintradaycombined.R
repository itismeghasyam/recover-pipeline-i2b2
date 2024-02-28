library(dplyr)

dataset <- "fitbitintradaycombined"

cat(glue::glue("Transforming data for {dataset}"),"\n")

vars <- 
  selected_vars %>% 
  filter(grepl(dataset, Export, ignore.case = TRUE)) %>% 
  pull(Variable)

df <- 
  arrow::open_dataset(file.path(downloadLocation, glue::glue("dataset_{dataset}"))) %>% 
  select(all_of(vars)) %>% 
  mutate(
    DeepSleepSummaryBreathRate = as.numeric(DeepSleepSummaryBreathRate),
    RemSleepSummaryBreathRate = as.numeric(RemSleepSummaryBreathRate),
    FullSleepSummaryBreathRate = as.numeric(FullSleepSummaryBreathRate),
    LightSleepSummaryBreathRate = as.numeric(LightSleepSummaryBreathRate),
  ) %>% 
  mutate(
    drop_row = ifelse(
      test = 
        is.na(DeepSleepSummaryBreathRate) & 
        is.na(RemSleepSummaryBreathRate) & 
        is.na(FullSleepSummaryBreathRate) & 
        is.na(LightSleepSummaryBreathRate),
      yes = "drop", 
      no = "keep")) %>% 
  dplyr::filter(drop_row=="keep") %>% 
  select(-(dplyr::any_of(c("drop_row")))) %>% 
  dplyr::filter(
    DeepSleepSummaryBreathRate >= 0,
    RemSleepSummaryBreathRate >= 0,
    FullSleepSummaryBreathRate >= 0,
    LightSleepSummaryBreathRate >= 0
  ) %>% 
  collect()

colnames(df) <- tolower(colnames(df))

excluded_concepts <- c("participantidentifier", "datetime")

approved_concepts_summarized <- 
  setdiff(
    tolower(selected_vars$Variable[selected_vars$Export==dataset]),
    excluded_concepts
  )

df[approved_concepts_summarized] <- lapply(df[approved_concepts_summarized], as.numeric)

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

df_melted_filtered <- 
  df_filtered %>% 
  recoverSummarizeR::melt_df(excluded_concepts = excluded_concepts) %>% 
  select(if("participantidentifier" %in% colnames(.)) "participantidentifier",
         dplyr::matches("(?<!_)date(?!_)", perl = T),
         if("concept" %in% colnames(.)) "concept",
         if("value" %in% colnames(.)) "value") %>% 
  tidyr::drop_na("value") %>% 
  mutate(value = as.numeric(value))
cat("recoverSummarizeR::melt_df() completed.\n")

df_summarized <- 
  df_melted_filtered %>% 
  rename(startdate = dplyr::any_of(c("date", "datetime"))) %>% 
  mutate(enddate = if (!("enddate" %in% names(.))) NA else enddate) %>% 
  select(all_of(c("participantidentifier", "startdate", "enddate", "concept", "value"))) %>% 
  recoverSummarizeR::stat_summarize() %>% 
  distinct()
cat("recoverSummarizeR::stat_summarize() completed.\n")

output_concepts <- 
  process_df(df_summarized, concept_map, concept_replacements_reversed, concept_map_concepts = "CONCEPT_CD", concept_map_units = "UNITS_CD") %>% 
  dplyr::mutate(nval_num = signif(nval_num, 9)) %>% 
  dplyr::arrange(concept) %>% 
  dplyr::mutate(dplyr::across(.cols = dplyr::everything(), .fns = as.character)) %>% 
  replace(is.na(.), "<null>") %>% 
  dplyr::filter(nval_num != "<null>" | tval_char != "<null>")
cat("recoverSummarizeR::process_df() completed.\n")

output_concepts %>% 
  write.csv(file.path(outputConceptsDir, glue::glue("{dataset}.csv")), row.names = F)
cat(glue::glue("output_concepts written to {file.path(outputConceptsDir, paste0(dataset, '.csv'))}"), "\n")

cat(glue::glue("Finished transforming data for {dataset}"),"\n\n")

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
   output_concepts)
