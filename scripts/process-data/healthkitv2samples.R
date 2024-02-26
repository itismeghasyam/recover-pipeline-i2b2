library(dplyr)

dataset <- "healthkitv2samples"

vars <- 
  selected_vars %>% 
  filter(grepl(dataset, Export, ignore.case = TRUE)) %>% 
  pull(Variable)

df <- 
  arrow::open_dataset(file.path(downloadLocation, glue::glue("dataset_{dataset}"))) %>% 
  select(all_of(vars)) %>% 
  dplyr::filter(
    Type=="RespiratoryRate" | 
      Type=="HeartRate" | 
      Type=="HeartRateVariability" | 
      Type=="OxygenSaturation"
    ) %>% 
  dplyr::filter(
    (Device_Manufacturer %in% 
       c("Apple", "Apple Inc.", "Garmin", "Polar Electro Oy") & 
       !Device_Model %in% c("iPhone", "iPod")
     ) | Device_Model=="HRM808S") %>% 
  # select(-(any_of(c("Device_Model", "Device_Manufacturer")))) %>%
  rename(concept = Type) %>% 
  collect()

colnames(df) <- tolower(colnames(df))

df_melted_filtered <- 
  df %>% 
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

tmp_concept_replacements <- c("respiratoryrate" = "breathingrate",
                              "heartratevariability" = "hrv",
                              "heartrate" = "avghr",
                              "oxygensaturation" = "spo2avg")

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
   df_melted_filtered, 
   df_summarized, 
   tmp_concept_replacements,
   output_concepts)
