library(dplyr)

dataset <- "healthkitv2samples"

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
  select(-(any_of(c("Device_Model", "Device_Manufacturer")))) %>%
  rename(concept = Type) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  collect()

colnames(df) <- tolower(colnames(df))

# Get QA/QC ranges for variables and exclude values outside the ranges
criteria <- list(selected_vars$Variable=="AverageHeartRate" & selected_vars$Export=="fitbitactivitylogs",
             selected_vars$Variable=="BreathingRate" & selected_vars$Export=="fitbitdailydata",
             selected_vars$Variable=="SpO2_Avg" & selected_vars$Export=="fitbitdailydata")

bounds <- data.frame(Variable = c("HeartRate", "RespiratoryRate", "OxygenSaturation"),
                     Lower_Bound = sapply(criteria, function(x) selected_vars$Lower_Bound[x]), 
                     Upper_Bound = sapply(criteria, function(x) selected_vars$Upper_Bound[x]))

bounds$Lower_Bound[bounds$Variable=="OxygenSaturation"] <- bounds$Lower_Bound[bounds$Variable=="OxygenSaturation"]/100
bounds$Upper_Bound[bounds$Variable=="OxygenSaturation"] <- bounds$Upper_Bound[bounds$Variable=="OxygenSaturation"]/100

df_filtered <- df
for (i in 1:nrow(bounds)) {
  var <- bounds$Variable[i]
  lower <- bounds$Lower_Bound[i]
  upper <- bounds$Upper_Bound[i]
  
  df_filtered$value[df_filtered$concept==var] <- 
    ifelse(df_filtered$value[df_filtered$concept==var] < lower | df_filtered$value[df_filtered$concept==var] > upper,
           NA,
           df_filtered$value[df_filtered$concept==var])
}

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
  recoverSummarizeR::stat_summarize() %>% 
  distinct()
cat("recoverSummarizeR::stat_summarize() completed.\n")

tmp_concept_replacements <- c("respiratoryrate" = "breathingrate",
                              "heartratevariability" = "hrv",
                              "heartrate" = "avghr",
                              "oxygensaturation" = "spo2avg")

# Add i2b2 columns from concept map (ontology file) and clean the output
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

# Write the output
output_concepts %>% 
  write.csv(file.path(outputConceptsDir, glue::glue("{dataset}.csv")), row.names = F)
cat(glue::glue("output_concepts written to {file.path(outputConceptsDir, paste0(dataset, '.csv'))}"),"\n")

cat(glue::glue("Finished transforming data for {dataset}"),"\n\n")

# Remove objects created here from the global environment
rm(dataset,
   vars, 
   df, 
   df_melted_filtered, 
   df_summarized, 
   tmp_concept_replacements,
   output_concepts)
