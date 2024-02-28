library(dplyr)

dataset <- c("fitbitdevices", "healthkitv2samples")

cat(glue::glue("Transforming device data for {dataset}"),"\n")

vars <- list(fitbitdevices = c("ParticipantIdentifier", "Device", "Date"), 
             healthkitv2samples = c("ParticipantIdentifier", "Device_Model", "Device_Manufacturer", "StartDate", "Date"))

df <- 
  sapply(dataset, function(x) {
    tmp <- vars[[x]]
    arrow::open_dataset(file.path(downloadLocation, glue::glue("dataset_{x}"))) %>% 
      select(all_of(tmp)) %>% 
      collect()
  })

df <- 
  lapply(df, function(x) {
    colnames(x) <- tolower(colnames(x))
    return(x)
  })

df$fitbitdevices <- 
  df$fitbitdevices %>% 
  mutate(type = ifelse(is.na(device) | device == "", NA, "fitbit"))

df$healthkitv2samples <- 
  df$healthkitv2samples %>% 
  mutate(type = case_when(device_manufacturer %in% c("Apple", "Apple Inc.") ~ "Apple",
                          device_manufacturer %in% c("Garmin") ~ "Garmin",
                          device_manufacturer %in% c("Polar Electro Oy") ~ "Polar",
                          device_model %in% c("HRM808S") ~ "HRM808S",
                          .default = "Other"))

df_joined <- 
  bind_rows(df$fitbitdevices %>% 
              select(all_of(c("participantidentifier", "type", "date"))), 
            df$healthkitv2samples %>% 
              select(all_of(c("participantidentifier", "type", "startdate", "date")))) %>% 
  select(all_of(c("participantidentifier", "type", "startdate", "date"))) %>% 
  rename(enddate = date) %>% 
  mutate(startdate = lubridate::as_date(startdate),
         enddate = lubridate::as_date(enddate)) %>% 
  group_by(participantidentifier, startdate, enddate) %>% 
  summarise(type = toString(sort(unique(type)))) %>% 
  mutate(concept = "mhp:device") %>% 
  rename(value = type) %>% 
  select(all_of(c("participantidentifier", "startdate", "enddate", "concept", "value"))) %>% 
  ungroup()

output_concepts <- 
  process_df(df_joined, concept_map, concept_map_concepts = "CONCEPT_CD", concept_map_units = "UNITS_CD") %>% 
  # dplyr::mutate(nval_num = signif(nval_num, 9)) %>% 
  dplyr::arrange(concept) %>% 
  dplyr::mutate(dplyr::across(.cols = dplyr::everything(), .fns = as.character)) %>% 
  replace(is.na(.), "<null>") %>% 
  dplyr::filter(nval_num != "<null>" | tval_char != "<null>")
cat("recoverSummarizeR::process_df() completed.\n")

output_concepts %>% 
  write.csv(file.path(outputConceptsDir, "participant_devices.csv"), row.names = F)
cat(glue::glue("participant_devices written to {file.path(outputConceptsDir, 'participant_devices.csv')}"), "\n")

cat(glue::glue("Finished tansforming device data for {dataset}"),"\n\n")

rm(dataset,
   vars,
   df,
   df_joined,
   output_concepts)
