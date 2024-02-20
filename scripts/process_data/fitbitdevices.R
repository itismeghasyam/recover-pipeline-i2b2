library(dplyr)

vars <- 
  selected_vars %>% 
  filter(grepl("fitbitdevices", Export, ignore.case = TRUE)) %>% 
  pull(Variable)

df <- 
  arrow::open_dataset(file.path(downloadLocation, "dataset_fitbitdevices")) %>% 
  select(all_of(vars)) %>% 
  collect()

colnames(df) <- tolower(colnames(df))

participant_devices <- 
  df %>% 
  distinct(participantidentifier) %>% 
  mutate(device = "fitbit")

participant_devices %>% 
  write.csv(file.path(outputConceptsDir, "participant_devices.csv"), row.names = F)
cat(glue::glue("participant_devices written to {file.path(outputConceptsDir, 'participant_devices.csv')}\n"))

rm(vars,
   df,
   participant_devices)
