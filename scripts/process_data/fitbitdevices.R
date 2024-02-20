library(dplyr)

dataset <- "fitbitdevices"

vars <- 
  selected_vars %>% 
  filter(grepl(dataset, Export, ignore.case = TRUE)) %>% 
  pull(Variable)

df <- 
  arrow::open_dataset(file.path(downloadLocation, glue::glue("dataset_{dataset}"))) %>% 
  select(all_of(vars)) %>% 
  collect()

colnames(df) <- tolower(colnames(df))

participant_devices <- 
  df %>% 
  distinct(participantidentifier) %>% 
  mutate(device = "fitbit")

participant_devices %>% 
  write.csv(file.path(outputConceptsDir, "participant_devices_fitbit.csv"), row.names = F)
cat(glue::glue("participant_devices_fitbit written to {file.path(outputConceptsDir, 'participant_devices_fitbit.csv')}\n"))

rm(vars,
   df,
   participant_devices)
