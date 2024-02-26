library(dplyr)

dataset <- c("fitbitdevices", "healthkitv2samples")

devices_list <- 
  lapply(dataset, function(x) {
    vars <- 
      selected_vars %>% 
      filter(grepl(x, Export, ignore.case = TRUE)) %>% 
      pull(Variable)
    
    df <- 
      arrow::open_dataset(file.path(downloadLocation, glue::glue("dataset_{x}"))) %>% 
      select(all_of(vars)) %>% 
      collect()
    
    colnames(df) <- tolower(colnames(df))
    
    participant_devices <- 
      df %>% 
      distinct(participantidentifier) %>% 
      mutate(
        device = ifelse(
          test = x=="fitbitdevices", 
          yes = "fitbit", 
          no = "healthkit"
        )
      )
    
    participant_devices %>% 
      write.csv(file.path(outputConceptsDir, glue::glue("participant_devices_{x}.csv")), row.names = F)
    cat(glue::glue("participant_devices_fitbit written to {file.path(outputConceptsDir, glue::glue('participant_devices_{x}.csv'))}"), "\n")
  })

fitbit_devices <- read.csv(file.path(outputConceptsDir, glue::glue("participant_devices_{dataset[1]}.csv")))
healthkit_devices <- read.csv(file.path(outputConceptsDir, glue::glue("participant_devices_{dataset[2]}.csv")))

rm(dataset,
   devices_list)

# Testing
vars <- 
  selected_vars %>% 
  filter(grepl(dataset[2], Export, ignore.case = TRUE)) %>% 
  pull(Variable)

df <- 
  arrow::open_dataset(file.path(downloadLocation, glue::glue("dataset_{dataset[2]}"))) %>% 
  select(all_of(c("ParticipantIdentifier", "Device_Manufacturer"))) %>% 
  collect()

colnames(df) <- tolower(colnames(df))

participant_devices <- 
  df %>% 
  distinct(participantidentifier)
