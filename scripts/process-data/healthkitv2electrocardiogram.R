# Functions

#' Create i2b2 summaries
#'
#' `ecg_stat_summarize()` summarizes the ECG data in a data frame on 
#' specific time scales (all data and weekly) for the following statistics: 
#' Number of records.
#'
#' @param df A data frame with `participantidentifier`, `concept`, `value`, and 
#' some combination of `startdate`, `enddate`, `date`, or `datetime` columns.
#'
#' @return A data frame that differs in size from `df`. The original `value` 
#' column is dropped and replaced by a new `value` column containing the values 
#' of each summary statistic computed for each group of data (group by 
#' `participantidentifier` and `concept` for all data, and 
#' `participantidentifier`, `concept`, `year`, `week` for weekly summaries).
ecg_stat_summarize <- function(df) {
  cat("Running ecg_stat_summarize()...\n")
  
  if (!is.data.frame(df)) stop("df must be a data frame")
  if (!all(c("participantidentifier", "concept", "value") %in% colnames(df))) stop("'participantidentifier', 'concept', and 'value' columns must be present in df")
  
  summarize_alltime <- function(df) {
    if ("startdate" %in% colnames(df) & "enddate" %in% colnames(df)) {
      # Do nothing
    } else if ("date" %in% colnames(df)) {
      df <- 
        df %>% 
        dplyr::rename(startdate = date) %>% 
        dplyr::mutate(enddate = NA)
    } else if ("datetime" %in% colnames(df) & !"date" %in% colnames(df)) {
      df <- 
        df %>% 
        dplyr::rename(startdate = datetime) %>% 
        dplyr::mutate(enddate = NA)
    } else {
      stop("No 'startdate', 'enddate', 'date', or 'datetime' column found")
    }
    
    df %>%
      dplyr::select(participantidentifier, startdate, enddate, concept, value) %>%
      dplyr::group_by(participantidentifier, concept) %>%
      dplyr::reframe(startdate = lubridate::as_date(min(startdate)),
                     enddate = lubridate::as_date(max(enddate)),
                     numrecords = dplyr::n()) %>%
      dplyr::ungroup() %>% 
      tidyr::pivot_longer(cols = c(numrecords),
                          names_to = "stat",
                          values_to = "value") %>%
      dplyr::mutate(concept = paste0("mhp:summary:alltime:", stat, ":", concept)) %>%
      dplyr::select(participantidentifier, startdate, enddate, concept, value) %>%
      dplyr::distinct()
  }
  
  summarize_weekly <- function(df) {
    if ("startdate" %in% colnames(df)) {
      # Do nothing
    } else if ("date" %in% colnames(df)) {
      df <- 
        df %>% 
        dplyr::rename(startdate = date)
    } else if ("datetime" %in% colnames(df) & !"date" %in% colnames(df)) {
      df <- 
        df %>% 
        dplyr::rename(startdate = datetime)
    } else {
      stop("No 'startdate, enddate, date, or datetime' column found")
    }
    
    df %>%
      dplyr::select(participantidentifier, 
                    concept, 
                    value, 
                    startdate) %>% 
      dplyr::mutate(startdate = lubridate::as_date(startdate)) %>% 
      dplyr::filter(startdate >= lubridate::floor_date(min(startdate), 
                                                       unit = "week", 
                                                       week_start = 7)) %>% 
      dplyr::mutate(startdate = lubridate::floor_date(startdate, unit = "week", week_start = 7), 
                    enddate = startdate + lubridate::days(6)) %>% 
      dplyr::group_by(participantidentifier, 
                      concept, 
                      startdate, 
                      enddate) %>% 
      dplyr::reframe(numrecords = dplyr::n()) %>% 
      dplyr::ungroup() %>% 
      tidyr::pivot_longer(cols = c(numrecords), 
                          names_to = "stat", 
                          values_to = "value") %>% 
      dplyr::mutate(concept = paste0("mhp:summary:weekly:", stat, ":", concept)) %>% 
      dplyr::select(participantidentifier, startdate, enddate, concept, value) %>% 
      dplyr::distinct()
  }
  
  result <- 
    dplyr::bind_rows(summarize_alltime(df), 
                     summarize_weekly(df)) %>% 
    dplyr::distinct()
  
  return(result)
}

library(dplyr)

dataset <- "healthkitv2electrocardiogram"

cat(glue::glue("Transforming data for {dataset}"),"\n")

# Get variables for this dataset
vars <- 
  selected_vars %>% 
  filter(grepl(paste0(dataset, "$"), Export, ignore.case = TRUE, perl = TRUE)) %>% 
  pull(Variable)

# Exclude participants who have i2b2 summaries from fitbit data
participants_to_exclude <- 
  read.csv(file.path(outputConceptsDir, "fitbit_participants.csv")) %>% 
  pull(participantidentifier)

# Load the desired subset of this dataset in memory and do some feature engineering for derived variables
df <- 
  arrow::open_dataset(file.path(downloadLocation, glue::glue("dataset_{dataset}"))) %>% 
  select(all_of(c(vars))) %>% 
  dplyr::filter(!(ParticipantIdentifier %in% participants_to_exclude)) %>% 
  filter(Classification %in% c("SinusRhythm", "AtrialFibrillation")) %>% 
  collect() %>% 
  rename(EndDate = Date)

colnames(df) <- tolower(colnames(df))

# Create lists for ID variables and i2b2 concept variables
excluded_concepts <- c("participantidentifier", "startdate", "enddate", "date")

approved_concepts_summarized <- 
  setdiff(
    tolower(selected_vars$Variable[selected_vars$Export==dataset]),
    excluded_concepts
  )

# Pivot data frame from long to wide
df_melted_filtered <- 
  df %>% 
  mutate("SinusRhythm" = if_else(classification == "SinusRhythm", 1, NA),
         "AtrialFibrillation" = if_else(classification == "AtrialFibrillation", 1, NA)) %>% 
  select(-c(classification)) %>% 
  recoverSummarizeR::melt_df(excluded_concepts = excluded_concepts) %>% 
  select(if("participantidentifier" %in% colnames(.)) "participantidentifier",
         dplyr::matches("(?<!_)date(?!_)", perl = T),
         if("concept" %in% colnames(.)) "concept",
         if("value" %in% colnames(.)) "value") %>% 
  tidyr::drop_na("value") %>% 
  mutate(value = as.numeric(value))
cat("recoverSummarizeR::melt_df() completed.\n")

# Generate i2b2 summaries
df_summarized <- 
  df_melted_filtered %>% 
  select(all_of(c("participantidentifier", "startdate", "enddate", "concept", "value"))) %>% 
  ecg_stat_summarize() %>% 
  mutate(value = as.numeric(value)) %>% 
  distinct()
cat("recoverSummarizeR::stat_summarize() completed.\n")

# Add i2b2 columns from concept map (ontology file) and clean the output
output_concepts <- 
  process_df(df_summarized, 
             concept_map, 
             concept_replacements_reversed, 
             concept_map_concepts = "CONCEPT_CD", 
             concept_map_units = "UNITS_CD") %>% 
  dplyr::mutate(nval_num = signif(nval_num, 9)) %>% 
  dplyr::arrange(concept) %>% 
  dplyr::mutate(dplyr::across(.cols = dplyr::everything(), .fns = as.character)) %>% 
  replace(is.na(.), "<null>") %>% 
  dplyr::filter(nval_num != "<null>" | tval_char != "<null>")
cat("recoverSummarizeR::process_df() completed.\n")

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
rm(ecg_stat_summarize,
   dataset,
   vars,
   df,
   participants_to_exclude,
   excluded_concepts,
   approved_concepts_summarized,
   df_melted_filtered,
   df_summarized,
   output_concepts,
   curr_hk_participants,
   prev_hk_participants,
   hk_participants)
