# Functions

#' Create i2b2 summaries
#'
#' `sleeplogs_stat_summarize()` summarizes the sleep data in a data frame on 
#' specific time scales (all data and weekly) for the following statistics: 
#' 5th/95th percentiles, mean, median, variance, and number of records.
#'
#' @param df A data frame with `participantidentifier`, `concept`, `value`, and 
#' some combination of `startdate`, `enddate`, `date`, or `datetime` columns.
#'
#' @return A data frame that differs in size from `df`. The original `value` 
#' column is dropped and replaced by a new `value` column containing the values 
#' of each summary statistic computed for each group of data (group by 
#' `participantidentifier` and `concept` for all data, and 
#' `participantidentifier`, `concept`, `year`, `week` for weekly summaries).
sleeplogs_stat_summarize <- function(df) {
  cat("Running sleeplogs_stat_summarize()...\n")
  
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
                     mean = ifelse(
                       concept %in% c("sleepstarttime", "sleependtime", "midsleep"),
                       psych::circadian.mean(as.numeric(value)),
                       mean(as.numeric(value), na.rm = T)
                     ),
                     median = stats::median(as.numeric(value), na.rm = T),
                     variance = ifelse(
                       concept %in% c("sleepstarttime", "sleependtime", "midsleep"),
                       as.numeric(psych::circadian.sd(as.numeric(value))$Rvar),
                       stats::var(as.numeric(value), na.rm = T)
                     ),
                     `5pct` = stats::quantile(as.numeric(value), 0.05, na.rm = T),
                     `95pct` = stats::quantile(as.numeric(value), 0.95, na.rm = T),
                     numrecords = dplyr::n()) %>%
      dplyr::ungroup() %>% 
      tidyr::pivot_longer(cols = c(mean, median, variance, `5pct`, `95pct`, numrecords),
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
      dplyr::reframe(`5pct` = stats::quantile(as.numeric(value), 0.05, na.rm = T), 
                     `95pct` = stats::quantile(as.numeric(value), 0.95, na.rm = T), 
                     mean = ifelse(
                       concept %in% c("sleepstarttime", "sleependtime", "midsleep"),
                       psych::circadian.mean(as.numeric(value)),
                       mean(as.numeric(value), na.rm = T)
                     ),
                     median = stats::median(as.numeric(value), na.rm = T), 
                     variance = ifelse(
                       concept %in% c("sleepstarttime", "sleependtime", "midsleep"),
                       as.numeric(psych::circadian.sd(as.numeric(value))$Rvar),
                       stats::var(as.numeric(value), na.rm = T)
                     ),
                     numrecords = dplyr::n()) %>% 
      dplyr::ungroup() %>% 
      tidyr::pivot_longer(cols = c(mean, median, variance, `5pct`, `95pct`, numrecords), 
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

dataset <- "fitbitsleeplogs"

cat(glue::glue("Transforming data for {dataset}"),"\n")

# Get variables for this dataset
vars <- 
  selected_vars %>% 
  filter(grepl(paste0(dataset, "$"), Export, ignore.case = TRUE, perl = TRUE)) %>% 
  pull(Variable)

# Load the desired subset of this dataset in memory and do some feature engineering for derived variables
df <- 
  arrow::open_dataset(file.path(downloadLocation, glue::glue("dataset_{dataset}"))) %>% 
  select(all_of(c(vars, "LogId"))) %>% 
  collect() %>% 
  distinct() %>% 
  mutate(Duration = as.numeric(Duration),
         Efficiency = as.numeric(Efficiency),
         IsMainSleep = as.logical(IsMainSleep),
         SleepStartTime = lubridate::as_datetime(ifelse(IsMainSleep==TRUE, StartDate, NA)),
         SleepEndTime = lubridate::as_datetime(ifelse(IsMainSleep==TRUE, EndDate, NA)),
         MidSleep = format((lubridate::as_datetime(SleepStartTime) + ((Duration/1000)/2)), format = "%H:%M:%S"),
         SleepStartTime = 
           ((format(SleepStartTime, format = "%H:%M:%S") %>% lubridate::hms()) / lubridate::hours(24))*24,
         SleepEndTime = 
           ((format(SleepEndTime, format = "%H:%M:%S") %>% lubridate::hms()) / lubridate::hours(24))*24,
         MidSleep = 24*lubridate::hms(MidSleep)/lubridate::hours(24))

# Do some feature engineering to create the NumEpisodes derived variable and alltime/weekly summary statistics
numepisodes_df_alltime <- 
  df %>% 
  mutate(startdate2 = lubridate::as_date(StartDate),
         NumEpisodes = ifelse(!is.na(LogId), 1, NA)) %>% 
  group_by(ParticipantIdentifier, startdate2) %>% 
  select(ParticipantIdentifier, startdate2, NumEpisodes) %>% 
  summarise(numeps = sum(NumEpisodes), .groups = "keep") %>% 
  ungroup() %>% 
  group_by(ParticipantIdentifier) %>% 
  summarise(startdate = min(startdate2),
            mean = mean(numeps, na.rm = T), 
            median = median(numeps, na.rm = T), 
            variance = var(numeps, na.rm = T), 
            `5pct` = stats::quantile(as.numeric(numeps), 0.05, na.rm = T), 
            `95pct` = stats::quantile(as.numeric(numeps), 0.95, na.rm = T), 
            numrecords = dplyr::n(),
            .groups = "keep") %>% 
  ungroup() %>% 
  left_join(y = 
              df %>% 
              select(ParticipantIdentifier, EndDate) %>% 
              group_by(ParticipantIdentifier) %>% 
              summarise(enddate = max(lubridate::as_date(EndDate)), .groups = "keep") %>% 
              ungroup(), 
            by = "ParticipantIdentifier") %>% 
  select(ParticipantIdentifier, startdate, enddate, tidyselect::everything())

numepisodes_df_weekly <- 
  df %>% 
  mutate(startdate2 = lubridate::as_date(StartDate),
         startdate3 = lubridate::floor_date(startdate2, unit = "week", week_start = 7),
         enddate2 = lubridate::as_date(EndDate),
         enddate3 = startdate3 + lubridate::days(6),
         NumEpisodes = ifelse(!is.na(LogId), 1, NA)) %>% 
  select(ParticipantIdentifier, startdate3, enddate3, NumEpisodes) %>% 
  group_by(ParticipantIdentifier, startdate3, enddate3) %>% 
  summarise(mean = mean(NumEpisodes, na.rm = T), 
            median = median(NumEpisodes, na.rm = T), 
            variance = var(NumEpisodes, na.rm = T), 
            `5pct` = stats::quantile(as.numeric(NumEpisodes), 0.05, na.rm = T), 
            `95pct` = stats::quantile(as.numeric(NumEpisodes), 0.95, na.rm = T), 
            numrecords = dplyr::n(),
            .groups = "keep") %>% 
  ungroup() %>% 
  rename(startdate = startdate3, enddate = enddate3) %>% 
  select(ParticipantIdentifier, startdate, enddate, tidyselect::everything())

# Use the sleeplogs_sleeplogdetails dataset to create the NumAwakenings derived variable
sleeplogsdetails_vars <- 
  selected_vars %>% 
  filter(grepl("sleeplogdetails", Export, ignore.case = TRUE)) %>% 
  pull(Variable)

sleeplogsdetails_df <- 
  arrow::open_dataset(file.path(downloadLocation, "dataset_fitbitsleeplogs_sleeplogdetails")) %>% 
  select(all_of(sleeplogsdetails_vars)) %>% 
  collect() %>% 
  left_join(y = (df %>% select(LogId, IsMainSleep)), by = "LogId")

numawakenings_logid_filtered <- 
  sleeplogsdetails_df %>% 
  filter(IsMainSleep==TRUE) %>% 
  group_by(LogId) %>% 
  summarise(NumAwakenings = sum(Value %in% c("wake", "awake") &
                                  !(row_number() == 1 & Value %in% c("wake", "awake")) &
                                  !(row_number() == n() & Value %in% c("wake", "awake"))), 
            .groups = "keep") %>% 
  ungroup()

# Merge the original df with the numawakenings df to create a united df
df_joined <- left_join(x = df, y = numawakenings_logid_filtered, by = "LogId")

colnames(df_joined) <- tolower(colnames(df_joined))

colnames(numepisodes_df_alltime) <- tolower(colnames(numepisodes_df_alltime))

colnames(numepisodes_df_weekly) <- tolower(colnames(numepisodes_df_weekly))

# Create lists for ID variables and i2b2 concept variables
excluded_concepts <- c("participantidentifier", 
                       "startdate", 
                       "enddate", 
                       "duration", 
                       "ismainsleep",
                       "logid")

approved_concepts_summarized <- 
  setdiff(
    colnames(df_joined),
    excluded_concepts
  )

df_joined[approved_concepts_summarized] <- lapply(df_joined[approved_concepts_summarized], as.numeric)

# Get QA/QC ranges for variables and exclude values outside the ranges
bounds <- 
  selected_vars %>% 
  filter(grepl(dataset, Export, ignore.case = TRUE),
         tolower(Variable) %in% approved_concepts_summarized) %>% 
  select(Variable, Lower_Bound, Upper_Bound) %>% 
  mutate(Variable = tolower(Variable)) %>% 
  filter(!is.na(Lower_Bound) & !is.na(Upper_Bound))

df_filtered <- df_joined
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

# Pivot data frames from long to wide
df_melted_filtered <- 
  df_filtered %>% 
  recoverSummarizeR::melt_df(excluded_concepts = excluded_concepts) %>% 
  select(if("participantidentifier" %in% colnames(.)) "participantidentifier",
         dplyr::matches("(?<!_)date(?!_)", perl = T),
         if("concept" %in% colnames(.)) "concept",
         if("value" %in% colnames(.)) "value") %>% 
  tidyr::drop_na("value") %>% 
  mutate(value = as.numeric(value))

numepisodes_df_melted_filtered_alltime <- 
  numepisodes_df_alltime %>% 
  tidyr::pivot_longer(cols = !c(participantidentifier, startdate, enddate), 
                      names_to = "stat",
                      values_to = "value") %>% 
  select(if("participantidentifier" %in% colnames(.)) "participantidentifier",
         dplyr::matches("(?<!_)date(?!_)", perl = T),
         if("stat" %in% colnames(.)) "stat",
         if("value" %in% colnames(.)) "value") %>% 
  tidyr::drop_na("value") %>%
  mutate(value = as.numeric(value))

numepisodes_df_melted_filtered_weekly <- 
  numepisodes_df_weekly %>% 
  tidyr::pivot_longer(cols = !c(participantidentifier, startdate, enddate), 
                      names_to = "stat",
                      values_to = "value") %>% 
  select(if("participantidentifier" %in% colnames(.)) "participantidentifier",
         dplyr::matches("(?<!_)date(?!_)", perl = T),
         if("stat" %in% colnames(.)) "stat",
         if("value" %in% colnames(.)) "value") %>% 
  tidyr::drop_na("value") %>%
  mutate(value = as.numeric(value))

cat("recoverSummarizeR::melt_df() completed.\n")

# Generate i2b2 summaries
df_summarized <- 
  df_melted_filtered %>% 
  select(all_of(c("participantidentifier", "startdate", "enddate", "concept", "value"))) %>% 
  sleeplogs_stat_summarize() %>% 
  distinct()

numepisodes_df_summarized_alltime <- 
  numepisodes_df_melted_filtered_alltime %>% 
  select(participantidentifier, startdate, enddate, stat, value) %>% 
  mutate(concept = paste0("mhp:summary:alltime:", stat, ":numepisodes")) %>%
  select(participantidentifier, startdate, enddate, concept, value) %>%
  distinct()

numepisodes_df_summarized_weekly <- 
  numepisodes_df_melted_filtered_weekly %>% 
  select(participantidentifier, startdate, enddate, stat, value) %>% 
  filter(startdate >= lubridate::floor_date(min(startdate), unit = "week", week_start = 7)) %>% 
  mutate(concept = paste0("mhp:summary:weekly:", stat, ":numepisodes")) %>% 
  select(participantidentifier, startdate, enddate, concept, value) %>% 
  distinct()

# Unite all i2b2 summaries data frames
final_df_summarized <- 
  dplyr::bind_rows(df_summarized, 
                   numepisodes_df_summarized_alltime, 
                   numepisodes_df_summarized_weekly) %>% 
  dplyr::distinct()

cat("sleeplogs_stat_summarize() completed.\n")

# Add i2b2 columns from concept map (ontology file) and clean the output
output_concepts <- 
  process_df(final_df_summarized, concept_map, concept_replacements_reversed, concept_map_concepts = "CONCEPT_CD", concept_map_units = "UNITS_CD") %>% 
  dplyr::mutate(nval_num = signif(nval_num, 9)) %>% 
  dplyr::arrange(concept) %>% 
  dplyr::mutate(dplyr::across(.cols = dplyr::everything(), .fns = as.character)) %>% 
  replace(is.na(.), "<null>") %>% 
  dplyr::filter(nval_num != "<null>" | tval_char != "<null>")
cat("recoverSummarizeR::process_df() completed.\n")

# Identify the participants who have output concepts derived from fitbit variables
curr_fitbit_participants <- 
  sort(unique(output_concepts$participantidentifier)) %>% 
  as.data.frame() %>% 
  dplyr::rename(participantidentifier = ".")

prev_fitbit_participants <- 
  read.csv(file.path(outputConceptsDir, "fitbit_participants.csv"))

fitbit_participants <- 
  dplyr::bind_rows(prev_fitbit_participants, 
                   curr_fitbit_participants) %>% 
  distinct()

fitbit_participants %>% 
  write.csv(file.path(outputConceptsDir, "fitbit_participants.csv"), 
            row.names = F)

# Write the output
output_concepts %>% 
  write.csv(file.path(outputConceptsDir, glue::glue("{dataset}.csv")), row.names = F)
cat(glue::glue("output_concepts written to {file.path(outputConceptsDir, paste0(dataset, '.csv'))}"),"\n")

cat(glue::glue("Finished transforming data for {dataset}"),"\n\n")

# Remove objects created here from the global environment
rm(sleeplogs_stat_summarize,
   dataset,
   vars, 
   df, 
   numepisodes_df_alltime,
   numepisodes_df_weekly,
   sleeplogsdetails_vars,
   sleeplogsdetails_df,
   numawakenings_logid_filtered,
   df_joined,
   excluded_concepts, 
   approved_concepts_summarized, 
   bounds,
   df_filtered,
   col_name,
   lower_bound,
   upper_bound,
   df_melted_filtered, 
   numepisodes_df_melted_filtered_alltime,
   numepisodes_df_melted_filtered_weekly,
   df_summarized, 
   numepisodes_df_summarized_alltime,
   numepisodes_df_summarized_weekly,
   final_df_summarized,
   output_concepts,
   curr_fitbit_participants, 
   prev_fitbit_participants, 
   fitbit_participants)
