library(dplyr)

vars <- 
  selected_vars %>% 
  filter(grepl("dailydata", Export, ignore.case = TRUE)) %>% 
  pull(Variable) %>% 
  tolower()

df <- 
  df_list$fitbitdailydata %>%
  select(all_of(vars))

