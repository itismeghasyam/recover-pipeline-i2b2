library(dplyr)

vars <- 
  selected_vars %>% 
  filter(grepl("activitylogs", Export, ignore.case = TRUE)) %>% 
  pull(Variable) %>% 
  tolower()

df <- 
  df_list$fitbitactivitylogs %>%
  select(all_of(vars))

excluded_concepts <- diff_concepts(df_list, concept_replacements, concept_map, concept_filter_col)