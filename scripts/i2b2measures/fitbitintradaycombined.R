library(dplyr)

df_list_melted_filtered$fitbitintradaycombined <- 
  df_list_melted_filtered$fitbitintradaycombined %>% 
  dplyr::filter(value>=0)