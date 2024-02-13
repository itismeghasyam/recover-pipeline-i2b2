library(synapser)
library(recoverSummarizeR)
library(magrittr)

config::get(
  file = "config/config.yml", 
  config = "prod"
) %>% 
  list2env(envir = .GlobalEnv)

synLogin()

concept_map <- syn_file_to_df(ontologyFileID, "concept_cd")
selected_vars <- syn_file_to_df(selectedVarsFileID)

dataset_name_filter <- selected_vars %>% dplyr::pull(Export) %>% unique()

df_list_original <- 
  syn_parquet_dataset_to_dflist(
    parquetDirID, 
    method, 
    s3bucket, 
    s3basekey, 
    downloadLocation, 
    dataset_name_filter,
    deleteExistingDir = FALSE
  )

df_list_unified_tmp <- 
  unify_dfs(df_list_original) %>% 
  lapply(function(x) {
    names(x) <- tolower(names(x))
    return(x)})

df_list <- 
  df_list_unified_tmp %>% 
  lapply(function(df) {
    names(df) <- gsub("value", "value_original", names(df))
    return(df)
  })

concept_replacements_reversed <- vec_reverse(concept_replacements)
