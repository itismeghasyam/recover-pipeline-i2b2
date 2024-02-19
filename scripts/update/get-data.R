library(synapser)
library(recoverSummarizeR)
library(dplyr)

config::get(
  file = "config/config.yml", 
  config = "prod"
) %>% 
  list2env(envir = .GlobalEnv)

synLogin()

concept_map <- 
  syn_file_to_df(ontologyFileID, "CONCEPT_CD") %>% 
  filter(CONCEPT_CD!="<null>")

selected_vars <- 
  syn_file_to_df(selectedVarsFileID) %>% 
  mutate(across(everything(), tolower))

dataset_name_filter <- selected_vars %>% dplyr::pull(Export) %>% unique()

# Sync S3 bucket to local
token <- synapser::synGetStsStorageToken(
  entity = parquetDirID,
  permission = "read_only",
  output_format = "json")

if (s3bucket==token$bucket && s3basekey==token$baseKey) {
  base_s3_uri <- paste0('s3://', token$bucket, '/', token$baseKey)
} else {
  base_s3_uri <- paste0('s3://', s3bucket, '/', s3basekey)
}

Sys.setenv('AWS_ACCESS_KEY_ID'=token$accessKeyId,
           'AWS_SECRET_ACCESS_KEY'=token$secretAccessKey,
           'AWS_SESSION_TOKEN'=token$sessionToken)

if (deleteExistingDir==TRUE) {
  unlink(downloadLocation, recursive = T, force = T)
}

inclusions <- paste0("--include \"*",dataset_name_filter,"*\"", collapse = " ")
sync_cmd <- glue::glue('aws s3 sync {base_s3_uri} {downloadLocation} --exclude "*" {inclusions}')
system(sync_cmd)
rm(sync_cmd)

concept_replacements_reversed <- vec_reverse(concept_replacements)

if (!dir.exists(outputConceptsDir)) {
  dir.create(outputConceptsDir)
}
