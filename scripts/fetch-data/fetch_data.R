library(synapser)
library(recoverutils)
library(dplyr)

cat("Fetching data\n")

synLogin()

# Get input files from synapse
concept_map <- 
  recoverutils::syn_file_to_df(ontologyFileID, "CONCEPT_CD") %>% 
  filter(CONCEPT_CD!="<null>")

selected_vars <- 
  recoverutils::syn_file_to_df(selectedVarsFileID) %>% 
  mutate(Lower_Bound = suppressWarnings(as.numeric(Lower_Bound)),
         Upper_Bound = suppressWarnings(as.numeric(Upper_Bound)))

# Get list of which datasets to use
dataset_name_filter <- 
  selected_vars %>% 
  dplyr::pull(Export) %>% 
  unique()

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

# Only sync the bucket folders containing the datasets we need
inclusions <- paste0("--include \"*",dataset_name_filter,"*\"", collapse = " ")
sync_cmd <- glue::glue('aws s3 sync {base_s3_uri} {downloadLocation} --exclude "*" {inclusions}')
system(sync_cmd)
rm(sync_cmd)

# For use in process-data steps
concept_replacements_reversed <- recoverutils::vec_reverse(concept_replacements)

if (!dir.exists(outputConceptsDir)) {
  dir.create(outputConceptsDir)
}

cat("Finished fetching data\n\n")
