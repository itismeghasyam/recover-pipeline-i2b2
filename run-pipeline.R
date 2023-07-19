require(synapser)
require(devtools)
library(recoverSummarizeR)

synapser::synLogin(authToken=Sys.getenv("SYNAPSE_AUTH_TOKEN"))

ontologyFileID <- Sys.getenv("ONTOLOGY_FILE_ID")
parquetDirID <- Sys.getenv("PARQUET_DIR_ID")
dataset_name_filter <- Sys.getenv("DATASET_NAME_FILTER")
concept_replacements <- eval(parse(text=Sys.getenv("CONCEPT_REPLACEMENTS")))
concept_filter_col <- Sys.getenv("CONCEPT_FILTER_COL")
synFolderID <- Sys.getenv("SYN_FOLDER_ID")

recoverSummarizeR::summarize_pipeline(ontologyFileID, parquetDirID, dataset_name_filter, concept_replacements, concept_filter_col, synFolderID)
