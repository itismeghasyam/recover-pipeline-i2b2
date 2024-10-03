# Generate a TSV manifest file of intermediate summaries files (intermediate output concepts files)
synapserutils::generate_sync_manifest(
  directory_path = outputConceptsDir, 
  parent_id = tempOutputConceptSynID, 
  manifest_path = "temp-output-concepts-manifest.tsv"
)

# Exclude the final output_concepts.csv file from the manifest and write the
# modified manifest file to a CSV file
read.delim("temp-output-concepts-manifest.tsv") %>% 
  dplyr::filter(!grepl("output_concepts.csv", path)) %>% 
  write.csv(file = "temp-output-concepts-manifest.csv", row.names = FALSE, quote = FALSE)

login <- synapser::synLogin()

# Store each file listed in the CSV manifest file at its specified parent Synapse ID
synapserutils::syncToSynapse(manifestFile = "temp-output-concepts-manifest.csv")

# synclient <- reticulate::import("synapseclient")
# syn_temp <- synclient$Synapse()
# syn <- syn_temp$login()
# 
# synutils <- reticulate::import("synapseutils")
# synutils$syncToSynapse(syn = syn, manifestFile = "temp-output-concepts-manifest.csv")
