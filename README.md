## recover-pipeline-i2b2

This repo hosts the code for the egress pipeline used by the MHDR (SageBionetworks) for data enrichment and i2b2 summarization of data.

## Requirements

-   R >= 4.0.0
-   Docker
-   Synapse account with relevant access permissions
-   Synapse authentication token

A Synapse authentication token is required for use of the Synapse APIs (e.g. the `synapser` package for R). For help with Synapse, Synapse APIs, Synapse authentication tokens, etc., please refer to the [Synapse documentation](https://help.synapse.org/docs/).

## Usage

There are two methods to run this pipeline: 1) **Docker container** or 2) **Manual Job**. Please refer to the [Docker Container](#docker-container) and [Manual Job](#manual-job) sections for their respective usage instructions.

### Docker Container

For the Docker method, there is a pre-published docker image available at [Packages](https://github.com/orgs/pranavanba/packages/container/package/recover-pipeline-i2b2).

The primary purpose of using the Docker method is that the docker image published from this repo contains instructions to:

1. Create a computing environment with the dependencies needed by the machine running the pipeline
2. Install the packages needed in order to run the pipeline
3. Run a script containing the instructions for the pipeline

#### Use the pre-built Docker image

1.  Add your Synapse personal access token to the environment

```Shell
# Option 1: For only the current shell session:
export SYNAPSE_AUTH_TOKEN=<your-token>

# Option 2: For all future shell sessions (modify your shell profile)
# Open the profile file
nano ~/.bash_profile

# Append the following
SYNAPSE_AUTH_TOKEN=<your-token>
export SYNAPSE_AUTH_TOKEN

# Save the file
source ~/.bash_profile
```

2.  Pull the docker image

```Shell
docker pull ghcr.io/pranavanba/recover-pipeline-i2b2:main
```

3.  Run the docker container

```Shell
docker run \
  --name <container-name> \
  -e SYNAPSE_AUTH_TOKEN=$SYNAPSE_AUTH_TOKEN \
  -e ONTOLOGY_FILE_ID=<synapseID> \
  -e PARQUET_DIR_ID=<synapseID> \
  -e DATASET_NAME_FILTER=<string> \
  -e CONCEPT_REPLACEMENTS=<named-vector-in-parentheses> \
  -e CONCEPT_FILTER_COL=<concept-map-column-name> \
  -e SYN_FOLDER_ID=<synapseID> \
  ghcr.io/pranavanba/recover-pipeline-i2b2:main
```

For an explanation of the various environment variables required in the `docker run` command, please see [Environment Variables](#environment-variables).

#### Build the Docker image yourself

1.  Add your Synapse personal access token to the environment

```Shell
# Option 1: For only the current shell session:
export SYNAPSE_AUTH_TOKEN=<your-token>

# Option 2: For all future shell sessions (modify your shell profile)
# Open the profile file
nano ~/.bash_profile

# Append the following
SYNAPSE_AUTH_TOKEN=<your-token>
export SYNAPSE_AUTH_TOKEN

# Save the file
source ~/.bash_profile
```

2. Clone this repo

```Shell
git clone https://github.com/pranavanba/recover-pipeline-i2b2.git
```

4.  Build the docker image

```Shell
# Option 1: From the directory containing the Dockerfile
cd /path/to/Dockerfile
docker build <optional-arguments> -t <image-name> .

# Option 2: From anywhere
docker build <optional-arguments> -t <image-name> -f <path-to-Dockerfile> .
```

4.  Run the docker container

```Shell
docker run \
  --name <docker-container-name> \
  -e SYNAPSE_AUTH_TOKEN=$SYNAPSE_AUTH_TOKEN \
  -e ONTOLOGY_FILE_ID=<synapseID> \
  -e PARQUET_DIR_ID=<synapseID> \
  -e DATASET_NAME_FILTER=<string> \
  -e CONCEPT_REPLACEMENTS=<named-vector-in-parentheses> \
  -e CONCEPT_FILTER_COL=<concept-map-column-name> \
  -e SYN_FOLDER_ID=<synapseID> \
  <docker-image-name>
```

For an explanation of the various environment variables required in the `docker run` command, please see [Environment Variables](#environment-variables).

### Manual Job

If you would like to run the pipeline manually, or pass just a single script to a job scheduler, please follow the instructions in this section.

1. Add your Synapse personal access token to the environment

```Shell
# Option 1: For only the current shell session:
export SYNAPSE_AUTH_TOKEN=<your-token>

# Option 2: For all future shell sessions (modify your shell profile)
# Open the profile file
nano ~/.bash_profile

# Append the following
SYNAPSE_AUTH_TOKEN=<your-token>
export SYNAPSE_AUTH_TOKEN

# Save the file
source ~/.bash_profile
```

2. Clone this repo or get just the [run-pipeline.R](run-pipeline.R) file

```Shell
git clone https://github.com/pranavanba/recover-pipeline-i2b2.git
```

2. Modify the variables and parameters in [run-pipeline.R](run-pipeline.R). If you do not modify how the values of the variables in [run-pipeline.R](run-pipeline.R) are read in, then you will need to set the values of those variables as environment variables. If you want to set the values of those variables in an R session, then either use the `Sys.setenv()` function or modify the file itself to assign values to variables the normal way in R, e.g. `var <- val`.
4. Run [run-pipeline.R](run-pipeline.R)

### Environment Variables

The environment variables passed to `docker run ...` are the input arguments of `recoversummarizer::summarize_pipeline()`, and as such must be provided in order to use the docker method. Please refer to the [recoverSummarizeR](https://github.com/Sage-Bionetworks/recoverSummarizeR) R package for more information on the `recoverSummarizeR` package and its functions.

Variable | Definition | Example
---|---|---
| `ONTOLOGY_FILE_ID` | A Synapse ID for a CSV file stored in Synapse. For RECOVER, this file is the i2b2 concepts map. | syn12345678
| `PARQUET_DIR_ID` | A Synapse ID for a folder entity in Synapse where the data is stored. For RECOVER, this would be the folder housing the post-ETL parquet data. | syn12345678
| `DATASET_NAME_FILTER`  | A string found in the names of the files to be read. This acts like a filter to include only the files that contain the string in their names. | fitbit
| `CONCEPT_REPLACEMENTS` | A named vector of strings and their replacements. The names must be valid values of the `concept_filter_col` column of the `concept_map` data frame. For RECOVER, `concept_map` is the ontology file data frame. | "c('mins' = 'minutes', 'avghr' = 'averageheartrate', 'spo2' = 'spo2\_', 'hrv' = 'hrv_dailyrmssd', 'restinghr' = 'restingheartrate', 'sleepbrth' = 'sleepsummarybreath')" <br><br> *Must surround `c(…)` in parentheses (as indicated above) in `docker run …`*
| `CONCEPT_FILTER_COL` | The column of the `concept_map` data frame that contains "approved concepts" (column names of dataset data frames that are not to be excluded). For RECOVER, `concept_map` is the ontology file data frame. | concept_cd
| `SYN_FOLDER_ID` | A Synapse ID for a folder entity in Synapse where you want to store a file. | syn12345678
