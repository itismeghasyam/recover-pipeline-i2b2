## recover-pipeline-i2b2

[![Build and publish a Docker image](https://github.com/Sage-Bionetworks/recover-parquet-external/actions/workflows/docker-build.yml/badge.svg?branch=main)](https://github.com/Sage-Bionetworks/recover-parquet-external/actions/workflows/docker-build.yml)

This repo hosts the code for the egress pipeline used by the Digital Health Data Repository (Sage Bionetworks) for data enrichment and summarization for i2b2.

## Requirements

-   R >= 4.0.0
-   Docker
-   Synapse account with relevant access permissions
-   Synapse authentication token

A Synapse authentication token is required for use of the Synapse APIs (e.g. the `synapser` package for R). For help with Synapse, Synapse APIs, Synapse authentication tokens, etc., please refer to the [Synapse documentation](https://help.synapse.org/docs/).

## Usage

There are two methods to run this pipeline: 
1) [**Docker container**](#docker-container), or
2) [**Manual Job**](#manual-job)

### Set Synapse Personal Access Token

Regardless of which method you use, you need to set your Synapse Personal Access Token somewhere in your environment. See the examples below

1.  Option 1: For only the current shell session:

```Shell
export SYNAPSE_AUTH_TOKEN=<your-token>
```

2. Option 2: For all future shell sessions (modify your shell profile)

```Shell
# Open the profile file
nano ~/.bash_profile

# Append the following
SYNAPSE_AUTH_TOKEN=<your-token>
export SYNAPSE_AUTH_TOKEN

# Save the file
source ~/.bash_profile
```

### Docker Container

For the Docker method, there is a pre-published docker image available [here](https://github.com/orgs/Sage-Bionetworks/packages/container/package/recover-pipeline-i2b2).

The primary purpose of using the Docker method is that the docker image published from this repo contains instructions to:

1. Create a computing environment with the dependencies needed by the machine running the pipeline
2. Install the packages needed in order to run the pipeline
3. Run a script containing the instructions for the pipeline

If you do not want to use the pre-built Docker image, skip to the next section ([**Build the Docker image yourself**](#build-the-docker-image-yourself))

#### Use the pre-built Docker image

1.  Pull the docker image

```Shell
docker pull ghcr.io/sage-bionetworks/recover-pipeline-i2b2:main
```

2.  Run the docker container

```Shell
docker run \
  --name container-name \
  -e SYNAPSE_AUTH_TOKEN=$SYNAPSE_AUTH_TOKEN \
  ghcr.io/Sage-Bionetworks/recover-pipeline-i2b2:main
```

For an explanation of the various config parameters used in the pipeline, please see [Config Parameters](#config-parameters).

#### Build the Docker image yourself

1. Clone this repo

```Shell
git clone https://github.com/Sage-Bionetworks/recover-pipeline-i2b2.git
```

2.  Build the docker image

```Shell
# Option 1: From the directory containing the Dockerfile
cd /path/to/Dockerfile/
docker build <optional-arguments> -t image-name .

# Option 2: From anywhere
docker build <optional-arguments> -t image-name -f /path/to/Dockerfile/ .
```

3.  Run the docker container

```Shell
docker run \
  --name container-name \
  -e SYNAPSE_AUTH_TOKEN=$SYNAPSE_AUTH_TOKEN \
  image-name
```

For an explanation of the various config parameters used in the pipeline, please see [Config Parameters](#config-parameters).

### Manual Job

If you would like to run the pipeline manually, please follow the instructions in this section.

1. Clone this repo

```Shell
git clone https://github.com/Sage-Bionetworks/recover-pipeline-i2b2.git
```

2. Modify the parameters in the [config](config/config.yml) as needed

3. Run [run-pipeline.R](pipeline/run-pipeline.R)

### Config Parameters

This table contains all of the parameters needed to run the pipeline, along with their definitions and examples.

Parameter | Definition | Example
---|---|---
| `ontologyFileID` | A Synapse ID for the the i2b2 concepts map ontology file stored in Synapse. | syn12345678
| `parquetDirID` | A Synapse ID for a folder entity in Synapse where the input data is stored. This should be the folder housing the post-ETL parquet data. | syn12345678
| `deleteExistingDir` | Boolean representing if you want the `downloadLocation` folder to be removed before syncing input data there. Setting this to `TRUE` ensures that there is a new, clean location to sync the input data to and keep it isolated from input data that is old, unwanted, modified, etc. | `TRUE`
| `concept_replacements` | A named vector of strings and their replacements. The names must be valid values of the `concept_filter_col` column of the `concept_map` data frame. For RECOVER, `concept_map` is the ontology file data frame. | R Example<br>c('mins' = 'minutes', 'avghr' = 'averageheartrate', 'spo2' = 'spo2\_', 'hrv' = 'hrv_dailyrmssd', 'restinghr' = 'restingheartrate', 'sleepbrth' = 'sleepsummarybreath') | concept_cd
| `synFolderID` | A Synapse ID for a folder entity in Synapse where you want to store the final output files. | syn12345678
| `s3bucket` | The name of the S3 bucket containing input data | recover-bucket
| `s3basekey` | The base key of the S3 bucket containing input data. | main/archive/2024-.../
| `downloadLocation` | The location to sync input files to. | ./parquet
| `selectedVarsFileID` | A Synapse ID for the CSV file listing which datasets and variables have been selected for use in this pipeline | syn12345678
| `outputConceptsDir` | The location to save intermediate and final i2b2 summary files to | ./output-concepts

