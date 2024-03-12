a <- Sys.time()
# Fetch data
source("scripts/fetch-data/fetch_data.R")

# Process data
source("scripts/process-data/fitbitactivitylogs.R")
source("scripts/process-data/fitbitdailydata.R")
source("scripts/process-data/fitbitintradaycombined.R")
source("scripts/process-data/fitbitsleeplogs.R")
source("scripts/process-data/healthkitv2samples.R")
source("scripts/process-data/healthkitv2statistics.R")
source("scripts/process-data/participant_devices.R")

# Create final output concepts
source("scripts/write-output/final-output-concepts.R")
b <- Sys.time()
b-a

# Egress
source("scripts/egress/egress.R")
c <- Sys.time()
c-b
