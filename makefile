FETCH_SCRIPT_DIR = scripts/fetch_data
PROCESS_SCRIPT_DIR = scripts/process_data

all: fetch_data process_data

fetch_data: Rscript $(FETCH_SCRIPT_DIR)/fetch_data.R

process_data: 
	Rscript $(PROCESS_SCRIPT_DIR)/fitbitactivitylogs.R
