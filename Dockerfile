FROM rocker/tidyverse

RUN apt-get update -y && \
    apt-get upgrade -y && \
    apt-get install -y git python3 python3-pip python3-venv curl libssl-dev libcurl4-openssl-dev

RUN python3 -m pip install --upgrade pip
RUN pip install synapseclient

RUN R -e 'install.packages("synapser", repos = c("http://ran.synapse.org", "http://cran.fhcrc.org"))'
RUN R -e 'install.packages("devtools")'
RUN R -e 'devtools::install_github("Sage-Bionetworks/recoverSummarizeR")'

RUN curl -o /root/run-pipeline.R https://raw.githubusercontent.com/Sage-Bionetworks/recover-pipeline-i2b2/main/run-pipeline.R

WORKDIR /root

CMD Rscript run-pipeline.R
