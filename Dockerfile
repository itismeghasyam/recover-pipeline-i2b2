FROM ghcr.io/pranavanba/rocker-sage:main

RUN R -e 'install.packages("devtools")'
RUN R -e 'devtools::install_github("Sage-Bionetworks/recoverSummarizeR")'

RUN apt-get update && apt-get install -y git

WORKDIR /root

RUN git clone https://github.com/Sage-Bionetworks/recover-pipeline-i2b2.git

WORKDIR /root/recover-pipeline-i2b2

CMD Rscript pipeline/run-pipeline.R
