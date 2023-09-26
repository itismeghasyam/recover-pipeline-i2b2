FROM ghcr.io/pranavanba/rocker-sage:main

RUN R -e 'install.packages("devtools")'
RUN R -e 'devtools::install_github("Sage-Bionetworks/recoverSummarizeR")'

RUN curl -o /root/run-pipeline.R https://raw.githubusercontent.com/Sage-Bionetworks/recover-pipeline-i2b2/main/run-pipeline.R

WORKDIR /root

CMD Rscript run-pipeline.R
