# diffex

A `shiny` tool to view differential expression data:

Input
- tpm.tsv: a tsv file containing samples (columns) and features (rows), e.g. output from salmon txi$abundance
- design.tsv: a tsv file containing colData for the experiment (explanatory variables for samples)
- results.tsv: a tsv file containing the results from deseq2
