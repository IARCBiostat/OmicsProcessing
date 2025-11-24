# Calculate Intraclass Correlation Coefficient (ICC) parallel

This function calculates the Intraclass Correlation Coefficient (ICC)
for a set of samples and quality controls using a linear mixed-effects
model. From: https://pmc.ncbi.nlm.nih.gov/articles/PMC6570933/ Based on:
https://github.com/courtneyschiffman/Metabolomics-Filtering/blob/master/ICC.R

## Usage

``` r
calculate_ICC_parallel(df, id_samples, id_qc)
```

## Arguments

- df:

  A data frame where rows = features and columns = samples column names
  should be sample IDs; row names should be feature IDs

- id_samples:

  A vector of column names or indices representing the sample columns in
  `df`.

- id_qc:

  A vector of column names or indices representing the quality control
  columns in `df`.

## Value

A named numeric vector of ICC values for each row in `df`.
