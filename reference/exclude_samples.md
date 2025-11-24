# Exclude samples with X missingness

This function excludes samples from a dataset (`df`) based on a
specified threshold of missing data percentage in a meta data file.

## Usage

``` r
exclude_samples(df, missing_pct_sample = NULL)
```

## Arguments

- df:

  A data frame of feature data only

- missing_pct_sample:

  Numeric, the threshold (between 0 and 1) for sample missing data
  exclusion (e.g., 0.2 for 20%).

## Value

A list containing:

- df:

  The filtered data frame with excluded samples removed.

- excluded_samples:

  A vector of the names of samples that were excluded.
