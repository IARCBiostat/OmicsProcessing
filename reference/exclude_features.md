# Exclude features with X misingness

This function excludes features from a dataset (`df`) based on a
specified threshold of missing data percentage in a meta data file.

## Usage

``` r
exclude_features(df, missing_pct_feature = NULL)
```

## Arguments

- df:

  A data frame of feature data only

- missing_pct_feature:

  Numeric, the threshold (between 0 and 1) for feature missing data
  exclusion (e.g., 0.2 for 20%).

## Value

A list containing:

- df:

  The filtered data frame with excluded features removed.

- excluded_features:

  A vector of the names of features that were excluded.
