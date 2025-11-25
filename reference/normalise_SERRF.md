# SERRF Normalization

Applies the SERRF normalization method to remove unwanted variation (Fan
2019, Anal Chem).

## Usage

``` r
normalise_SERRF(
  df,
  target_cols = NULL,
  is_qc = NULL,
  strata_col = NULL,
  num_screen_SERRF = 10
)
```

## Arguments

- df:

  A data frame or tibble.

- target_cols:

  Feature columns to normalize (character vector or selector passed to
  `resolve_target_cols`).

- is_qc:

  Logical vector indicating which rows are QC samples. Must be the same
  length as the number of rows in `df`.

- strata_col:

  Name of the column containing batch/strata IDs. If `NULL`, a dummy
  single-level strata is created so all samples are treated as one
  batch. When provided, the column must exist in `df` with no NA values.

- num_screen_SERRF:

  Number of correlated features to use in model fitting. Default is 10.

## Value

A normalized data frame.
