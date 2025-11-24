# Select rows to keep based on missingness

Computes per-row NA proportions across the provided target columns. QC
rows (`is_qc_full == TRUE`) are always retained.

## Usage

``` r
rows_keep_from_cols(df, cols_target, row_thresh, is_qc_full)
```

## Arguments

- df:

  A data.frame.

- cols_target:

  Character vector of target column names (subset of `names(df)`).

- row_thresh:

  Numeric. Maximum allowed proportion of missing values per row.

- is_qc_full:

  Logical vector of length `nrow(df)`; TRUE for QC rows.

## Value

Integer indices of rows to keep.
