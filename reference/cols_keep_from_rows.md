# Select target columns to keep based on missingness

Computes per-column NA proportions on the provided non-QC rows of the
dataset and returns the subset of target columns that meet the
threshold.

## Usage

``` r
cols_keep_from_rows(df, rows_idx_non_qc, target_cols, col_thresh)
```

## Arguments

- df:

  A data.frame.

- rows_idx_non_qc:

  Integer indices of rows to include in the calculation (should exclude
  QC rows).

- target_cols:

  Character vector of target column names.

- col_thresh:

  Numeric. Maximum allowed proportion of missing values per column.

## Value

Character vector of target column names to keep.
