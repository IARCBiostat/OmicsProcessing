# Filter a data.frame by missingness in rows and columns

Applies different strategies to filter rows and columns of a dataset
based on missingness thresholds. Supports iterative refinement until
stable.

## Usage

``` r
filter_by_missingness(
  df,
  row_thresh = 0.5,
  col_thresh = 0.5,
  target_cols = NULL,
  is_qc = NULL,
  filter_order = c("iterative", "simultaneous", "col_then_row", "row_then_col"),
  max_iter = 10
)
```

## Arguments

- df:

  A data.frame.

- row_thresh:

  Proportion of missing values allowed per (non-QC) row in target
  columns.

- col_thresh:

  Proportion of missing values allowed per column in target columns.

- target_cols:

  Character vector of target columns. If NULL, resolved by
  [`resolve_target_cols()`](https://iarcbiostat.github.io/OmicsProcessing/reference/resolve_target_cols.md).

- is_qc:

  Logical vector the same length as `nrow(df)` indicating QC rows
  (always retained).

- filter_order:

  One of `"iterative"` (default), `"col_then_row"`, `"row_then_col"`, or
  `"simultaneous"`.

  - `"iterative"`: alternately filter rows and columns until stable or
    `max_iter` is reached.

  - `"col_then_row"`: filter columns first, then rows.

  - `"row_then_col"`: filter rows first, then columns.

  - `"simultaneous"`: filter rows and columns independently, then
    intersect results.

- max_iter:

  Maximum number of iterations when `filter_order="iterative"`. Default
  10.

## Value

Filtered data.frame with a subset of rows and/or columns.

## Examples
