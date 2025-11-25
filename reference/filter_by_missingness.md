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

``` r
# Example dataset with deliberate missingness
df <- data.frame(
  a = c(NA, 1, NA, 1, NA),
  b = c(NA, NA, 2, 2, NA),
  c = c(3, NA, NA, 3, NA),
  d = 1
)

# Mark row 2 and 5 as QC (always kept, but excluded from filtering thresholds)
is_qc <- c(FALSE, TRUE, FALSE, FALSE, TRUE)
target_cols <- c("a","b","c")

# Original
print(df)
#>    a  b  c d
#> 1 NA NA  3 1
#> 2  1 NA NA 1
#> 3 NA  2 NA 1
#> 4  1  2  3 1
#> 5 NA NA NA 1
#    a  b  c d
# 1 NA NA  3 1
# 2  1 NA NA 1
# 3 NA  2 NA 1
# 4  1  2  3 1
# 5 NA NA NA 1

# Iterative filtering
print(filter_by_missingness(df, row_thresh=0.5, col_thresh=0.5,
                            target_cols=target_cols, is_qc=is_qc,
                            filter_order="iterative"))
#>    b  c d
#> 1 NA  3 1
#> 2 NA NA 1
#> 3  2 NA 1
#> 4  2  3 1
#> 5 NA NA 1
# [1] "iteration"
#    b  c d
# 1 NA  3 1
# 2 NA NA 1
# 3  2 NA 1
# 4  2  3 1
# 5 NA NA 1

# Simultaneous filtering
print(filter_by_missingness(df, row_thresh=0.5, col_thresh=0.5,
                            target_cols=target_cols, is_qc=is_qc,
                            filter_order="simultaneous"))
#>    b  c d
#> 2 NA NA 1
#> 4  2  3 1
#> 5 NA NA 1
# [1] "simultaneous"
#    b  c d
# 2 NA NA 1
# 4  2  3 1
# 5 NA NA 1

# Column-then-row filtering
print(filter_by_missingness(df, row_thresh=0.5, col_thresh=0.5,
                            target_cols=target_cols, is_qc=is_qc,
                            filter_order="col_then_row"))
#>    b  c d
#> 1 NA  3 1
#> 2 NA NA 1
#> 3  2 NA 1
#> 4  2  3 1
#> 5 NA NA 1
#    b  c d
# 1 NA  3 1
# 2 NA NA 1
# 3  2 NA 1
# 4  2  3 1
# 5 NA NA 1

# Row-then-column filtering
print(filter_by_missingness(df, row_thresh=0.5, col_thresh=0.5,
                            target_cols=target_cols, is_qc=is_qc,
                            filter_order="row_then_col"))
#>    a  b  c d
#> 2  1 NA NA 1
#> 4  1  2  3 1
#> 5 NA NA NA 1
#    a  b  c d
# 2  1 NA NA 1
# 4  1  2  3 1
# 5 NA NA NA 1

# For this simple data set the "iteration" and the "col_then_row" filtering results are the same.
```
