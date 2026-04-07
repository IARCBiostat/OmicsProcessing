# Prepare long-format data for feature plots

Reshapes a wide data frame of feature intensities into long format
suitable for plotting against run order. Adds standardised columns for
run order, feature name, value, QC status, batch, and plate.

## Usage

``` r
prepare_df_long(
  df,
  target_cols,
  run_order,
  is_qc = NULL,
  batch = NULL,
  plate = NULL
)
```

## Arguments

- df:

  A data frame containing feature measurements and metadata.

- target_cols:

  Character vector of feature column names to include.

- run_order:

  Character scalar specifying the run order column.

- is_qc:

  Optional character scalar specifying the QC indicator column. If
  `NULL`, all samples are treated as non-QC.

- batch:

  Optional character scalar specifying the batch column. If `NULL`, a
  single batch level `"all"` is used.

- plate:

  Optional character scalar specifying the plate column. If `NULL`, a
  single plate level `"all"` is used.

## Value

A long-format `data.frame` with columns:

- run_order:

  Numeric run order.

- feature:

  Feature name.

- value:

  Feature intensity.

- is_qc:

  Logical QC indicator.

- batch:

  Factor batch assignment.

- plate:

  Factor plate assignment.

## Examples

``` r
df_long <- prepare_df_long(
  df = df,
  target_cols = c("feat1", "feat2"),
  run_order = "injection_order",
  is_qc = "qc_flag",
  batch = "batch",
  plate = "plate"
)
#> Error in prepare_df_long(df = df, target_cols = c("feat1", "feat2"), run_order = "injection_order",     is_qc = "qc_flag", batch = "batch", plate = "plate"): could not find function "prepare_df_long"
```
