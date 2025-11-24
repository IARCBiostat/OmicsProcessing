# check required columns

This function checks for the presence of required columns It verifies
that necessary columns are present based on specified exclusion flags.

## Usage

``` r
data_check(
  data,
  data_meta_features,
  data_meta_samples,
  col_samples,
  col_features = NULL,
  exclusion_extreme_feature = FALSE,
  exclusion_extreme_sample = FALSE,
  missing_pct_feature = NULL,
  missing_pct_sample = NULL
)
```

## Arguments

- data:

  Data frame containing sample data, which must include `col_samples`.

- data_meta_features:

  Data frame containing feature metadata, which must include
  `col_features`

- data_meta_samples:

  Data frame containing sample metadata, which must include
  `col_samples`

- col_samples:

  Character vector specifying the column name(s) in `data` representing
  sample IDs.

- col_features:

  Character string specifying the column name in `data_meta_features`
  representing feature IDs (optional, only checked if provided).

- exclusion_extreme_feature:

  Logical; if `TRUE`, `missing_pct_feature` must be provided and exist
  in `data_meta_features`.

- exclusion_extreme_sample:

  Logical; if `TRUE`, `missing_pct_sample` must be provided and exist in
  `data_meta_samples`.

- missing_pct_feature:

  Numeric value indicating the acceptable percentage of missing data for
  features (required if `exclusion_extreme_feature` is `TRUE`).

- missing_pct_sample:

  Numeric value indicating the acceptable percentage of missing data for
  samples (required if `exclusion_extreme_sample` is `TRUE`).

## Value

Logical; `TRUE` if all required columns are present, otherwise an error
is raised.
