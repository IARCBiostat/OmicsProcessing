# Impute missing data in a data frame

This function imputes missing data in a data frame (`df`) using the
specified `imputation_method`. Supported methods include "LOD", "1/5th",
"KNN", "PPCA", "median", "mean", "RF", and "LCMD".

## Usage

``` r
impute_data(
  df,
  df_meta_features = NULL,
  imputation_method = "mean",
  col_features = NULL,
  col_LOD = NULL
)
```

## Arguments

- df:

  A data frame with missing values to be imputed.

- df_meta_features:

  A data frame containing feature metadata, required for "LOD"
  imputation.

- imputation_method:

  A character string specifying the imputation method. Valid options
  are: "LOD", "1/5th", "KNN", "PPCA", "median", "mean", "RF", "LCMD".

- col_features:

  (Optional) Character name of the column in `df_meta_features`
  containing feature names, required for "LOD" imputation.

- col_LOD:

  (Optional) Character name of the column in `df_meta_features`
  containing the limit of detection (LOD) values, required for "LOD"
  imputation.

## Value

A data frame (`df`) with imputed values.
