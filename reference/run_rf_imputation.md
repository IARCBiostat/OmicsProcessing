# Run Random Forest Imputation with Optional Parallelization

This function performs missing value imputation using the `missForest`
package on a subset of columns in a data frame. Column selection can be
explicit or based on a regular expression, using
[`resolve_target_cols`](https://iarcbiostat.github.io/OmicsProcessing/reference/resolve_target_cols.md)
internally.

## Usage

``` r
run_rf_imputation(df, target_cols, control_RF = list())
```

## Arguments

- df:

  A data frame containing the data to be imputed. Only non-QC rows
  should be passed here.

- target_cols:

  A character vector of column names or a regex string to identify
  columns to impute.

- control_RF:

  A named list of additional or overriding arguments passed to
  [`missForest::missForest()`](https://rdrr.io/pkg/missForest/man/missForest.html).
  Also supports an internal control argument `n_cores` (numeric), used
  to determine parallelism when `parallelize` is not "no".

  Accepted entries include (see
  [`missForest::missForest`](https://rdrr.io/pkg/missForest/man/missForest.html)
  for details):

  `mtry`

  :   Number of variables randomly sampled at each split.

  `ntree`

  :   Number of trees to grow.

  `maxiter`

  :   Maximum number of imputation iterations.

  `parallelize`

  :   Character string: "no", "variables", or "forests". Enables
      parallelization.

  `variablewise`

  :   Logical. If TRUE, variable-wise error is returned.

  `verbose`

  :   Logical. If TRUE, progress messages are printed.

  `replace`

  :   Logical. Whether sampling of observations is done with
      replacement.

  `classwt`, `cutoff`, `strata`, `sampsize`, etc.

  :   Other optional arguments passed to
      [`randomForest::randomForest`](https://rdrr.io/pkg/randomForest/man/randomForest.html).

  `n_cores`

  :   (Internal) Number of CPU cores to use if `parallelize` is not
      "no". This is removed before calling `missForest()`.

## Value

A list with two components:

- imputed:

  A data frame of the same shape as the input subset, with imputed
  values.

- oob:

  A named numeric vector of out-of-bag error estimates for each feature
  (if available).

## Details

The function supports optional parallel execution via `missForest`'s
`parallelize` argument. If `n_cores` is less than or equal to 1,
parallelization is disabled automatically and the imputation runs
sequentially.

## See also

[`missForest`](https://rdrr.io/pkg/missForest/man/missForest.html),
[`resolve_target_cols`](https://iarcbiostat.github.io/OmicsProcessing/reference/resolve_target_cols.md),
`register_cluster`, `unregister_cluster`

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(a = c(1, NA, 3), b = c(4, 5, NA))

# Default imputation on selected columns
run_rf_imputation(df, target_cols = c("a", "b"))

# Use regex to select columns and customize RF parameters
run_rf_imputation(
    df,
    target_cols = "^a|b$",
    control_RF = list(ntree = 200, mtry = 1, n_cores = 4)
)
} # }
```
