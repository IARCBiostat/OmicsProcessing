# Run LCMD Imputation (Overall)

Performs left-censored missing data (LCMD) imputation using
[`imputeLCMD::impute.MAR.MNAR()`](https://rdrr.io/pkg/imputeLCMD/man/impute.MAR.MNAR.html)
in overall mode.

## Usage

``` r
run_lcmd_imputation(df, target_cols, control_LCMD = list())
```

## Arguments

- df:

  A data frame containing the data to impute.

- target_cols:

  A character vector of column names to be imputed.

- control_LCMD:

  A named list of optional arguments:

  `method.MAR`

  :   Method for MAR imputation (e.g., "KNN").

  `method.MNAR`

  :   Method for MNAR imputation (e.g., "QRILC", "MinProb").

## Value

A data frame of the same shape and order as the input, with imputed
values.
