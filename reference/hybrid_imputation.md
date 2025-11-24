# Hybrid Imputation: Random Forest + LCMD

This function performs hybrid imputation on selected columns of a data
frame by combining Random Forest (RF) imputation (via `missForest`) and
left-censored missing data (LCMD) imputation (via `imputeLCMD`). It
selects the method per feature based on the out-of-bag error (OOBE) from
the RF model.

## Usage

``` r
hybrid_imputation(
  df,
  target_cols = NULL,
  is_qc = NULL,
  method = c("RF-LCMD"),
  oobe_threshold = 0.1,
  control_RF = list(),
  control_LCMD = list()
)
```

## Arguments

- df:

  A data frame with samples (rows) and features (columns).

- target_cols:

  A character vector of column names or a single regular expression to
  identify target columns for imputation. If NULL, all columns are
  considered.

- is_qc:

  A logical vector indicating which rows are QC samples. Must match
  `nrow(df)`.

- method:

  Imputation strategy to use (currently only `"RF-LCMD"` supported).

- oobe_threshold:

  Numeric. Features with OOBE below this threshold will use RF, others
  will use LCMD.

- control_RF:

  A named list of control arguments for
  [`missForest::missForest()`](https://rdrr.io/pkg/missForest/man/missForest.html).
  Also supports `n_cores` (internal).

- control_LCMD:

  A named list of control arguments for
  [`imputeLCMD::impute.MAR.MNAR()`](https://rdrr.io/pkg/imputeLCMD/man/impute.MAR.MNAR.html),
  including `mode = "overall"` or `"column-wise"`.

## Value

A named list with the following components:

- hybrid_rf_lcmd:

  The fully imputed data frame combining RF and LCMD decisions.

- rf:

  The RF-imputed data frame (non-QC rows only, in full column
  structure).

- lcmd:

  The LCMD-imputed data frame (non-QC rows only, in full column
  structure).

- oob:

  A named numeric vector of feature-level OOB errors from RF.

## See also

[`run_rf_imputation`](https://iarcbiostat.github.io/OmicsProcessing/reference/run_rf_imputation.md),
[`run_lcmd_imputation`](https://iarcbiostat.github.io/OmicsProcessing/reference/run_lcmd_imputation.md)
