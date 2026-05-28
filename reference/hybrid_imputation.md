# Hybrid Imputation: Random Forest + LCMD

Performs hybrid imputation on selected target columns of a data frame by
combining Random Forest (RF) imputation via `missForest` and
left-censored missing data (LCMD) imputation via `imputeLCMD`. Only
non-QC rows are imputed. QC rows are excluded from model fitting, OOB
error estimation, and imputation, and are returned unchanged. Non-target
columns are also returned unchanged.

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

  Character vector of column names, or a single regular expression,
  identifying columns to impute. Only these columns are passed to the RF
  and LCMD imputation routines and only these columns can be modified in
  the returned data frames. Non-target columns are retained but are not
  imputed or used for method selection. If `NULL`, target columns are
  resolved automatically using
  [`resolve_target_cols()`](https://iarcbiostat.github.io/OmicsProcessing/reference/resolve_target_cols.md).

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

## Details

For each target column, the final imputed values are selected according
to the feature-level out-of-bag error (OOBE) from the RF model. Target
columns with RF OOBE strictly below `oobe_threshold` use RF-imputed
values, whereas target columns with RF OOBE greater than or equal to
`oobe_threshold` use LCMD-imputed values.

In addition to the hybrid result, the function also returns the complete
RF and LCMD imputed data frames. This allows users to inspect
feature-level method choices, perform sensitivity analyses, compare
alternative imputation strategies, or generate new hybrid imputed data
frames using different OOBE thresholds without rerunning the imputation
procedures.

## See also

[`run_rf_imputation`](https://iarcbiostat.github.io/OmicsProcessing/reference/run_rf_imputation.md),
[`run_lcmd_imputation`](https://iarcbiostat.github.io/OmicsProcessing/reference/run_lcmd_imputation.md)
