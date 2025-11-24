# Hybrid imputation with RF + LCMD

## Step 4: Hybrid Imputation (Random Forest + LCMD)

``` r
imputed_results <- OmicsProcessing::hybrid_imputation(
  log_transformed_df,
  target_cols = "@",
  method = c("RF-LCMD"),
  oobe_threshold = 0.1
)
imputed_df <- imputed_results$hybrid_rf_lcmd
```

[`hybrid_imputation()`](https://iarcbiostat.github.io/OmicsProcessing/reference/hybrid_imputation.md)
combines two complementary strategies:

- **Random Forest (RF)** via
  [`missForest::missForest()`](https://rdrr.io/pkg/missForest/man/missForest.html)
  for features that are well-predicted by other variables (low OOBE).
- **Left-Censored Missing Data (LCMD)** via
  [`imputeLCMD::impute.MAR.MNAR()`](https://rdrr.io/pkg/imputeLCMD/man/impute.MAR.MNAR.html)
  for features with higher OOBE.

The function fits RF per feature, uses the out-of-bag error (OOBE) to
decide whether to keep the RF estimate or switch that feature to LCMD,
and returns:

- `hybrid_rf_lcmd`: the combined result.
- `rf` / `lcmd`: per-method outputs.
- `oob`: OOBE values (helpful for diagnostics).

See the full reference:
[`hybrid_imputation()`](https://iarcbiostat.github.io/OmicsProcessing/reference/hybrid_imputation.md).

### Customising the RF and LCMD controls

You can tweak both engines via control lists:

``` r
my_control_RF <- list(
  parallelize = "no",
  mtry = floor(sqrt(length(target_cols))),
  ntree = 100,
  maxiter = 10,
  variablewise = TRUE,
  verbose = TRUE,
  n_cores = parallel::detectCores()
)

my_control_LCMD <- list(
  method.MAR = "KNN",
  method.MNAR = "QRILC"
)

df_rf_lcmd_hybrid <- OmicsProcessing::hybrid_imputation(
  log_transformed_df,
  target_cols = "@",
  method = c("RF-LCMD"),
  oobe_threshold = 0.1,
  control_LCMD = my_control_LCMD,
  control_RF = my_control_RF
)
```

### Parallelising the RF step (`missForest`)

`missForest` can run in parallel when you register a **foreach** backend
and set `parallelize`:

``` r
library(doParallel)

n_cores <- parallel::detectCores(logical = FALSE)
cl <- parallel::makeCluster(n_cores)
doParallel::registerDoParallel(cl)

ctrl_parallel_RF <- list(
  parallelize = "variables", # or "forests"
  mtry = floor(sqrt(length(target_cols))),
  ntree = 200,
  maxiter = 10,
  variablewise = TRUE,
  verbose = TRUE
)

imputed_parallel <- OmicsProcessing::hybrid_imputation(
  log_transformed_df,
  target_cols = "@",
  method = "RF-LCMD",
  oobe_threshold = 0.1,
  control_RF = ctrl_parallel_RF
)

parallel::stopCluster(cl)
doParallel::registerDoSEQ()
```

Guidance:

- Use `parallelize = "variables"` for many features; `"forests"` spreads
  trees instead.
- Keep `ntree` reasonable when parallelising to avoid memory pressure.

Tips:

- Keep `target_cols` explicit when possible for clarity; `"@"` will use
  all feature columns resolved via
  [`resolve_target_cols()`](https://iarcbiostat.github.io/OmicsProcessing/reference/resolve_target_cols.md).
- Inspect `imputed_results$oob` to confirm the RF ↔︎ LCMD split aligns
  with your expectations.
- For very wide matrices, tune `ntree`, `mtry`, or the number of worker
  cores to balance runtime and stability.
