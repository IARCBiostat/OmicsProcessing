# Batch correction using SERRF

## Batch correction using Systematical Error Removal using Random Forest (SERRF)

``` r
out_serrf <- OmicsProcessing::normalise_SERRF(
  imputed_df,
  target_cols = "@",
  is_qc = grepl("^sQC", imputed_df$sample_type),
  strata_col = "batch"  # omit to treat all samples as one strata
)
```

[`normalise_SERRF()`](https://iarcbiostat.github.io/OmicsProcessing/reference/normalise_SERRF.md)
models unwanted technical variation using QC samples within each
batch/stratum and applies SERRF normalization to the specified feature
columns. Use `target_cols` to select features by name, tidyselect
helper, or regex (resolved via
[`resolve_target_cols()`](https://iarcbiostat.github.io/OmicsProcessing/reference/resolve_target_cols.md)),
and supply a logical `is_qc` flag for each row. See the reference:
[`normalise_SERRF()`](https://iarcbiostat.github.io/OmicsProcessing/reference/normalise_SERRF.md).

### Input requirements

- `strata_col`: provide the batch/strata column when it exists; if you
  omit it, a dummy single-level strata is created so all samples are
  treated as one batch. If supplied, it is coerced to a factor (no NAs).
- `is_qc` is a logical vector the same length as `nrow(df)`; ensure
  every batch has enough QC samples for the SERRF model.
- All `target_cols` must be numeric and **contain no NA values** (impute
  beforehand).
- At least three target features are required (SERRF needs predictors to
  model each feature).

### Tuning and tips

- `num_screen_SERRF` (default 10) controls how many correlated features
  are screened per target; increase cautiously if you have many features
  and stable QC coverage.
- Inspect before/after QC CVs or PCA to verify batch drift is reduced.
- Keep batches balanced: very small batches or few QC points can lead to
  unstable fits; consider merging sparse strata or adding QC if
  possible.
