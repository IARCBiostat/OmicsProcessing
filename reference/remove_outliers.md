# Remove outliers (optionally stratified) using PCA + LOF

Wrapper around
[`outlier_pca_lof()`](https://iarcbiostat.github.io/OmicsProcessing/reference/outlier_pca_lof.md)
with conveniences:

- Select a subset of columns (`target_cols`)

- Exclude QC rows from outlier detection

- Temporarily impute missing values (half of column minimum) for
  detection

- Apply detection independently within user-defined strata (`strata`)

## Usage

``` r
remove_outliers(
  df,
  target_cols = NULL,
  is_qc = NULL,
  method = c("pca-lof-overall"),
  impute_method = c(NULL, "half-min-value"),
  restore_missing_values = TRUE,
  return_ggplots = FALSE,
  strata = NULL
)
```

## Arguments

- df:

  A data.frame with features in columns and samples in rows.

- target_cols:

  Character vector of column names (or tidyselect helpers if supported
  by
  [`resolve_target_cols()`](https://iarcbiostat.github.io/OmicsProcessing/reference/resolve_target_cols.md)).
  If `NULL`, uses `resolve_target_cols(df, NULL)` to infer targets.

- is_qc:

  Logical vector (length `nrow(df)`) marking QC rows to exclude from
  detection. Defaults to all `FALSE`.

- method:

  Character; currently supports `"pca-lof-overall"` (default behavior).

- impute_method:

  `NULL` or `"half-min-value"`. When set, missing values in
  `target_cols` are imputed as half the minimum non-missing value—by
  default **within each stratum**; if any \#' stratum has a target
  column entirely `NA`, imputation is performed **globally** on non-QC
  rows (see Missing-data policy).

- restore_missing_values:

  Logical; if `TRUE`, original `NA`s in `target_cols` are restored after
  filtering.

- return_ggplots:

  Logical; if `TRUE`, returns a named list of ggplots per stratum.

- strata:

  `NULL` (default), a single column name in `df`, or an external vector
  of length `nrow(df)`. When provided, outlier detection is run
  independently within each stratum (QC rows excluded within the
  stratum).

## Value

A list with:

- df_filtered:

  `df` with detected outlier rows removed (QC rows always retained).

- excluded_ids:

  Character vector of row names removed (union across strata).

- plot_samples_outlier:

  If `return_ggplots = TRUE`, a named list of ggplot objects per
  stratum; otherwise `NULL`.

## Details

### Stratification

Set `strata` to:

- `NULL` (default) to run detection once over all non-QC rows, or

- a single column name in `df`, or

- an external vector (length `nrow(df)`) to group samples.

Outlier detection is performed **independently within each stratum** on
non-QC rows (QC rows are always excluded from detection but retained in
the output). Strata with fewer than 5 non-QC samples are skipped (no
outliers removed for that stratum).

### Missing-data policy

- If `impute_method = NULL` and any `target_cols` contain missing values
  among non-QC rows, the function **errors** and lists the affected
  columns with counts. Enable `impute_method = "half-min-value"` or
  resolve missingness beforehand.

- If `impute_method = "half-min-value"`:

  - The function first checks for target columns that are **entirely
    `NA` across all non-QC rows**. If any exist, it **errors** (a
    half-minimum cannot be computed).

  - It then checks, **per stratum**, for target columns that are
    entirely `NA` **within that stratum**. If any are found, a
    **warning** is emitted listing the affected strata and columns, and
    **temporary imputation is applied on the whole non-QC dataset
    (ignoring stratification)**, while outlier detection still runs
    **per stratum** as requested.

- After outlier removal, if `restore_missing_values = TRUE`, the
  original `NA`s in `target_cols` are restored in the returned data.

## Examples

``` r
# 1) No stratification
remove_outliers(
  df,
  target_cols = c("f1","f2"),
  impute_method = "half-min-value"
)
#> Error in rep(FALSE, nrow(df)): invalid 'times' argument

# 2) Stratify by a column in df
remove_outliers(
  df,
  target_cols = c("f1","f2"),
  strata = "batch",
  impute_method = "half-min-value"
)
#> Error in rep(FALSE, nrow(df)): invalid 'times' argument

# 3) Stratify by an external vector
my_strata <- c("A", "A", "B", "B", "B", "C", "C")
remove_outliers(
  df,
  target_cols = c("f1","f2"),
  strata = grp,
  impute_method = "half-min-value"
)
#> Error in rep(FALSE, nrow(df)): invalid 'times' argument

# 4) Stratum with all-NA target columns -> triggers global temporary imputation (warning)
# \donttest{
df2 <- data.frame(
  f1 = c(1, 2, 3, NA, NA, NA),
  f2 = c(2, 3, 4, NA, NA, NA),
  batch = c("A","A","A","B","B","B")
)
rownames(df2) <- paste0("s", seq_len(nrow(df2)))
remove_outliers(
  df2,
  target_cols = c("f1","f2"),
  strata = "batch",
  impute_method = "half-min-value"
)
#> Warning: 
#> 
#> Detected strata with columns entirely missing among non-QC rows. Applying temporary imputation on the whole non-QC dataset (ignoring stratification). Affected strata and columns:
#> strat : columns
#>   - B: f1, f2
#> $df_filtered
#>    f1 f2 batch
#> s1  1  2     A
#> s2  2  3     A
#> s3  3  4     A
#> s4 NA NA     B
#> s5 NA NA     B
#> s6 NA NA     B
#> 
#> $plot_samples_outlier
#> NULL
#> 
#> $excluded_ids
#> character(0)
#> 
# }
```
