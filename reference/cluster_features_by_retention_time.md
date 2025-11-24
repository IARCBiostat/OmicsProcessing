# Cluster features by retention time and summarise within clusters

Groups features into clusters based on their retention time using
hierarchical clustering, then selects a representative feature for each
cluster either via scores or via correlation-based aggregation.

## Usage

``` r
cluster_features_by_retention_time(
  df,
  target_cols = NULL,
  is_qc = NULL,
  rt_height = 0.07,
  method = c("correlations", "scores"),
  scores = NULL,
  cut_height = NULL,
  corr_thresh = NULL
)
```

## Arguments

- df:

  A data.frame (or tibble) with feature data in columns and samples in
  rows.

- target_cols:

  Character vector of feature/column names to use. If `NULL`,
  [`resolve_target_cols()`](https://iarcbiostat.github.io/OmicsProcessing/reference/resolve_target_cols.md)
  is used to determine the set of features.

- is_qc:

  Logical vector marking QC samples. Must have length equal to
  `nrow(df)`. If `NULL`, all samples are treated as non-QC. (Currently
  only used for consistency with other interfaces; it is not used
  directly in this function.)

- rt_height:

  Numeric height at which to cut the dendrogram in retention-time space.
  Smaller values create more clusters.

- method:

  Character string indicating how to obtain a single representative per
  retention-time cluster. Either `"correlations"` or `"scores"`. With
  `"scores"`, one existing feature per cluster is selected based on
  pre-computed scores. With `"correlations"`, clusters are further
  processed using correlation-based summarisation.

- scores:

  Named numeric vector of feature scores, required when
  `method = "scores"`. Names must coincide with `target_cols`. For each
  retention-time cluster, the feature with the largest score is chosen
  as representative.

- cut_height:

  Numeric height used inside `cluster_features_based_on_correlations`
  when `method = "correlations"`. Passed on to that function's
  `cut_height` argument.

- corr_thresh:

  Numeric threshold used inside `cluster_features_based_on_correlations`
  when `method = "correlations"`. Passed on to that function's
  `corr_thresh` argument.

## Value

A list with two elements:

- `clustered_df`:

  A data.frame with one column per final representative feature. When
  `method = "scores"`, this consists of a subset of the original feature
  columns. When `method = "correlations"`, it may contain synthetic
  features created by
  [`cluster_features_based_on_correlations()`](https://iarcbiostat.github.io/OmicsProcessing/reference/cluster_features_based_on_correlations.md).

- `representatives_map`:

  A named list mapping each representative feature (original or
  synthetic) to the character vector of feature names that it represents
  within its retention-time cluster.

## Details

The function proceeds in two steps:

1.  Retention-time clustering: feature names in `target_cols` are first
    parsed using
    [`parse_mass_rt()`](https://iarcbiostat.github.io/OmicsProcessing/reference/parse_mass_rt.md)
    to extract retention times, which are then clustered by
    `cluster_hierarchical()` at height `rt_height`. This yields groups
    of features with similar retention times.

2.  Within-cluster summarisation:

    - If `method = "scores"`, each retention-time cluster is reduced to
      a single original feature using
      [`get_features_representatives_based_on_scores()`](https://iarcbiostat.github.io/OmicsProcessing/reference/get_features_representatives_based_on_scores.md),
      based on the supplied `scores`.

    - If `method = "correlations"`, each retention-time cluster is
      passed as a feature list to
      [`cluster_features_based_on_correlations()`](https://iarcbiostat.github.io/OmicsProcessing/reference/cluster_features_based_on_correlations.md),
      which may further split or summarise the cluster into synthetic
      variables based on correlations.

## See also

[`resolve_target_cols`](https://iarcbiostat.github.io/OmicsProcessing/reference/resolve_target_cols.md),
[`parse_mass_rt`](https://iarcbiostat.github.io/OmicsProcessing/reference/parse_mass_rt.md),
`cluster_hierarchical`,
[`get_features_representatives_based_on_scores`](https://iarcbiostat.github.io/OmicsProcessing/reference/get_features_representatives_based_on_scores.md),
[`cluster_features_based_on_correlations`](https://iarcbiostat.github.io/OmicsProcessing/reference/cluster_features_based_on_correlations.md)

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(
  `100@150` = rnorm(20),
  `100@151` = rnorm(20),
  `101@200` = rnorm(20)
)

target_cols <- c("100@150", "100@151", "101@200")
is_qc <- rep(FALSE, nrow(df))

## Using scores to pick one feature per RT cluster
scores <- c(
  `100@150` = 0.2,
  `100@151` = 0.8,
  `101@200` = 0.5
)

res_scores <- cluster_features_by_retention_time(
  df = df,
  target_cols = target_cols,
  is_qc = is_qc,
  rt_height = 0.07,
  method = "scores",
  scores = scores
)

## Using correlation-based summarisation within RT clusters
res_corr <- cluster_features_by_retention_time(
  df = df,
  target_cols = target_cols,
  is_qc = is_qc,
  rt_height = 0.07,
  method = "correlations",
  cut_height = 0.26,
  corr_thresh = 0.75
)
} # }
```
