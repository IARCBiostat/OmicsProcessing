# Feature clustering by retention time

## Cluster features by retention time

[`cluster_features_by_retention_time()`](https://iarcbiostat.github.io/OmicsProcessing/reference/cluster_features_by_retention_time.md)
groups features that elute together, based on similar retention times
(RTs). It returns a data frame that includes all unclustered features
**unchanged**, plus **one representative per cluster**. The
`representatives_map` output shows which original features each
representative corresponds to. See the function reference for further
details:
[`cluster_features_by_retention_time()`](https://iarcbiostat.github.io/OmicsProcessing/reference/cluster_features_by_retention_time.md).

> **Prerequisites:** use **imputed and normalised** data before applying
> RT clustering. This procedure assumes no missing values and benefits
> from stabilised intensity profiles.

The `is_qc` argument prevents QC-designated rows from contributing to
the clustering. Typically, `is_qc` should be set to `NULL` so that all
samples contribute.

### How representatives are selected with the “scores” method

The “scores” method clusters features by RT and returns **one
representative per RT cluster** based on the input score. It employs
[`get_features_representatives_based_on_scores()`](https://iarcbiostat.github.io/OmicsProcessing/reference/get_features_representatives_based_on_scores.md).
This helper is called internally by
[`cluster_features_by_retention_time()`](https://iarcbiostat.github.io/OmicsProcessing/reference/cluster_features_by_retention_time.md);
you do not need to invoke it explicitly.

For each RT-based cluster:

- Single-feature clusters: the feature is returned unchanged.
- Multi-feature clusters: the feature with the **highest score** is
  selected as the representative, and `representatives_map` records all
  members that it represents.

### Example 1: Score-based representatives (mean intensity before normalisation)

``` r
# Assume `imputed_df` is your imputed, SERRF-normalised data
# Use pre-normalisation means as scores
target_cols <- OmicsProcessing::resolve_target_cols(clean_df, "@")
scores <- colMeans(pre_normalised_df[, target_cols])

res_scores <- OmicsProcessing::cluster_features_by_retention_time(
  df = normalised_df,
  target_cols = target_cols,
  rt_height = 0.07,
  method = "scores",
  scores = scores
)

clustered_df <- res_scores$clustered_df     # original features + representatives
rep_map <- res_scores$representatives_map   # mapping of representatives to raw features
```

The `rt_height` parameter defines the maximum RT span for forming a
cluster: features whose RTs differ by less than this value are grouped.
Within each cluster, the supplied `scores` determine the representative.
In this example, pre-normalisation mean intensities serve as scores;
consequently, each representative is the feature with the highest mean
intensity before normalisation. The `representatives_map` lists
representatives as names, with vectors of raw feature names as their
values.

### How representatives are selected with the correlation method

With `method = "correlations"`, the features are first grouped together
by the `rt_height` (like the scores method). Then, a secondary
correlation-based clustering step is applied within each RT-defined
group. Clustering behaviour is governed by `corr_thresh` and
`cut_height`:

- RT cluster of size 1: retained unchanged.

- RT cluster of size 2:

  - If `|corr| ≥ corr_thresh`, the pair is summarised as a synthetic PC1
    feature.
  - Otherwise, both features are retained.

- RT cluster of size ≥3: hierarchical clustering (ClustOfVar) is cut at
  `cut_height`, and each resulting sub-cluster is summarised as a
  synthetic PC1, aligned to the first feature in that sub-cluster.

In all cases, the `representatives_map` lists synthetic feature names
and records the raw features incorporated into each synthetic
representative.

### Example 2: Correlation-based summarisation within RT clusters

``` r
res_corr <- OmicsProcessing::cluster_features_by_retention_time(
  df = normalised_df,
  target_cols = "@",
  is_qc = grepl("^sQC", normalised_df$sample_type),
  rt_height = 0.07,
  method = "correlations",
  cut_height = 0.26,
  corr_thresh = 0.75
)

clustered_df_corr <- res_corr$clustered_df
rep_map_corr <- res_corr$representatives_map
```

### Tips

- Set `rt_height` to reflect the instrument’s RT precision; smaller
  values produce more, and tighter, RT clusters.
- For `"scores"`, ensure that `scores` is a **named numeric vector**
  aligned with `target_cols`. Using pre-normalisation means is a simple
  and efficient strategy.
- For `"correlations"`, tune `cut_height` and `corr_thresh` to adjust
  the merging stringency for correlated features.
- Always inspect the `representatives_map` to understand how each
  representative relates to original features, particularly for
  reporting, audit trails, or sensitivity analyses.
