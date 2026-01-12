# Cluster feature groups based on correlations

Given a list of pre-defined feature groups, construct a data set of
representative features by collapsing groups with high correlations into
synthetic variables.

## Usage

``` r
cluster_features_based_on_correlations(
  df,
  cluster_feature_list,
  cut_height,
  corr_thresh
)
```

## Arguments

- df:

  A tibble or data.frame containing the feature columns (and possibly
  other variables not used here). All variables referenced in
  `cluster_feature_list` must be columns of `df`.

- cluster_feature_list:

  A list where each element is a character vector giving the names of
  the features that belong to one initial cluster. Feature names are
  typically of the form `"RT@mass"`, where `RT` and `mass` are real
  numbers corresponding to retention time and mass, but any valid column
  names are accepted.

- cut_height:

  Numeric. Cut-off height for the correlation-based clustering within
  groups of size three or more (as implemented in ClustOfVar). A
  suggested value is `0.26`, which corresponds roughly to a pairwise
  correlation of about `0.75`. You can use a dedicated helper (e.g.
  `simul_eval_link_hhh_corr`) to explore the correspondence between
  pairwise correlations and the tree height returned by
  [`ClustOfVar::hclustvar()`](https://rdrr.io/pkg/ClustOfVar/man/hclustvar.html).

- corr_thresh:

  Numeric. Cut-off for the absolute pairwise correlation used when the
  number of features in a group is exactly two. If the absolute
  correlation between the two features is at least `corr_thresh`, they
  are replaced by a single synthetic feature (first principal
  component); otherwise both original features are kept.

## Value

A list with the following components:

- clustered_df:

  A tibble with one row per observation and one column per final
  representative feature. For clusters of size one, the column is the
  original feature. For size two with correlation above `corr_thresh`,
  the column is the first principal component of the two features. For
  larger clusters, columns are synthetic variables returned by
  ClustOfVar, oriented to have a positive correlation with the first
  feature of the corresponding sub-cluster.

- representatives_map:

  A named list mapping each synthetic feature (columns whose names start
  with `"SynthFeat@"`) to the character vector of original feature names
  that it summarises. Original features that are kept as is do not
  appear in this map.

## Details

For groups of size one, the original feature is kept as is. For groups
of size two, the decision is based on their pairwise correlation. For
larger groups, clustering is based on the decrease in homogeneity as in
the ClustOfVar package. The homogeneity of a cluster is the squared
correlation (for quantitative variables) between the variables and the
cluster centre (the first principal component).

This function is typically used after a pre-grouping step (for example,
after retention-time-based grouping). Each element of
`cluster_feature_list` corresponds to one such pre-defined group and may
contain one, two, or more feature names; all cases are handled.

For groups of size three or more, a hierarchical clustering tree is
first obtained using
[`ClustOfVar::hclustvar()`](https://rdrr.io/pkg/ClustOfVar/man/hclustvar.html).
The tree is then cut at a data-driven position derived from
`cut_height`, and
[`ClustOfVar::cutreevar()`](https://rdrr.io/pkg/ClustOfVar/man/cutreevar.html)
is used to obtain synthetic scores for the resulting sub-clusters. Each
score is multiplied by \\+1\\ or \\-1\\ so that it has a positive
correlation with the first feature in its sub-cluster.

## Examples

``` r
set.seed(123)
data_tbl <- tibble::tibble(
  `100@150` = rnorm(20),
  `100@151` = rnorm(20),
  `101@200` = rnorm(20)
)

# Pre-defined feature groups (e.g. after a retention-time grouping step)
cluster_feature_list <- list(
  c("100@150", "100@151"), # group of size 2
  "101@200" # group of size 1
)

res <- cluster_features_based_on_correlations(
  df = data_tbl,
  cluster_feature_list = cluster_feature_list,
  cut_height = 0.26,
  corr_thresh = 0.75
)

res$clustered_df
#>        100@150     100@151     101@200
#> 1  -0.56047565 -1.06782371 -0.69470698
#> 2  -0.23017749 -0.21797491 -0.20791728
#> 3   1.55870831 -1.02600445 -1.26539635
#> 4   0.07050839 -0.72889123  2.16895597
#> 5   0.12928774 -0.62503927  1.20796200
#> 6   1.71506499 -1.68669331 -1.12310858
#> 7   0.46091621  0.83778704 -0.40288484
#> 8  -1.26506123  0.15337312 -0.46665535
#> 9  -0.68685285 -1.13813694  0.77996512
#> 10 -0.44566197  1.25381492 -0.08336907
#> 11  1.22408180  0.42646422  0.25331851
#> 12  0.35981383 -0.29507148 -0.02854676
#> 13  0.40077145  0.89512566 -0.04287046
#> 14  0.11068272  0.87813349  1.36860228
#> 15 -0.55584113  0.82158108 -0.22577099
#> 16  1.78691314  0.68864025  1.51647060
#> 17  0.49785048  0.55391765 -1.54875280
#> 18 -1.96661716 -0.06191171  0.58461375
#> 19  0.70135590 -0.30596266  0.12385424
#> 20 -0.47279141 -0.38047100  0.21594157
res$representatives_map
#> list()
```
