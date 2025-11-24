# Select representative features per cluster based on scores

For each cluster, this function selects a single representative feature.
If a cluster contains one feature, that feature is selected. If it
contains multiple features, the feature with the highest score in
`scores` is selected.

## Usage

``` r
get_features_representatives_based_on_scores(cluster_feature_list, scores)
```

## Arguments

- cluster_feature_list:

  A list where each element is a character vector of feature names
  belonging to one cluster. Each element must be a non-empty character
  vector.

- scores:

  A named numeric vector where names correspond to feature names and
  values represent feature scores. All features appearing in
  `cluster_feature_list` must be present in `names(scores)`.

## Value

A list with two elements:

- `representatives`:

  A character vector giving one selected representative feature per
  cluster.

- `representatives_map`:

  A named list mapping each representative feature to the vector of
  original features in its cluster.

## Details

The function also returns a named list mapping each representative
feature to all cluster members it represents.

## Examples

``` r
cluster_feature_list <- list(
  c("feat_a", "feat_b", "feat_c"),
  "feat_d",
  c("feat_e", "feat_f")
)

scores <- c(
  feat_a = 0.1,
  feat_b = 0.4,
  feat_c = 0.3,
  feat_d = 0.9,
  feat_e = 0.2,
  feat_f = 0.8
)

res <- get_features_representatives_based_on_scores(
  cluster_feature_list = cluster_feature_list,
  scores = scores
)

res$representatives
#> [1] "feat_b" "feat_d" "feat_f"
res$representatives_map
#> $feat_b
#> [1] "feat_a" "feat_b" "feat_c"
#> 
#> $feat_f
#> [1] "feat_e" "feat_f"
#> 
```
