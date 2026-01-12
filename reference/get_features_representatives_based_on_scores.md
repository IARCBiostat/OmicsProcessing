# Select representative features per cluster using correlation threshold and scores

This function selects representative features from pre-defined clusters
of variables, using a correlation-based rule and a scoring vector:

## Usage

``` r
get_features_representatives_based_on_scores(
  df,
  cluster_feature_list,
  corr_thresh,
  scores
)
```

## Arguments

- df:

  A data frame containing the feature columns referenced in
  `cluster_feature_list`. Column names must match the feature names.

- cluster_feature_list:

  A list where each element is a non-empty character vector of feature
  names belonging to one cluster (e.g., the output of a prior clustering
  step).

- corr_thresh:

  A numeric threshold in `[0, 1]`. For two-feature clusters, if
  `|cor| >= corr_thresh`, the feature with the highest score is
  selected. For clusters with more than two features, subclusters are
  created by cutting the hierarchical tree at `h = 1 - corr_thresh`.

- scores:

  A named numeric vector of feature scores. All features appearing in
  `cluster_feature_list` must be present in `names(scores)`.

## Value

A list with two components:

- `representatives`:

  A character vector of selected representative features (one per
  single-feature cluster; zero, one, or more per multi-feature cluster,
  depending on the correlation structure and threshold).

- `representatives_map`:

  A named list mapping each representative feature (when uniquely
  chosen) to the character vector of features it represents.

## Details

- If a cluster contains a single feature, that feature is selected.

- If a cluster contains two features, the absolute pairwise correlation
  is computed. If `|cor| >= corr_thresh`, only the highest-scoring
  feature is selected; otherwise, both features are kept as
  representatives.

- If a cluster contains more than two features, hierarchical clustering
  is performed using the distance `1 - |cor|` with `method = "average"`.
  The dendrogram is cut at `h = 1 - corr_thresh` to form subclusters.
  From each subcluster, the highest-scoring feature is selected as the
  representative.

In all cases where a unique representative is chosen for a (sub)cluster,
`representatives_map` stores the mapping from the representative feature
to the set of original features it represents.

For clusters of size greater than two, the distance matrix is computed
as `1 - abs(cor(X))` on the selected columns, followed by
`hclust(..., method = "average")`. Subclusters are extracted via
`cutree(hc, h = 1 - corr_thresh)`. Within each subcluster, the
representative is the feature with the maximum score.

When a two-feature cluster has `|cor| < corr_thresh`, both features are
returned as representatives and no mapping is stored for that pair, as
neither represents the other by design.

## Examples

``` r
# Toy data frame
set.seed(1)
df <- data.frame(
  feat_a = rnorm(100),
  feat_b = rnorm(100),
  feat_c = rnorm(100),
  feat_d = rnorm(100),
  feat_e = rnorm(100),
  feat_f = rnorm(100)
)

# Example clusters
cluster_feature_list <- list(
  c("feat_a", "feat_b", "feat_c"), # size > 2 -> hierarchical subclusters
  "feat_d", # size = 1 -> itself
  c("feat_e", "feat_f") # size = 2 -> pairwise rule
)

# Scores for all features
scores <- c(
  feat_a = 0.1,
  feat_b = 0.4,
  feat_c = 0.3,
  feat_d = 0.9,
  feat_e = 0.2,
  feat_f = 0.8
)

# Choose a correlation threshold
corr_thresh <- 0.7

res <- get_features_representatives_based_on_scores(
  df = df,
  cluster_feature_list = cluster_feature_list,
  corr_thresh = corr_thresh,
  scores = scores
)

res$representatives
#> [1] "feat_a" "feat_b" "feat_c" "feat_d" "feat_e" "feat_f"
res$representatives_map
#> list()
```
