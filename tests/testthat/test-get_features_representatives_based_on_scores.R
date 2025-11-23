test_that("cluster_feature_list must be a list", {
  expect_error(
    get_features_representatives_based_on_scores("not_a_list", c(a = 1)),
    "`cluster_feature_list` must be a list\\."
  )
})

test_that("scores must be a named numeric vector", {
  expect_error(
    get_features_representatives_based_on_scores(list("a"), c(1, 2)),
    "`scores` must be a named numeric vector\\."
  )
})

test_that("each cluster element must be character vector", {
  expect_error(
    get_features_representatives_based_on_scores(list(1:3), c(a = 1)),
    "Each element of `cluster_feature_list` must be a"
  )
})

test_that("errors when scores for cluster members are missing", {
  expect_error(
    get_features_representatives_based_on_scores(
      list("a", c("b", "c")),
      c(a = 1, b = 2)
    ),
    "Some features in cluster 2 have no score: c"
  )
})

test_that("selects highest scoring feature per cluster and returns map", {
  clusters <- list(
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
    cluster_feature_list = clusters,
    scores = scores
  )

  expect_equal(res$representatives, c("feat_b", "feat_d", "feat_f"))
  expect_equal(
    res$representatives_map,
    list(
      feat_b = c("feat_a", "feat_b", "feat_c"),
      feat_f = c("feat_e", "feat_f")
    )
  )
})
