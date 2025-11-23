test_that("single-feature clusters are returned unchanged", {
  df <- data.frame(a = 1:4, b = 5:8)

  res <- cluster_features_based_on_correlations(
    df = df,
    cluster_feature_list = list("a", "b"),
    cut_height = 0.26,
    corr_thresh = 0.75
  )

  expect_equal(names(res$clustered_df), c("a", "b"))
  expect_length(res$representatives_map, 0)
})

test_that("two-feature clusters above threshold are collapsed to synthetic feature", {
  skip_if_not_installed("FactoMineR")

  set.seed(123)
  x <- rnorm(10)
  y <- x + rnorm(10, sd = 0.001)  # highly correlated with x
  df <- data.frame(x = x, y = y)

  res <- cluster_features_based_on_correlations(
    df = df,
    cluster_feature_list = list(c("x", "y")),
    cut_height = 0.26,
    corr_thresh = 0.5
  )

  expect_setequal(names(res$clustered_df), "SynthFeat@1")
  expect_equal(res$representatives_map$`SynthFeat@1`, c("x", "y"))
  expect_gt(cor(res$clustered_df$`SynthFeat@1`, df$x), 0)
})

test_that("two-feature clusters below threshold keep both originals", {
  set.seed(456)
  df <- data.frame(
    x = rnorm(12),
    y = rnorm(12)
  )

  res <- cluster_features_based_on_correlations(
    df = df,
    cluster_feature_list = list(c("x", "y")),
    cut_height = 0.26,
    corr_thresh = 0.95
  )

  expect_setequal(names(res$clustered_df), c("x", "y"))
  expect_length(res$representatives_map, 0)
})

test_that("clusters of size three use ClustOfVar path", {
  skip_if_not_installed("FactoMineR")
  skip_if_not_installed("ClustOfVar")

  set.seed(789)
  f1 <- rnorm(15)
  f2 <- f1 + rnorm(15, sd = 0.01)  # very similar to f1
  f3 <- rnorm(15)                  # different feature
  df <- data.frame(f1 = f1, f2 = f2, f3 = f3)

  res <- cluster_features_based_on_correlations(
    df = df,
    cluster_feature_list = list(c("f1", "f2", "f3")),
    cut_height = 0.2,
    corr_thresh = 0.75
  )

  expect_setequal(names(res$clustered_df), c("SynthFeat@1", "f3"))
  expect_equal(res$representatives_map$`SynthFeat@1`, c("f1", "f2"))
  expect_gt(cor(res$clustered_df$`SynthFeat@1`, df$f1), 0)
})
