
test_that("get_features_representatives_based_on_scores works on example data", {
  set.seed(789)
  f0 <- rnorm(15)
  f1 <- rnorm(15)
  f2 <- f1 + rnorm(15, sd = 0.01)
  f3 <- f1 + rnorm(15, sd = 0.01)
  f4 <- rnorm(15)
  f5 <- f4 + rnorm(15, sd = 0.01)
  f6 <- rnorm(15)
  f7 <- rnorm(15)
  f8 <- f7 + rnorm(15, sd = 0.01)
  f9 <- rnorm(15)
  f10 <- rnorm(15)

  df <- data.frame(
    "f0@1" = f0,
    "f1@1" = f1,
    "f2@1" = f2,
    "f3@1" = f3,
    "f4@1" = f4,
    "f5@1" = f5,
    "f6@2" = f6,
    "f7@3" = f7,
    "f8@3" = f8,
    "f9@4" = f9,
    "f10@4" = f10,  check.names = FALSE
  )

  scores <- c(
    "f0@1" = 0.1,
    "f1@1" = 0.1,
    "f2@1" = 0.4,   # should win cluster 1
    "f3@1" = 0.3,
    "f4@1" = 0.9,   # should win cluster 2
    "f5@1" = 0.8,
    "f6@2" = 0.8,   # single-feature cluster
    "f7@3" = 0.8,
    "f8@3" = 1.8,   # should win cluster 3
    "f9@4" = 1.8,   # tie with f10@4 -> first max index chosen
    "f10@4" = 1.8
  )
  # Cluster list:
  # cluster 1: 4 highly correlated features around "1"
  # cluster 2: 2 highly correlated features
  # cluster 3: 2 highly correlated features
  # cluster 4: 2 moderately correlated features
  cluster_feature_list <- list(
    c("f0@1", "f1@1", "f2@1", "f3@1", "f4@1", "f5@1"),
    "f6@2",
    c("f7@3", "f8@3"),
    c("f9@4", "f10@4")
  )

  corr_thresh <- 0.75

  result <- get_features_representatives_based_on_scores(
    df = df,
    cluster_feature_list = cluster_feature_list,
    corr_thresh = corr_thresh,
    scores = scores
  )

  reps <- result$representatives
  reps_map <- result$representatives_map
  
  # === Scenario 1: single-feature cluster in an RT group2 ===
  expect_true("f6@2" %in% reps)

  # === Scenario 2: multi-feature (size > 2) high correlation subcluster in RT group1 ===
  # f2@1 has highest score among f0,f1,f2,f3
  expect_true("f2@1" %in% reps)
  expect_equal(reps_map[["f2@1"]], c("f1@1", "f2@1", "f3@1"))
  
  # === Scenario 2.1: single-feature in an RT group1 ===
  # f2@1 has highest score among f0,f1,f2,f3
  expect_true("f0@1" %in% reps)
  expect_true(is.null(reps_map[["f0@1"]]))

  # === Scenario 3: 2-feature cluster with high correlation in an RT group1 ===
  # f4@1 and f5@1 are highly correlated; f4@1 wins (score 0.9)
  expect_true("f4@1" %in% reps)
  expect_equal(reps_map[["f4@1"]], c("f4@1", "f5@1"))

  # === Scenario 4: Another 2-feature cluster with high correlation in RT group3 ===
  # f7@3 vs f8@3 → f8@3 wins (score 1.8)
  expect_true("f8@3" %in% reps)
  expect_equal(reps_map[["f8@3"]], c("f7@3", "f8@3"))

  # === Scenario 5: 2 un correlated features in RT group4 ===
  # f9@4 and f10@4 tie in score → the first max (f9@4) should be selected
  expect_true("f9@4" %in% reps)
  expect_true("f10@4" %in% reps)
  expect_true(is.null(reps_map[["f9@4"]]))
  expect_true(is.null(reps_map[["f10@4"]]))
})
