test_that("requires exactly one of height or k", {
    expect_error(
        cluster_hierarchical(c(1, 2, 3)),
        "Supply exactly one of `height` or `k`\\."
    )
    expect_error(
        cluster_hierarchical(c(1, 2, 3), height = 1, k = 2),
        "Supply only one of `height` or `k`\\."
    )
})

test_that("clusters numeric vector with a height cut", {
    vec <- c(1, 1.1, 5, 5.2)
    res <- cluster_hierarchical(vec, height = 0.3)
    expect_equal(unname(res), c(1L, 1L, 2L, 2L))
})

test_that("clusters matrix input into k groups", {
    mat <- cbind(
        a = c(1, 1.1, 5, 5.2),
        b = c(10, 11, 30, 31)
    )
    res <- cluster_hierarchical(mat, k = 2)

    expect_equal(unname(res), c(1L, 1L, 2L, 2L))
})

test_that("correlation-based path produces synthetic feature for RT cluster", {
    skip_if_not_installed("FactoMineR")
    skip_if_not_installed("ClustOfVar")

    set.seed(42)
    f1 <- rnorm(20)
    f2 <- f1 + rnorm(20, sd = 0.01)  # highly correlated, close RTs
    f3 <- rnorm(20)                  # different RT cluster

    df <- data.frame(
        `100@150` = f1,
        `100@151` = f2,
        `101@200` = f3,
        check.names = FALSE
    )

    res <- cluster_features_by_retention_time(
        df = df,
        target_cols = c("100@150", "100@151", "101@200"),
        rt_height = 1,  # group first two together
        method = "correlations",
        cut_height = 0.2,
        corr_thresh = 0.5
    )

    expect_setequal(names(res$clustered_df), c("SynthFeat@1", "101@200"))
    expect_equal(res$representatives_map$`SynthFeat@1`, c("100@150", "100@151"))
    expect_gt(cor(res$clustered_df$`SynthFeat@1`, df$`100@150`), 0)
})
