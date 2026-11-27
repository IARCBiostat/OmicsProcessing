test_that("outlier_pca_lof flags outliers by shape (Outlier-only legend) and returns PC1-PC2 plot", {
  set.seed(42)
  n <- 50
  p <- 12
  mat <- matrix(rnorm(n * p), nrow = n, ncol = p)
  colnames(mat) <- paste0("F", seq_len(p))
  rownames(mat) <- sprintf("SAMPLE_%03d", seq_len(n))
  mat[5, ] <- mat[5, ] + 6
  mat[22, ] <- mat[22, ] - 6
  df <- as.data.frame(mat)

  res <- outlier_pca_lof(df, return_ggplot = TRUE, verbose = FALSE)

  expect_named(
    res,
    c("df", "plot_samples_outlier", "plot_samples_outlier_pc1_pc2", "id_samples_outlier")
  )
  expect_true(all(c("SAMPLE_005", "SAMPLE_022") %in% res$id_samples_outlier))
  expect_s3_class(res$plot_samples_outlier, "ggmatrix")
  expect_s3_class(res$plot_samples_outlier_pc1_pc2, "ggplot")

  # Verify standalone PC1 vs PC2 shape scale has no title and only "Outlier" in breaks
  shape_scale_pc12 <- res$plot_samples_outlier_pc1_pc2$scales$get_scales("shape")
  expect_false(is.null(shape_scale_pc12))
  expect_null(shape_scale_pc12$name)
  expect_identical(shape_scale_pc12$breaks, "Outlier")

  # Verify both plots build cleanly without errors
  expect_no_error(ggplot2::ggplot_build(res$plot_samples_outlier_pc1_pc2))
  expect_no_error(GGally::ggmatrix_gtable(res$plot_samples_outlier))
})

test_that("outlier_pca_lof handles zero outliers cleanly with return_ggplot = TRUE", {
  set.seed(123)
  mat <- matrix(rnorm(50 * 12), nrow = 50, ncol = 12)
  colnames(mat) <- paste0("F", 1:12)
  rownames(mat) <- sprintf("CLEAN_%03d", 1:50)
  df <- as.data.frame(mat)

  res <- outlier_pca_lof(df, return_ggplot = TRUE, verbose = FALSE)

  expect_length(res$id_samples_outlier, 0L)
  expect_s3_class(res$plot_samples_outlier, "ggmatrix")
  expect_s3_class(res$plot_samples_outlier_pc1_pc2, "ggplot")
  expect_no_error(ggplot2::ggplot_build(res$plot_samples_outlier_pc1_pc2))
  expect_no_error(GGally::ggmatrix_gtable(res$plot_samples_outlier))
})

test_that("outlier_pca_lof returns NULL plots when return_ggplot = FALSE or NAs are present", {
  set.seed(7)
  mat <- matrix(rnorm(40 * 10), nrow = 40, ncol = 10)
  colnames(mat) <- paste0("F", 1:10)
  rownames(mat) <- sprintf("S_%02d", 1:40)
  df <- as.data.frame(mat)

  res_no_plot <- outlier_pca_lof(df, return_ggplot = FALSE, verbose = FALSE)
  expect_null(res_no_plot$plot_samples_outlier)
  expect_null(res_no_plot$plot_samples_outlier_pc1_pc2)

  df_na <- df
  df_na[1, 1] <- NA
  res_na <- outlier_pca_lof(df_na, return_ggplot = TRUE, verbose = FALSE)
  expect_null(res_na$plot_samples_outlier)
  expect_null(res_na$plot_samples_outlier_pc1_pc2)
  expect_null(res_na$id_samples_outlier)
})

test_that("outlier_pca_lof works with fewer than 10 features (>= 2) when return_ggplot = TRUE", {
  set.seed(99)
  mat <- matrix(rnorm(40 * 5), nrow = 40, ncol = 5)
  colnames(mat) <- paste0("M", 1:5)
  rownames(mat) <- sprintf("S_%02d", 1:40)
  mat[10, ] <- mat[10, ] + 7
  df <- as.data.frame(mat)

  res <- outlier_pca_lof(df, return_ggplot = TRUE, verbose = FALSE)
  expect_s3_class(res$plot_samples_outlier, "ggmatrix")
  expect_s3_class(res$plot_samples_outlier_pc1_pc2, "ggplot")
  expect_no_error(ggplot2::ggplot_build(res$plot_samples_outlier_pc1_pc2))
  expect_no_error(GGally::ggmatrix_gtable(res$plot_samples_outlier))
})

test_that("remove_outliers returns per-stratum plot_samples_outlier_pc1_pc2 when return_ggplots = TRUE", {
  set.seed(42)
  n <- 80
  p <- 10
  mat <- matrix(rnorm(n * p), nrow = n, ncol = p)
  colnames(mat) <- paste0("F", seq_len(p))
  df <- as.data.frame(mat)
  df$batch <- rep(c("B1", "B2"), each = n / 2)
  rownames(df) <- sprintf("S_%02d", seq_len(n))
  df[3, 1:p] <- df[3, 1:p] + 7

  res <- remove_outliers(
    df = df,
    target_cols = paste0("F", seq_len(p)),
    strata = "batch",
    impute_method = NULL,
    return_ggplots = TRUE
  )

  expect_true("plot_samples_outlier_pc1_pc2" %in% names(res))
  expect_named(res$plot_samples_outlier, c("B1", "B2"))
  expect_named(res$plot_samples_outlier_pc1_pc2, c("B1", "B2"))
  expect_s3_class(res$plot_samples_outlier$B1, "ggmatrix")
  expect_s3_class(res$plot_samples_outlier_pc1_pc2$B1, "ggplot")

  res_off <- remove_outliers(
    df = df,
    target_cols = paste0("F", seq_len(p)),
    strata = "batch",
    impute_method = NULL,
    return_ggplots = FALSE
  )
  expect_null(res_off$plot_samples_outlier)
  expect_null(res_off$plot_samples_outlier_pc1_pc2)
})

test_that("process_data returns plot_samples_outlier_pc1_pc2 when outlier = TRUE", {
  set.seed(42)
  n <- 50
  p <- 12
  mat <- matrix(rnorm(n * p), nrow = n, ncol = p)
  colnames(mat) <- paste0("F", seq_len(p))
  df <- cbind(data.frame(ID_sample = sprintf("S_%03d", seq_len(n))), as.data.frame(mat))
  df[5, -1] <- df[5, -1] + 6

  res <- process_data(
    data = df,
    col_samples = "ID_sample",
    outlier = TRUE
  )

  expect_true("plot_samples_outlier" %in% names(res))
  expect_true("plot_samples_outlier_pc1_pc2" %in% names(res))
  expect_s3_class(res$plot_samples_outlier, "ggmatrix")
  expect_s3_class(res$plot_samples_outlier_pc1_pc2, "ggplot")
})
