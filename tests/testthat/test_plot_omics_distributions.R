testthat::test_that("prepare_df_long reshapes data and standardises columns", {
  df <- data.frame(
    injection_order = c(2, 1, 3),
    qc_flag = c(TRUE, FALSE, TRUE),
    batch_id = c("b1", "b1", "b2"),
    plate_id = c("p1", "p2", "p1"),
    feat1 = c(10, 20, 30),
    feat2 = c(100, 200, 300)
  )

  out <- prepare_df_long(
    df = df,
    target_cols = c("feat1", "feat2"),
    run_order = "injection_order",
    is_qc = "qc_flag",
    batch = "batch_id",
    plate = "plate_id"
  )
  
  testthat::expect_s3_class(out, "data.frame")
  testthat::expect_equal(
    sort(names(out)),
    sort(c(
      "injection_order", "qc_flag", "batch_id", "plate_id",
      "feature", "value", "run_order", "is_qc", "batch", "plate"
    ))
  )
  testthat::expect_equal(nrow(out), 6)
  testthat::expect_true(is.numeric(out$run_order))
  testthat::expect_true(is.logical(out$is_qc))
  testthat::expect_s3_class(out$batch, "factor")
  testthat::expect_s3_class(out$plate, "factor")
  testthat::expect_equal(out$run_order, c(1, 1, 2, 2, 3, 3))
  testthat::expect_setequal(unique(out$feature), c("feat1", "feat2"))
})

testthat::test_that("prepare_df_long uses defaults when qc, batch, and plate are NULL", {
  df <- data.frame(
    injection_order = c(2, 1),
    feat1 = c(10, 20)
  )

  out <- prepare_df_long(
    df = df,
    target_cols = "feat1",
    run_order = "injection_order",
    is_qc = NULL,
    batch = NULL,
    plate = NULL
  )

  testthat::expect_equal(nrow(out), 2)
  testthat::expect_identical(out$is_qc, c(FALSE, FALSE))
  testthat::expect_equal(levels(out$batch), "all")
  testthat::expect_equal(levels(out$plate), "all")
  testthat::expect_equal(out$run_order, c(1, 2))
})

testthat::test_that("plot_scatter_omics_feature returns a ggplot object", {
  df_long <- data.frame(
    run_order = c(1, 2, 3, 4),
    value = c(10, 11, 20, 21),
    feature = c("feat1", "feat1", "feat2", "feat2"),
    is_qc = c(TRUE, FALSE, TRUE, FALSE),
    batch = factor(c("b1", "b1", "b2", "b2")),
    plate = factor(c("p1", "p1", "p2", "p2"))
  )

  p <- plot_scatter_omics_feature(
    df_long = df_long,
    title = "Test plot",
    batch = "batch",
    point_size = 1
  )

  testthat::expect_s3_class(p, "ggplot")
})

testthat::test_that("plot_scatter_omics_feature adds batch boundary lines when batch is given", {
  df_long <- data.frame(
    run_order = c(1, 2, 3, 4),
    value = c(10, 11, 20, 21),
    feature = c("feat1", "feat1", "feat2", "feat2"),
    is_qc = c(TRUE, FALSE, TRUE, FALSE),
    batch = factor(c("b1", "b1", "b2", "b2")),
    plate = factor(c("p1", "p1", "p2", "p2"))
  )

  p <- plot_scatter_omics_feature(
    df_long = df_long,
    batch = "batch"
  )

  layer_classes <- vapply(
    p$layers,
    function(x) class(x$geom)[1],
    character(1)
  )

  testthat::expect_true("GeomVline" %in% layer_classes)
})

testthat::test_that("plot_scatter_omics_feature does not add batch lines when batch is NULL", {
  df_long <- data.frame(
    run_order = c(1, 2),
    value = c(10, 20),
    feature = c("feat1", "feat1"),
    is_qc = c(TRUE, FALSE),
    batch = factor(c("b1", "b1")),
    plate = factor(c("p1", "p1"))
  )

  p <- plot_scatter_omics_feature(
    df_long = df_long,
    batch = NULL
  )

  layer_classes <- vapply(
    p$layers,
    function(x) class(x$geom)[1],
    character(1)
  )

  testthat::expect_false("GeomVline" %in% layer_classes)
})

testthat::test_that("plot_omics_distributions returns a single ggplot without comparison data", {
  df <- data.frame(
    injection_order = c(1, 2, 3),
    qc_flag = c(TRUE, FALSE, FALSE),
    batch_id = c("b1", "b1", "b2"),
    plate_id = c("p1", "p1", "p2"),
    feat1 = c(10, 20, 30)
  )

  p <- plot_omics_distributions(
    df = df,
    target_cols = "feat1",
    run_order = "injection_order",
    is_qc = "qc_flag",
    batch = "batch_id",
    plate = "plate_id"
  )

  testthat::expect_s3_class(p, "ggplot")
})

testthat::test_that("plot_omics_distributions returns a patchwork object with comparison data", {
  df <- data.frame(
    injection_order = c(1, 2, 3),
    qc_flag = c(TRUE, FALSE, FALSE),
    batch_id = c("b1", "b1", "b2"),
    plate_id = c("p1", "p1", "p2"),
    feat1 = c(10, 20, 30)
  )

  df_comp <- data.frame(
    injection_order = c(1, 2, 3),
    qc_flag = c(TRUE, FALSE, FALSE),
    batch_id = c("b1", "b1", "b2"),
    plate_id = c("p1", "p1", "p2"),
    feat1 = c(11, 19, 31)
  )

  p <- plot_omics_distributions(
    df = df,
    df_comp = df_comp,
    target_cols = "feat1",
    run_order = "injection_order",
    is_qc = "qc_flag",
    batch = "batch_id",
    plate = "plate_id"
  )

  testthat::expect_true(inherits(p, "patchwork"))
})

testthat::test_that("plot_omics_distributions sets default titles when comparison is supplied", {
  df <- data.frame(
    injection_order = c(1, 2),
    qc_flag = c(TRUE, FALSE),
    batch_id = c("b1", "b1"),
    plate_id = c("p1", "p1"),
    feat1 = c(10, 20)
  )

  p <- plot_omics_distributions(
    df = df,
    df_comp = df,
    target_cols = "feat1",
    run_order = "injection_order",
    is_qc = "qc_flag",
    batch = "batch_id",
    plate = "plate_id"
  )

  testthat::expect_true(inherits(p, "patchwork"))
})
