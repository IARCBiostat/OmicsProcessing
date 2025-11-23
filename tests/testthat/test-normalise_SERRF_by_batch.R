test_that("normalise_SERRF_by_batch handles valid input", {
  set.seed(123)
  df <- data.frame(
    Feature1 = rnorm(12),
    Feature2 = rnorm(12),
    Feature3 = rnorm(12),
    Samplename = paste0("Sample", 1:12),
    batch = factor(rep(c("A", "B"), each = 6))
  )
  is_qc <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)

  result <- normalise_SERRF_by_batch(
    df = df,
    target_cols = c("Feature1", "Feature2", "Feature3"),
    is_qc = is_qc,
    strata_col = "batch",
    num_screen_SERRF = 2
  )

  expect_equal(dim(result), dim(df))
  expect_true(all(c("Feature1", "Feature2", "Feature3") %in% colnames(result)))
  expect_false(any(is.na(result[, c("Feature1", "Feature2", "Feature3")])))
})

test_that("normalise_SERRF_by_batch errors on NA in target features", {
  df <- data.frame(
    Feature1 = c(1, NA, 3, 4),
    batch = factor(c("A", "A", "B", "B"))
  )
  expect_error(
    normalise_SERRF_by_batch(df, target_cols = "Feature1", is_qc = c(TRUE, TRUE, FALSE, FALSE), strata_col = "batch"),
    "contain NA values"
  )
})

test_that("normalise_SERRF_by_batch errors on non-numeric target features", {
  df <- data.frame(
    Feature1 = as.character(c(1, 2, 3, 4)),
    batch = factor(c("A", "A", "B", "B"))
  )
  expect_error(
    normalise_SERRF_by_batch(df, target_cols = "Feature1", is_qc = c(TRUE, TRUE, FALSE, FALSE), strata_col = "batch"),
    "must be numeric"
  )
})

test_that("normalise_SERRF_by_batch normalises synthetic data with drift", {
  set.seed(42)

  make_test_df <- function() {
    n_per_batch <- 10  # 6 samples + 4 QCs per batch
    batches <- c("Batch1", "Batch2")

    df <- do.call(rbind, lapply(batches, function(b) {
      qc <- data.frame(
        Feature1 = rnorm(4, mean = 10),
        Feature2 = rnorm(4, mean = 50),
        Feature3 = rnorm(4, mean = 100),
        Samplename = paste0("QC_", seq_len(4), "_", b),
        batch = b,
        is_qc = TRUE
      )

      drift <- seq(0, 1, length.out = 6)
      sample <- data.frame(
        Feature1 = rnorm(6, mean = 10) + drift * 3,
        Feature2 = rnorm(6, mean = 50) + drift * 5,
        Feature3 = rnorm(6, mean = 100) + drift * 10,
        Samplename = paste0("Sample_", seq_len(6), "_", b),
        batch = b,
        is_qc = FALSE
      )

      rbind(qc, sample)
    }))

    df$batch <- factor(df$batch)
    df <- df[order(df$batch, df$is_qc), ]
    rownames(df) <- NULL
    return(df)
  }

  df_test <- make_test_df()

  result <- normalise_SERRF_by_batch(
    df = df_test,
    target_cols = c("Feature1", "Feature2", "Feature3"),
    is_qc = df_test$is_qc,
    strata_col = "batch",
    num_screen_SERRF = 2
  )

  expect_equal(dim(result), dim(df_test))
  expect_true(all(c("Feature1", "Feature2", "Feature3") %in% colnames(result)))
  expect_false(any(is.na(result[, c("Feature1", "Feature2", "Feature3")])))
  expect_equal(unique(result$batch), unique(df_test$batch))
})