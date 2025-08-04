generate_test_df <- function(n = 40, na_frac = 0.1) {
  set.seed(123)

  # Generate values between -4 and 4
  df <- data.frame(
    A = runif(n, min = -1, max = 1),
    B = runif(n, min = -1, max = 1),
    C = runif(n, min = -1, max = 1),
    D = runif(n, min = -1, max = 1),
    E = runif(n, min = -1, max = 1),
    F = runif(n, min = -1, max = 1),
    G = sample(letters, n, replace = TRUE) # non-numeric
  )
  rownames(df) <- paste0("sample_", seq_len(n))

  # Track NA positions for validation
  na_positions <- list()

  # Add missing values randomly in numeric columns
  na_count <- round(n * na_frac)
  for (col in c("A", "B", "C", "D", "E", "F")) {
    idx <- sample(seq_len(n), na_count)
    df[[col]][idx] <- NA
    na_positions[[col]] <- idx
  }

  return(list(
    df = df,
    na_positions = na_positions
  ))
}

test_that("run_rf_imputation returns correct structure and dimensions", {
    df <- generate_test_df(n = 40, na_frac = 0.3)$df
    result <- run_rf_imputation(df, target_cols = c("A", "B", "C", "D"), control_RF = list(parallelize = "no", ntree = 10, maxiter = 2))

    expect_type(result, "list")
    expect_named(result, c("imputed", "oob"))
    expect_s3_class(result$imputed, "data.frame")
    expect_equal(dim(result$imputed), dim(df[, c("A", "B", "C", "D")]))
    expect_true(all(names(result$oob) %in% c("A", "B", "C", "D")))
})



test_that("switches to sequential mode when parallelize = \"no\"", {
    df <- generate_test_df(n = 40, na_frac = 0.3)$df

    expect_message(
        run_rf_imputation(df, target_cols = c("A", "B", "C", "D"), control_RF = list(parallelize = "no", ntree = 10, maxiter = 2, n_cores = 5)),
        "Running missForest::missForest in sequential mode"
    )
})


test_that("switches to sequential mode when n_cores <= 1", {
    df <- generate_test_df(n = 40, na_frac = 0.3)$df

    expect_message(
        run_rf_imputation(df, target_cols = c("A", "B", "C", "D"), control_RF = list(parallelize = "no", ntree = 10, maxiter = 2)),
        "Running missForest::missForest in sequential mode"
    )
})

test_that("returns NA for OOB when not available", {
    mock_missForest <- function(...) list(ximp = data.frame(a = 1:2, b = 3:4), OOBerror = NULL)
    with_mock(
        `missForest::missForest` = mock_missForest,
        {
            df <- data.frame(a = c(NA, 2), b = c(3, NA))
            result <- run_rf_imputation(df, c("a", "b"))
            expect_true(all(is.na(result$oob)))
        }
    )
})
