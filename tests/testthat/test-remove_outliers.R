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

test_that("remove_outliers works with explicit column names", {
  data <- generate_test_df(n = 40, na_frac = 0)
  df <- data$df # extract actual data frame

  is_qc <- rep(FALSE, nrow(df))
  is_qc[35:40] <- TRUE # 6 QC samples

  result <- remove_outliers(
    df,
    target_cols = c("A", "B", "C", "D"),
    is_qc = is_qc,
    method = "pca-lof-overall",
    impute_method = "half-min-value",
    restore_missing_values = TRUE
  )

  expect_s3_class(result$df_filtered, "data.frame")
  expect_true("plot_samples_outlier" %in% names(result))
  expect_true("excluded_ids" %in% names(result))
  expect_true(any(is_qc[rownames(result$df_filtered) %in% rownames(df)]))
})

test_that("remove_outliers works with regex column selection", {
  data <- generate_test_df(n = 40, na_frac = 0)
  df <- data$df # extract actual data frame

  names(df)[1:2] <- c("metab_1", "metab_2")
  is_qc <- rep(FALSE, nrow(df))
  is_qc[35:40] <- TRUE

  df[1, ] <- 4
  result <- remove_outliers(
    df,
    target_cols = "^metab_",
    is_qc = is_qc,
    method = "pca-lof-overall",
    impute_method = "half-min-value",
    restore_missing_values = FALSE
  )
  # print(result$df_filtered)
  expect_equal(ncol(result$df_filtered), ncol(df))
  expect_true(nrow(result$df_filtered) == (nrow(df) - 1))
})

test_that("remove_outliers works with all columns", {
  data <- generate_test_df(n = 40, na_frac = 0)
  df <- data$df[, -7] # extract actual data frame
  is_qc <- rep(FALSE, nrow(df))
  is_qc[35:40] <- TRUE

  df[1, ] <- 4
  df[5, ] <- -4
  result <- remove_outliers(
    df,
    target_cols = NULL,
    is_qc = is_qc,
    method = "pca-lof-overall",
    impute_method = "half-min-value",
    restore_missing_values = TRUE
  )

  expect_true(all(colnames(result$df_filtered) == colnames(df)))
  expect_true(nrow(result$df_filtered) < nrow(df))
})

test_that("remove_outliers works without is_qc argument", {
  data <- generate_test_df(n = 40, na_frac = 0.2)
  df <- data$df
  df[5, ] <- 4
  

  result <- remove_outliers(
    df = df,
    target_cols = c("A", "B"),
    method = "pca-lof-overall",
    impute_method = "half-min-value"
  )

  expect_true(nrow(result$df_filtered) == (nrow(df) - 1))
})

test_that("remove_outliers works without imputation", {
  data <- generate_test_df(n = 40, na_frac = 0)
  df <- data$df
  df[5, ] <- 4

  is_qc <- rep(FALSE, nrow(df))
  is_qc[35:40] <- TRUE

  result <- remove_outliers(
    df = df,
    target_cols = c("A", "B"),
    is_qc = is_qc,
    impute_method = NULL,
    method = "pca-lof-overall",
    restore_missing_values = FALSE
  )

  expect_true(nrow(result$df_filtered) < nrow(df))
})

test_that("remove_outliers restores missing values if requested", {
  data <- generate_test_df(n = 40, na_frac = 0.0)
  df <- data$df
  df[5, "A"] <- NA
  df[3, "B"] <- NA
  
  result <- remove_outliers(
    df = df,
    target_cols = c("A", "B"),
    method = "pca-lof-overall",
    impute_method = "half-min-value",
    restore_missing_values = TRUE
  )

  expect_true(is.na(result$df_filtered$A[5]))
  expect_true(is.na(result$df_filtered$B[3]))
  
})

test_that("remove_outliers restores missing values if requested + NAs in qc", {
  data <- generate_test_df(n = 40, na_frac = 0.0)
  df <- data$df
  df[5, "A"] <- NA
  df[3, "B"] <- NA
  df[37, "B"] <- NA

  is_qc <- rep(FALSE, nrow(df))
  is_qc[35:40] <- TRUE
  
  result <- remove_outliers(
    df = df,
    target_cols = c("A", "B"),
    is_qc = is_qc,
    method = "pca-lof-overall",
    impute_method = "half-min-value",
    restore_missing_values = TRUE
  )

  expect_true(is.na(result$df_filtered$A[5]))
  expect_true(is.na(result$df_filtered$B[3]))
  expect_true(is.na(result$df_filtered$B[37]))
  
})

test_that("remove_outliers throws error on invalid column", {
  data <- generate_test_df()
  df <- data$df

  expect_error(
    remove_outliers(df, target_cols = c("X", "Y"), is_qc = rep(FALSE, nrow(df))),
    "not in the dataframe"
  )
})

test_that("remove_outliers throws error if is_qc length is invalid", {
  data <- generate_test_df()
  df <- data$df

  expect_error(
    remove_outliers(df, target_cols = "A", is_qc = c(TRUE, FALSE)),
    "`is_qc` must be a logical vector"
  )
})

test_that("remove_outliers preserves original row order", {
  data <- generate_test_df()
  df <- data$df
  
  rownames(df) <- paste0("id_", seq_len(nrow(df)))

  result <- remove_outliers(
    df,
    target_cols = c("A", "B"),
    is_qc = rep(FALSE, nrow(df)),
    method = "pca-lof-overall",
    impute_method = "half-min-value"
  )

  expect_equal(rownames(result$df_filtered), rownames(df))
})
