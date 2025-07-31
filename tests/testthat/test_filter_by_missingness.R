test_that("function keeps all rows and columns when no missing values", {
  df <- data.frame(A = 1:5, B = 6:10)
  out <- filter_by_missingness(df)
  expect_equal(out, df)
})

test_that("columns with too much missingness are removed", {
  df <- data.frame(A = c(1, NA, NA, NA, NA), B = 1:5)
  out <- filter_by_missingness(df, col_thresh = 0.5, target_cols = "^A")
  expect_false("A" %in% names(out))
  expect_true("B" %in% names(out))
})

test_that("rows with too much missingness are removed", {
  df <- data.frame(A = c(1, NA, 1, NA), B = c(1, 1, NA, NA))
  out <- filter_by_missingness(df, row_thresh = 0.5, target_cols = "A|B")
  expect_equal(nrow(out), 3)
})

test_that("non-regex columns are always retained", {
  df <- data.frame(
    meta = 1:4,
    A = c(1, NA, NA, NA),
    B = c(1, 2, NA, NA)
  )
  out <- filter_by_missingness(df, col_thresh = 0.5, target_cols = "A|B")
  expect_true("meta" %in% names(out))
})

test_that("QC samples are excluded from missingness calculation but retained", {
  df <- data.frame(A = c(1, NA, NA, 2), B = c(1, 2, NA, 3))
  is_qc <- c(FALSE, FALSE, TRUE, TRUE)
  out <- filter_by_missingness(df, row_thresh = 0.5, target_cols = "A|B", is_qc = is_qc)
  
  expect_equal(nrow(out), 4)
})

test_that("default is_qc and regex_pattern behave correctly", {
  df <- data.frame(A = c(1, NA, NA), B = c(NA, 1, NA))
  out <- filter_by_missingness(df)  # Uses all columns and all samples
  expect_equal(ncol(out), 0)
})

test_that("error is thrown for invalid is_qc length", {
  df <- data.frame(A = 1:3)
  expect_error(filter_by_missingness(df, is_qc = c(TRUE, FALSE)), 
               "must be a logical vector with the same length")
})


test_that("target_cols handles both regex and explicit names", {
  df <- data.frame(A1 = c(1, NA), B2 = c(NA, 1), meta = 1:2)

  # Regex usage
  out1 <- filter_by_missingness(df, target_cols = "^A|B$")
  expect_true("meta" %in% names(out1))

  # Explicit usage
  out2 <- filter_by_missingness(df, target_cols = c("A1", "B2"))
  expect_true("meta" %in% names(out2))
})

test_that("all target columns removed if all exceed col_thresh", {
  df <- data.frame(A = c(NA, NA), B = c(NA, NA), C = 1:2)
  out <- filter_by_missingness(df, target_cols = c("A", "B"))
  expect_equal(names(out), "C")  # only non-target column remains
})

test_that("all rows removed if all exceed row_thresh", {
  df <- data.frame(A = c(NA, NA), B = c(NA, NA))
  out <- filter_by_missingness(df, row_thresh = 0.1)
  expect_equal(nrow(out), 0)
})

test_that("fully missing target columns removed", {
  df <- data.frame(A = c(NA, NA), B = c(1, 2), C = 1:2)
  out <- filter_by_missingness(df, target_cols = c("A", "B"))
  expect_false("A" %in% names(out))
})

test_that("fully complete data keeps all rows and columns", {
  df <- data.frame(A = 1:5, B = 6:10)
  out <- filter_by_missingness(df)
  expect_equal(out, df)
})

test_that("rows and columns exactly on threshold are retained", {
  df <- data.frame(
    A = c(1, NA),  # 50% missing
    B = c(1, 2)
  )
  out <- filter_by_missingness(df, row_thresh = 0.5, col_thresh = 0.5)
  expect_equal(nrow(out), 2)
  expect_equal(names(out), c("A", "B"))
})

test_that("single row behaves correctly", {
  df <- data.frame(A = NA, B = 1)
  out <- filter_by_missingness(df, row_thresh = 0.5, target_cols = c("A", "B"))
  expect_equal(nrow(out), 1)
  expect_equal(ncol(out), 1)
})

test_that("single column behaves correctly", {
  df <- data.frame(A = c(NA, 1, 1))
  out <- filter_by_missingness(df, target_cols = "A", col_thresh = 0.3)
  expect_equal(names(out), character(0))  # All columns filtered
})

test_that("empty dataframe returns empty result", {
  df <- data.frame()
  out <- filter_by_missingness(df)
  expect_equal(ncol(out), 0)
  expect_equal(nrow(out), 0)
})

test_that("invalid column names raise error", {
  df <- data.frame(A = 1:3)
  expect_error(
    filter_by_missingness(df, target_cols = c("X", "Y")),
    "not in the dataframe"
  )
})

test_that("regex that matches nothing raises error", {
  df <- data.frame(A = 1:3)
  expect_error(
    filter_by_missingness(df, target_cols = "^ZZ"),
    "No columns matched"
  )
})

test_that("QC rows retained even if fully missing", {
  df <- data.frame(A = c(NA, NA, 1), B = c(NA, 1, 1))
  is_qc <- c(TRUE, TRUE, FALSE)
  out <- filter_by_missingness(df, row_thresh = 0.3, target_cols = c("A", "B"), is_qc = is_qc)
  expect_equal(nrow(out), 3)
})

test_that("row order is preserved after filtering", {
  df <- data.frame(ID = 1:5, A = c(1, NA, NA, NA, NA))
  out <- filter_by_missingness(df, target_cols = "A", row_thresh = 0.25)
  expect_true(all(diff(out$ID) > 0))  # rows should be sorted as in original
})