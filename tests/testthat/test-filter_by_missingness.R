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
  out <- filter_by_missingness(df) # Uses all columns and all samples
  expect_equal(ncol(out), 0)
})

test_that("error is thrown for invalid is_qc length", {
  df <- data.frame(A = 1:3)
  expect_error(
    filter_by_missingness(df, is_qc = c(TRUE, FALSE)),
    "must be a logical vector with the same length"
  )
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
  expect_equal(names(out), "C") # only non-target column remains
})

test_that("all rows removed if all exceed row_thresh", {
  df <- data.frame(A = c(NA, NA), B = c(NA, NA))
  out <- filter_by_missingness(df, row_thresh = 0.1)
  expect_true(ncol(out) == 0 || nrow(out) == 0)
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
    A = c(1, NA), # 50% missing
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
  expect_equal(names(out), character(0)) # All columns filtered
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
  expect_true(all(diff(out$ID) > 0)) # rows should be sorted as in original
})

test_that("default filter_order is 'simultaneous'", {
  df <- data.frame(A = c(1, NA, NA), B = c(NA, NA, 3))
  expect_silent(filter_by_missingness(df)) # should default to simultaneous
})

test_that("filter_order = 'simultaneous' matches expected behavior", {
  df <- data.frame(
    A = c(1, NA, NA),
    B = c(NA, NA, 3),
    C = 1:3
  )
  result <- filter_by_missingness(df, target_cols = c("A", "B"), filter_order = "simultaneous")
  expect_equal(ncol(result), 1) # C should always be kept
  expect_equal(nrow(result), 2) # Row with 100% NA in A+B should be removed
})

test_that("filter_order = 'col_then_row' removes cols first then rows", {
  df <- data.frame(
    A = c(NA, NA, NA), # 100% missing
    B = c(1, NA, NA), # 66% missing
    C = 1:3 # metadata
  )
  res <- filter_by_missingness(
    df,
    target_cols = c("A", "B"),
    col_thresh = 0.7,
    row_thresh = 0.5,
    filter_order = "col_then_row"
  )
  expect_false("A" %in% names(res)) # column A removed
  expect_true("B" %in% names(res)) # column B retained
  expect_equal(ncol(res), 2) # one row removed
})

test_that("filter_order = 'row_then_col' removes rows first then columns", {
  df <- data.frame(
    A = c(NA, NA, 1), # keep row 3
    B = c(1, NA, NA), # retain if not too missing\
    meta = 1:3
  )
  res <- filter_by_missingness(
    df,
    target_cols = c("A", "B"),
    col_thresh = 0.5,
    row_thresh = 0.5,
    filter_order = "row_then_col"
  )
  expect_equal(nrow(res), 2) # one row removed
  expect_true("meta" %in% names(res)) # always retained
  expect_true("B" %in% names(res)) # column B: kept or dropped based on row removal
})

test_that("invalid filter_order triggers error", {
  df <- data.frame(A = c(1, NA))
  expect_error(
    filter_by_missingness(df, filter_order = "wrong"),
    "should be one of"
  )
})


test_that("iterative mode returns stable result equal to simultaneous on simple data", {
  df <- data.frame(
    a = c(1, NA, 3, 4),
    b = c(NA, 2, 3, NA),
    c = c(1, 2, NA, 4)
  )
  is_qc <- c(FALSE, FALSE, FALSE, FALSE)
  target <- c("a", "b", "c")

  sim <- filter_by_missingness(df, 0.5, 0.5, target_cols = target, is_qc = is_qc, filter_order = "simultaneous")
  iter <- filter_by_missingness(df, 0.5, 0.5, target_cols = target, is_qc = is_qc, filter_order = "iterative", max_iter = 10)

  expect_identical(sim, iter)
})

test_that("iterative mode converges within max_iter and keeps QC rows", {
  df <- data.frame(
    a = c(NA, 1, NA, 1, NA),
    b = c(NA, NA, 2, 2, NA),
    c = c(3, NA, NA, 3, NA),
    d = c(1, 1, 1, 1, 1) # non-target
  )
  is_qc <- c(FALSE, TRUE, FALSE, FALSE, TRUE)
  target <- c("a", "b", "c")
  out <- filter_by_missingness(df,
    row_thresh = 0.5, col_thresh = 0.5,
    target_cols = target, is_qc = is_qc,
    filter_order = "iterative", max_iter = 10
  )

  expect_true("d" %in% names(out))
  expect_true(all(df[is_qc, "d"] %in% out$d))
})

test_that("iterative mode stops at max_iter", {
  # Craft a dataset that would oscillate without the second pass reinsertion (defensive)
  df <- data.frame(
    a = c(NA, 1, NA, 1),
    b = c(1, NA, 1, NA)
  )
  is_qc <- c(FALSE, FALSE, FALSE, FALSE)
  target <- names(df)
  # This should converge quickly, but we assert it doesn't error at small max_iter
  expect_silent(
    filter_by_missingness(df, 0.5, 0.5,
      target_cols = target, is_qc = is_qc,
      filter_order = "iterative", max_iter = 2
    )
  )
})


# tests/test-filter-by-missingness-example.R

test_that("example from docs produces the expected outputs for each filter_order", {
  # Example dataset
  df <- data.frame(
    a = c(NA, 1, NA, 1, NA),
    b = c(NA, NA, 2, 2, NA),
    c = c(3, NA, NA, 3, NA),
    d = c(1, 1, 1, 1, 1) # non-target
  )
  is_qc <- c(FALSE, TRUE, FALSE, FALSE, TRUE)
  target_cols <- c("a", "b", "c")
  row_thresh <- 0.5
  col_thresh <- 0.5

  # Run all modes
  out_iter <- filter_by_missingness(
    df, row_thresh = row_thresh, col_thresh = col_thresh,
    target_cols = target_cols, is_qc = is_qc, filter_order = "iterative"
  )
  print(out_iter)
  out_simul <- filter_by_missingness(
    df, row_thresh = row_thresh, col_thresh = col_thresh,
    target_cols = target_cols, is_qc = is_qc, filter_order = "simultaneous"
  )
  out_ctr <- filter_by_missingness(
    df, row_thresh = row_thresh, col_thresh = col_thresh,
    target_cols = target_cols, is_qc = is_qc, filter_order = "col_then_row"
  )
  out_rtc <- filter_by_missingness(
    df, row_thresh = row_thresh, col_thresh = col_thresh,
    target_cols = target_cols, is_qc = is_qc, filter_order = "row_then_col"
  )

  # Expected outputs (constructed exactly as in the printed example)
  # "iteration" -> keep all rows, drop 'a'
  exp_iter <- df[, c("b", "c", "d")]

  # "simultaneous" -> keep rows 2,4,5 and columns b,c,d
  exp_simul <- df[c(2, 4, 5), c("b", "c", "d")]

  # "col_then_row" -> keep all rows, drop 'a'
  exp_ctr <- df[, c("b", "c", "d")]

  # "row_then_col" -> keep rows 2,4,5 and all columns (a,b,c,d)
  exp_rtc <- df[c(2, 4, 5), c("a", "b", "c", "d")]

  # Compare (ensure identical row names and column orders)
  expect_identical(out_iter, exp_iter)
  expect_identical(out_simul, exp_simul)
  expect_identical(out_ctr, exp_ctr)
  expect_identical(out_rtc, exp_rtc)
})
