test_that("check_dataframe_validity() passes for valid input", {
  df <- data.frame(A = 1:3, B = c(2.5, 3.2, 4.1))
  expect_true(check_dataframe_validity(df))
})

test_that("Fails on NULL input", {
  expect_error(check_dataframe_validity(NULL), "Input is NULL")
})

test_that("Fails on non-data.frame input", {
  expect_error(check_dataframe_validity(matrix(1:9, 3, 3)), "not a data frame")
})

test_that("Fails on < 2 rows", {
  df <- data.frame(A = c(1))
  expect_error(check_dataframe_validity(df), "must have at least 2 rows.")
})

test_that("Fails on < 1 column", {
  df <- data.frame()
  expect_error(check_dataframe_validity(df), "at least 1 column")
})

test_that("Fails on non-numeric columns", {
  df <- data.frame(A = c(1, 2, 3), B = c("x", "y", "z"))
  expect_error(check_dataframe_validity(df), "All columns must be numeric")
})

test_that("Fails and identifies non-numeric columns by name", {
  df <- data.frame(A = c(1, 2, 3), B = c("x", "y", "z"), C = c(TRUE, FALSE, TRUE))
  expect_error(check_dataframe_validity(df), "Non-numeric columns: B, C")
})
