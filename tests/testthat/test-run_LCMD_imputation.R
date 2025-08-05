test_that("Imputation works with missing values", {
  set.seed(1)
  df <- data.frame(
    A = c(1, 2, NA, 4),
    B = c(10, NA, 30, 40)
  )
  result <- run_lcmd_imputation(df, target_cols = c("A", "B"))
  expect_s3_class(result, "data.frame")
  expect_equal(dim(result), dim(df))
  expect_false(any(is.na(result)))
})

test_that("Fails if only a few non-NA values or identical values", {
  df <- data.frame(A = c(1, 2), B = c(NA, 2))
  expect_error(
    run_lcmd_imputation(df, target_cols = c("A", "B")),
    "LCMD imputation failed"
  )
})

test_that("Fails if data frame has zero columns", {
  df <- data.frame(A = 1:5)
  expect_error(
    run_lcmd_imputation(df, target_cols = character(0)),
    "at least 1 column"
  )
})

test_that("Single column with missing data imputes successfully", {
  df <- data.frame(A = c(1, 2, NA, 4))
  result <- run_lcmd_imputation(df, target_cols = "A")
  expect_equal(ncol(result), 1)
  expect_false(any(is.na(result)))
})

# test_that("Fails if all values are NA", {
#   df <- data.frame(
#     A = c(NA, NA, NA),
#     B = c(1, 2, 3)
#   )
# #   expect_error(
#     run_lcmd_imputation(df, target_cols = c("A", "B"))
#     # "LCMD imputation failed"
# #   )
# })

# test_that("Non-numeric columns cause imputation failure", {
#   df <- data.frame(
#     A = c(1, NA, 3),
#     B = c("x", "y", "z")
#   )
#   expect_error(
#     run_lcmd_imputation(df, target_cols = c("A", "B")),
#     "LCMD imputation failed"
#   )
# })

test_that("Invalid imputation method triggers error", {
  df <- data.frame(A = c(1, NA, 3), B = c(4, NA, 6))
  expect_error(
    run_lcmd_imputation(df, c("A", "B"), control_LCMD = list(method.MAR = "InvalidMethod")),
    "LCMD imputation failed"
  )
})

test_that("Row names are preserved after imputation", {
  df <- data.frame(
    A = c(1, NA, 3),
    B = c(4, 5, NA),
    row.names = c("sample1", "sample2", "sample3")
  )
  result <- run_lcmd_imputation(df, target_cols = c("A", "B"))
  expect_equal(rownames(result), rownames(df))
})
