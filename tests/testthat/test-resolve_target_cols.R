test_that("returns all columns when target_cols is NULL", {
  df <- data.frame(a = 1, b = 2, c = 3)
  result <- resolve_target_cols(df, NULL)
  expect_equal(result, names(df))
})

test_that("returns exact column names when specified explicitly", {
  df <- data.frame(x = 1, y = 2, z = 3)
  result <- resolve_target_cols(df, c("x", "z"))
  expect_equal(result, c("x", "z"))
})

test_that("throws error for missing column names", {
  df <- data.frame(a = 1, b = 2)
  expect_error(resolve_target_cols(df, c("a", "c")), 
               "The following `target_cols` are not in the dataframe: c")
})

test_that("resolves columns via regex", {
  df <- data.frame(score_A = 1, score_B = 2, value = 3)
  result <- resolve_target_cols(df, "^score")
  expect_equal(result, c("score_A", "score_B"))
})

test_that("throws error when regex does not match any columns", {
  df <- data.frame(x = 1, y = 2)
  expect_error(resolve_target_cols(df, "^not_present"), 
               "No columns matched the regular expression in `target_cols`")
})

test_that("regex is not applied if pattern characters are not present", {
  df <- data.frame(a1 = 1, a2 = 2)
  result <- resolve_target_cols(df, c("a1"))
  expect_equal(result, "a1")
})

test_that("empty data frame with NULL returns empty character vector", {
  df <- data.frame()
  result <- resolve_target_cols(df, NULL)
  expect_equal(result, character(0))
})

test_that("empty data frame with regex returns error", {
  df <- data.frame()
  expect_error(resolve_target_cols(df, "^x"), 
               "No columns matched the regular expression in `target_cols`")
})

test_that("duplicate column names are handled correctly", {
  df <- data.frame(a = 1, b = 2)
  names(df)[2] <- "a"  # duplicate column name
  result <- resolve_target_cols(df, "a")
  expect_equal(result, c("a","a"))
})
