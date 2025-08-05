test_that("keeps only valid arguments", {
  test_fun <- function(x, y = 1, z = 2) NULL
  input <- list(x = 10, y = 20, a = 99)
  result <- filter_function_args(input, test_fun, warn = FALSE)

  expect_named(result, c("x", "y"))
  expect_false("a" %in% names(result))
})

test_that("returns empty list when all arguments are invalid", {
  test_fun <- function(a = 1) NULL
  input <- list(x = 10, y = 20)
  result <- filter_function_args(input, test_fun, warn = FALSE)

  expect_equal(length(result), 0)
})

test_that("returns all arguments when they are valid", {
  test_fun <- function(x, y = 1) NULL
  input <- list(x = 5, y = 10)
  result <- filter_function_args(input, test_fun, warn = FALSE)

  expect_equal(result, input)
})

test_that("emits warning when invalid arguments are found", {
  test_fun <- function(x, y) NULL
  input <- list(x = 1, bogus = 999)

  expect_warning(
    filter_function_args(input, test_fun),
    "The following arguments are not accepted"
  )
})

test_that("does not emit warning when warn = FALSE", {
  test_fun <- function(x) NULL
  input <- list(x = 1, not_allowed = 2)

  expect_silent(
    filter_function_args(input, test_fun, warn = FALSE)
  )
})

test_that("works when function has no formals", {
  test_fun <- function() NULL
  input <- list(a = 1, b = 2)

  result <- filter_function_args(input, test_fun, warn = FALSE)
  expect_equal(length(result), 0)
})

test_that("errors if fun is not a function", {
  expect_error(
    filter_function_args(list(a = 1), "not_a_function"),
    "`fun` must be a function or function name"
  )
})
