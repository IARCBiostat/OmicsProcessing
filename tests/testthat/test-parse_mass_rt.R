test_that("returns masses as numeric vector", {
  expect_equal(
    parse_mass_rt("100.5@23.1", what = "mass"),
    100.5
  )
})

test_that("returns retention times and strips suffixes by default", {
  rts <- parse_mass_rt(
    c("10.2@1.5:1", "20.4@2.5:2"),
    what = "rt"
  )

  expect_equal(rts, c(1.5, 2.5))
})

test_that("returns both mass and rt as data.frame", {
  res <- parse_mass_rt("50@7.5", what = "both")

  expect_s3_class(res, "data.frame")
  expect_equal(res$mass, 50)
  expect_equal(res$rt, 7.5)
})

test_that("missing rt part yields NA", {
  rts <- parse_mass_rt(
    c("10.2@1.5", "20.4"),
    what = "rt"
  )

  expect_equal(rts, c(1.5, NA_real_))
})

test_that("suffix is preserved when clean_rt_suffix is FALSE", {
  rts <- parse_mass_rt(
    "1@2:1",
    what = "rt",
    clean_rt_suffix = FALSE
  )

  expect_true(is.na(rts))
})
