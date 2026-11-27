test_that("process_data(centre_scale = TRUE) centres and scales numeric features", {
  set.seed(1)
  df_cs <- data.frame(
    ID_sample = paste0("S", 1:20),
    f1 = rnorm(20, mean = 100, sd = 15),
    f2 = rnorm(20, mean = 500, sd = 50)
  )

  res_scaled <- process_data(
    data = df_cs,
    col_samples = "ID_sample",
    centre_scale = TRUE
  )

  expect_equal(mean(res_scaled$df_features$f1), 0, tolerance = 1e-8)
  expect_equal(sd(res_scaled$df_features$f1), 1, tolerance = 1e-8)
  expect_equal(mean(res_scaled$df_features$f2), 0, tolerance = 1e-8)
  expect_equal(sd(res_scaled$df_features$f2), 1, tolerance = 1e-8)
  expect_equal(res_scaled$df_features$ID_sample, df_cs$ID_sample)

  res_unscaled <- process_data(
    data = df_cs,
    col_samples = "ID_sample",
    centre_scale = FALSE
  )
  expect_equal(res_unscaled$df_features$f1, df_cs$f1)
  expect_equal(res_unscaled$df_features$f2, df_cs$f2)
})
