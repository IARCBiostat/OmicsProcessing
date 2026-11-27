test_that("process_data(plate_correction = TRUE) auto-builds feature metadata when omitted", {
  set.seed(42)
  n <- 60
  batch <- factor(rep(paste0("B", 1:4), each = n / 4))
  age <- rnorm(n, mean = 55, sd = 10)
  sex <- factor(sample(c("F", "M"), n, replace = TRUE))
  b_eff <- c(-1.0, -0.3, 0.4, 1.1)[as.integer(batch)]

  df_feat <- data.frame(
    ID_sample = paste0("S", seq_len(n)),
    f1 = 10 + 0.05 * age + b_eff + rnorm(n, 0, 0.5),
    f2 = 20 - 0.04 * age + 0.8 * b_eff + rnorm(n, 0, 0.5),
    stringsAsFactors = FALSE
  )
  df_meta_s <- data.frame(
    ID_sample = df_feat$ID_sample,
    batch = batch,
    age = age,
    sex = sex,
    stringsAsFactors = FALSE
  )
  df_meta_f <- data.frame(
    ID_feature = c("f1", "f2"),
    stringsAsFactors = FALSE
  )

  res_auto <- process_data(
    data = df_feat,
    data_meta_samples = df_meta_s,
    col_samples = "ID_sample",
    plate_correction = TRUE,
    cols_listRandom = "batch",
    cols_listFixedToKeep = "age",
    cols_listFixedToRemove = "sex"
  )

  res_explicit <- process_data(
    data = df_feat,
    data_meta_samples = df_meta_s,
    data_meta_features = df_meta_f,
    col_samples = "ID_sample",
    col_features = "ID_feature",
    plate_correction = TRUE,
    cols_listRandom = "batch",
    cols_listFixedToKeep = "age",
    cols_listFixedToRemove = "sex"
  )

  expect_equal(res_auto$df_features, res_explicit$df_features)
  expect_false(anyNA(res_auto$df_features))
})

test_that("process_data(plate_correction = TRUE) errors cleanly when data_meta_samples is missing", {
  df_feat <- data.frame(ID_sample = c("S1", "S2"), f1 = c(1, 2))
  expect_error(
    process_data(
      data = df_feat,
      col_samples = "ID_sample",
      plate_correction = TRUE,
      cols_listRandom = "batch",
      cols_listFixedToKeep = "age"
    ),
    "data_meta_samples"
  )
})
