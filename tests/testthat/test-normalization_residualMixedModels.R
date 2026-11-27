make_mixed_model_fixture <- function(n = 60) {
  set.seed(42)
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
    Name = c("f1", "f2"),
    stringsAsFactors = FALSE
  )
  list(
    data_features = df_feat,
    data_samples = df_meta_s,
    data_meta_features = df_meta_f
  )
}

test_that("normalization_residualMixedModels succeeds with a single fixed effect (lme4)", {
  inp <- make_mixed_model_fixture()

  out <- normalization_residualMixedModels(
    list = inp,
    identifier = "ID_sample",
    listRandom = "batch",
    listFixedToKeep = "age",
    listFixedToRemove = NULL,
    HeteroSked = NULL
  )

  expect_equal(nrow(out), nrow(inp$data_features))
  expect_equal(names(out), names(inp$data_features))
  expect_false(anyNA(out$f1))
  expect_false(anyNA(out$f2))
})

test_that("normalization_residualMixedModels succeeds with a single fixed effect (nlme HeteroSked)", {
  inp <- make_mixed_model_fixture()

  out <- normalization_residualMixedModels(
    list = inp,
    identifier = "ID_sample",
    listRandom = "batch",
    listFixedToKeep = "age",
    listFixedToRemove = NULL,
    HeteroSked = "batch"
  )

  expect_equal(nrow(out), nrow(inp$data_features))
  expect_false(anyNA(out$f1))
  expect_false(anyNA(out$f2))
})

test_that("normalization_residualMixedModels prints VIF when >= 2 fixed effects are supplied", {
  inp <- make_mixed_model_fixture()

  printed <- capture.output(
    out <- normalization_residualMixedModels(
      list = inp,
      identifier = "ID_sample",
      listRandom = "batch",
      listFixedToKeep = "age",
      listFixedToRemove = "sex",
      HeteroSked = NULL
    )
  )

  expect_true(any(grepl("Collinearity \\(VIF\\)", printed)))
  expect_equal(nrow(out), nrow(inp$data_features))
})
