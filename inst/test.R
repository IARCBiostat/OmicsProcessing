rm(list=ls())

# exclusions ====
data_processed <- process_data(
  data = data_features,
  col_samples = "ID_sample",
  exclusion_extreme_feature = TRUE, missing_pct_feature = 0.1,
  exclusion_extreme_sample = TRUE, missing_pct_sample = 0.1)

# imputation ====
data_processed <- process_data(
  data = data_features,
  data_meta_features = data_meta_features, col_features = "ID_feature", col_LOD = "LOD",
  col_samples = "ID_sample",
  imputation = TRUE, imputation_method = "LOD")

# transformation ====
data_processed <- process_data(
  data = data_features,
  col_samples = "ID_sample",
  transformation = TRUE, transformation_method = "Log10")

# outlier PCA/LOF ====
data_processed <- process_data(
  data = data_features,
  col_samples = "ID_sample",
  exclusion_extreme_feature = TRUE, missing_pct_feature = 0.1,
  exclusion_extreme_sample = TRUE, missing_pct_sample = 0.1,
  imputation = TRUE, imputation_method = "mean",
  outlier = TRUE)

data_features_outlier <- data_features %>%
  dplyr::mutate(dplyr::across(2:101, ~ ifelse(dplyr::row_number() == 10, . * 1.5, .)))
data_processed <- process_data(
  data = data_features_outlier,
  col_samples = "ID_sample",
  exclusion_extreme_feature = TRUE, missing_pct_feature = 0.1,
  exclusion_extreme_sample = TRUE, missing_pct_sample = 0.1,
  imputation = TRUE, imputation_method = "mean",
  outlier = TRUE)

# plate correction ====
data_processed <- process_data(
  data = data_features,
  data_meta_samples = data_meta_samples,
  col_samples = "ID_sample",
  data_meta_features = data_meta_features,
  col_features = "ID_feature",
  plate_correction = TRUE,
  cols_listRandom = c("batch"),
  cols_listFixedToKeep = c("age"),
  cols_listFixedToRemove = c("sex"),
  col_HeteroSked = NULL)

# center/scale ====
data_processed <- process_data(
  data = data_features,
  col_samples = "ID_sample",
  centre_scale = TRUE)

# save ====
data_features_outlier <- data_features %>%
  dplyr::mutate(dplyr::across(2:101, ~ ifelse(dplyr::row_number() == 10, . * 4, .)))
data_processed <- process_data(
  data = data_features_outlier,
  data_meta_samples = data_meta_samples,
  col_samples = "ID_sample",
  data_meta_features = data_meta_features,
  col_features = "ID_feature",
  exclusion_extreme_feature = TRUE, missing_pct_feature = 0.1,
  exclusion_extreme_sample = TRUE, missing_pct_sample = 0.1,
  imputation = TRUE, imputation_method = "mean",
  transformation = TRUE, transformation_method = "Log10",
  outlier = TRUE,
  plate_correction = TRUE, cols_listRandom = c("batch"), cols_listFixedToKeep = c("age"), cols_listFixedToRemove = c("sex"), col_HeteroSked = NULL,
  save = TRUE, path_out = "inst/", path_outliers = "inst/")

