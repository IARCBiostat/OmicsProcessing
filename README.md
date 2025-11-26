# OmicsProcessing

Pre-analysis processing for metabolomics and proteomics: missingness filtering, outlier handling, imputation, transformation, matched case-control handling, batch/plate correction, and SERRF-based normalisation across batches or strata. [Please visit out website for more information and vignettes](https://iarcbiostat.github.io/OmicsProcessing/index.html)

## Choose your workflow

### Semi-automated pipeline (`process_data()`)

- End-to-end wrapper that can filter on missingness, impute, transform, remove outliers (PCA + LOF), handle matched case-control designs, correct for plate/batch effects, and centre/scale.
- Takes three data frames (feature data, feature metadata, sample metadata) and returns processed data plus exclusion IDs and PCA/LOF plots.
- Full walk-through: [Semi-automated pipeline](https://iarcbiostat.github.io/OmicsProcessing/articles/process_data.html).

### Modular workflow (build your own)

- Compose individual steps to suit your study design. Typical sequence:
  - Filter by missingness with [`filter_by_missingness()`](https://iarcbiostat.github.io/OmicsProcessing/reference/filter_by_missingness.html) ([vignette](https://iarcbiostat.github.io/OmicsProcessing/articles/data-filtering.html))
  - Detect outlier samples with [`remove_outliers()`](https://iarcbiostat.github.io/OmicsProcessing/reference/remove_outliers.html) ([vignette](https://iarcbiostat.github.io/OmicsProcessing/articles/outlier-removal.html))
  - Impute with RF, LCMD, or both via [`hybrid_imputation()`](https://iarcbiostat.github.io/OmicsProcessing/reference/hybrid_imputation.html) ([vignette](https://iarcbiostat.github.io/OmicsProcessing/articles/hybrid-imputation.html))
  - Normalise with SERRF using [`normalise_SERRF()`](https://iarcbiostat.github.io/OmicsProcessing/reference/normalise_SERRF.html) ([vignette](https://iarcbiostat.github.io/OmicsProcessing/articles/serrf-normalisation.html))
  - Cluster features by RT or correlations using [`cluster_features_by_retention_time()`](https://iarcbiostat.github.io/OmicsProcessing/reference/cluster_features_by_retention_time.html) ([vignette](https://iarcbiostat.github.io/OmicsProcessing/articles/feature-clustering.html))

## Quick start

```r
# install.packages("remotes")
remotes::install_github("IARCBiostat/OmicsProcessing")
library(OmicsProcessing)
```

Run the semi-automated pipeline with three input tables:

```r
processed <- process_data(
  data = data_features,
  data_meta_features = data_meta_features,
  data_meta_samples = data_meta_samples,
  col_samples = "ID_sample",
  exclusion_extreme_feature = TRUE,
  exclusion_extreme_sample = TRUE,
  imputation = TRUE,
  transformation = TRUE,
  outlier = TRUE,
  plate_correction = TRUE
)
```

Or stitch together a modular workflow:

```r
# Load data
df <- readr::read_csv("path/to/data")

# Filter by missingness
df_filtered <- filter_by_missingness(
  df,
  row_thresh = 0.5,
  col_thresh = 0.5,
  target_cols = "@",
  is_qc = grepl("^sQC", df$sample_type),
  filter_order = "iterative"
)

# Detect outlier samples (PCA + LOF)
outliers <- remove_outliers(
  df_filtered,
  target_cols = "@",
  is_qc = grepl("^sQC", df_filtered$sample_type),
  method = "pca-lof-overall",
  impute_method = "half-min-value",
  restore_missing_values = TRUE,
  return_ggplots = FALSE
)
df_clean <- outliers$df_filtered

# Log-transform features
df_clean <- df_clean %>%
  dplyr::mutate(dplyr::across(tidyselect::contains("@"), log1p))

# Impute missing values (RF + LCMD)
df_imputed <- hybrid_imputation(
  df_clean,
  target_cols = "@",
  method = "RF-LCMD",
  oobe_threshold = 0.1
)$hybrid_rf_lcmd

# SERRF normalisation
df_normalised <- normalise_SERRF(
  df_imputed,
  target_cols = "@",
  is_qc = grepl("^sQC", df_imputed$sample_type),
  strata_col = "batch"
)

# Cluster features by RT using correlations
clusters <- cluster_features_by_retention_time(
  df = df_normalised,
  target_cols = "@",
  rt_height = 0.07,
  method = "correlations",
  cut_height = 0.26,
  corr_thresh = 0.75
)
```

## Developers & Contributors

We welcome contributions to **OmicsProcessing**. Our priorities are clean code and good documentation.

Please follow these guidelines: [Developers & Contributors](https://iarcbiostat.github.io/OmicsProcessing/articles/developer-guidelines.html)

## Resources

- Data filtering vignette: [Filtering missingness](https://iarcbiostat.github.io/OmicsProcessing/articles/data-filtering.html)
- Outlier removal vignette: [PCA + LOF outlier detection](https://iarcbiostat.github.io/OmicsProcessing/articles/outlier-removal.html)
- Hybrid imputation vignette: [Random Forest + LCMD](https://iarcbiostat.github.io/OmicsProcessing/articles/hybrid-imputation.html)
- Function reference index: [All functions](https://iarcbiostat.github.io/OmicsProcessing/reference/index.html)
- Semi-automated pipeline details: [Semi-automated pipeline vignette](https://iarcbiostat.github.io/OmicsProcessing/articles/process_data.html)
- Log-transform features: [Log transformation (log1p)](https://iarcbiostat.github.io/OmicsProcessing/articles/log-transformation.html)
- SERRF batch correction: [Batch correction using SERRF](https://iarcbiostat.github.io/OmicsProcessing/articles/serrf-normalisation.html)
- Feature clustering: [Retention-time clustering](https://iarcbiostat.github.io/OmicsProcessing/articles/feature-clustering.html)
- Developers & contributors: [Developer guide](https://iarcbiostat.github.io/OmicsProcessing/articles/developer-guidelines.html)