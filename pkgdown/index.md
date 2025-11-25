# OmicsProcessing

Pre-analysis processing for omics data (metabolomics, proteomics), covering missingness filtering, outlier handling, imputation, transformation, case-control handling, batch/plate correction, and SERRF-based normalisation across batches or strata.

## Choose your workflow

### Semi-automated pipeline (`process_data()`)

- End-to-end wrapper that can filter on missingness, impute, transform, remove outliers (PCA + LOF), handle matched case-control designs, correct for plate/batch effects, and centre/scale.
- Expects three data frames: feature data, feature meta-data, and sample meta-data; returns processed data frames plus exclusion IDs and PCA/LOF plots.
- Read the full pipeline walk-through in the vignette: [Semi-automated pipeline](articles/process_data.html).

### Modular workflow (build your own)

- Compose individual steps to suit your study design.
- Typical sequence: 
  - Filter data by missingness using [filter_by_missingness()](../reference/filter_by_missingness.html) [link to Filtering missingness vignette](articles/data-filtering.html)
  - Detect outlier samples using LOF [`remove_outliers()`](../reference/remove_outliers.html) [link to PCA + LOF outlier detection](articles/outlier-removal.html), 
  - Impute missing values using random forest (RF), the left-censored missing data (LCMD), or both [`hybrid_imputation()`](../reference/hybrid_imputation.html) [link to RF, LCMD, and hybrid imputation vignette](articles/hybrid-imputation.html), 
  - Normalise data with Systematical Error Removal using Random Forest (SERRF) using QC samples for the different strata [`normalise_SERRF()`](../reference/normalise_SERRF.html) [link to Batch correction using SERRF vignette](articles/serrf-normalisation.html), 
  - cluster features by retention time (RT) using custom scores or by their correlations [`cluster_features_by_retention_time()`](../reference/cluster_features_by_retention_time.html), [link to Retention-time clustering vignette](articles/feature-clustering.html).


## Quick start

```r
# install.packages("remotes")
remotes::install_github("IARCBiostat/OmicsProcessing")
library(OmicsProcessing)
```

Run the semi-automated pipeline with your three input tables:

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

Or have a look at this workflow that filters data by missingness, removes outlier samples, log-transforms the data, imputes, normalises the data, and finally clusters the features

```r
df <- readr::read_csv("path/to/data")

filtered_df <- OmicsProcessing::filter_by_missingness(
  df,
  row_thresh = 0.5, # Remove features with >50% missingness
  col_thresh = 0.5, # Remove samples with >50% missingness
  target_cols = "@", # Automatically detect feature columns that have "@" in it
  is_qc = grepl("^sQC", df$sample_type), # Identify QC samples
  filter_order = "iterative" # Default: iterative filtering
)


outlier_results <- OmicsProcessing::remove_outliers(
    filtered_df,
    target_cols = "@",
    is_qc = grepl("^sQC", filtered_df$sample_type),
    method = "pca-lof-overall",
    impute_method = "half-min-value",
    restore_missing_values = TRUE,
    return_ggplots = FALSE
  )
clean_df <- outlier_results$df_filtered


clean_df <- clean_df %>%
    dplyr::mutate(dplyr::across(
      .cols = tidyselect::all_of(feature_cols),
      .fns = ~ log1p(.x),
      .names = "{.col}"
    ))


imputed_results <- OmicsProcessing::hybrid_imputation(
    clean_df,
    target_cols = "@",
    method = c("RF-LCMD"),
    oobe_threshold = 0.1
  )
imputed_df <- imputed_results$hybrid_rf_lcmd

normalised_df <- OmicsProcessing::normalise_SERRF(
    imputed_df,
    target_cols = "@",
    is_qc = grepl("^sQC", imputed_df$sample_type),
    strata_col = "batch"
  )



res_corr <- OmicsProcessing::cluster_features_by_retention_time(
  df = normalised_df,
  target_cols = "@",
  is_qc = grepl("^sQC", normalised_df$sample_type),
  rt_height = 0.07,
  method = "correlations",
  cut_height = 0.26,
  corr_thresh = 0.75
)

clustered_df_corr <- res_corr$clustered_df   
```

## Developers & Contributors

We welcome contributions to **OmicsProcessing**! Our highest priority is
**clean code** and **good documentation** — with emphasis on *good*.

PLease follow these guidlines to contribute: [Developers & Contributors](articles/developer-guidelines.html)


## Resources

- Data filtering vignette: [Filtering missingness](articles/data-filtering.html)
- Outlier removal vignette: [PCA + LOF outlier detection](articles/outlier-removal.html)
- Hybrid imputation vignette: [Random Forest + LCMD](articles/hybrid-imputation.html)
- Function reference index: [All functions](reference/index.html)
- Semi-automated pipeline details: [Semi-automated pipeline vignette](articles/process_data.html)
- Log-transform features: [Log transformation (log1p)](articles/log-transformation.html)
- SERRF batch correction: [Batch correction using SERRF](articles/serrf-normalisation.html)
- Feature clustering: [Retention-time clustering](articles/feature-clustering.html)
- Developers & contributors: [Developer guide](articles/developer-guidelines.html)
- Modular workflow deep dive: [README section](https://github.com/farnudia/OmicsProcessing#modular-workflow)
