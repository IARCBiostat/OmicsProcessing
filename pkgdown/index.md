# OmicsProcessing

Pre-analysis processing for omics data (metabolomics, proteomics), covering missingness filtering, outlier handling, imputation, transformation, case-control handling, batch/plate correction, and SERRF-based normalisation across batches or strata.

## Choose your workflow

### Semi-automated pipeline (`process_data()`)

- End-to-end wrapper that can filter on missingness, impute, transform, remove outliers (PCA + LOF), handle matched case-control designs, correct for plate/batch effects, and centre/scale.
- Expects three data frames: feature data, feature meta-data, and sample meta-data; returns processed data frames plus exclusion IDs and PCA/LOF plots.
- Read the full pipeline walk-through in the vignette: [Semi-automated pipeline](articles/process_data.html).

### Modular workflow (build your own)

- Compose individual steps to suit your study design and QC needs.
- Typical sequence: filter missingness (`filter_by_missingness()`), detect outliers (`remove_outliers()`), impute (`hybrid_imputation()`), and normalise (`normalise_SERRF_by_batch()`), with helpers such as `resolve_target_cols()`.

Here is more details on the modules:
1) Data filtering vignette: [Filtering missingness](articles/data-filtering.html)
2) Outlier removal vignette: [PCA + LOF outlier detection](articles/outlier-removal.html)



## Quick start

```r
# install.packages("remotes")
remotes::install_github("farnudia/OmicsProcessing")
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

## Resources

- Package overview and SERRF-by-strata vignette: [OmicsProcessing Overview](articles/OmicsProcessing-intro.html)
- Data filtering vignette: [Filtering missingness](articles/data-filtering.html)
- Outlier removal vignette: [PCA + LOF outlier detection](articles/outlier-removal.html)
- Function reference index: [All functions](reference/index.html)
- Semi-automated pipeline details: [Semi-automated pipeline vignette](articles/process_data.html)
- Modular workflow deep dive: [README section](https://github.com/farnudia/OmicsProcessing#modular-workflow)
