# OmicsProcessing

Pre-analysis processing for omics data (metabolomics, proteomics),
covering missingness filtering, outlier handling, imputation,
transformation, case-control handling, batch/plate correction, and
SERRF-based normalisation across batches or strata.

## Choose your workflow

### Semi-automated pipeline (`process_data()`)

- End-to-end wrapper that can filter on missingness, impute, transform,
  remove outliers (PCA + LOF), handle matched case-control designs,
  correct for plate/batch effects, and centre/scale.
- Expects three data frames: feature data, feature meta-data, and sample
  meta-data; returns processed data frames plus exclusion IDs and
  PCA/LOF plots.
- Read the full pipeline walk-through in the vignette: [Semi-automated
  pipeline](https://iarcbiostat.github.io/OmicsProcessing/articles/process_data.md).

### Modular workflow (build your own)

- Compose individual steps to suit your study design and QC needs.
- Typical sequence: filter missingness
  ([`filter_by_missingness()`](https://iarcbiostat.github.io/OmicsProcessing/reference/filter_by_missingness.md)),
  detect outliers
  ([`remove_outliers()`](https://iarcbiostat.github.io/OmicsProcessing/reference/remove_outliers.md)),
  impute
  ([`hybrid_imputation()`](https://iarcbiostat.github.io/OmicsProcessing/reference/hybrid_imputation.md)),
  and normalise
  ([`normalise_SERRF_by_batch()`](https://iarcbiostat.github.io/OmicsProcessing/reference/normalise_SERRF_by_batch.md)),
  with helpers such as
  [`resolve_target_cols()`](https://iarcbiostat.github.io/OmicsProcessing/reference/resolve_target_cols.md).

Here is more details on the modules: 1) Data filtering vignette:
[Filtering
missingness](https://iarcbiostat.github.io/OmicsProcessing/articles/data-filtering.md)
2) Outlier removal vignette: [PCA + LOF outlier
detection](https://iarcbiostat.github.io/OmicsProcessing/articles/outlier-removal.md)
3) Log-transform vignette: [Log transformation
(log1p)](https://iarcbiostat.github.io/OmicsProcessing/articles/log-transformation.md)
4) Hybrid imputation vignette: [Random Forest +
LCMD](https://iarcbiostat.github.io/OmicsProcessing/articles/hybrid-imputation.md)
5) SERRF batch correction vignette: [Batch correction using
SERRF](https://iarcbiostat.github.io/OmicsProcessing/articles/serrf-normalisation.md)
6) Feature clustering vignette: [Retention-time
clustering](https://iarcbiostat.github.io/OmicsProcessing/articles/feature-clustering.md)
7) Developer guide: [Developers &
Contributors](https://iarcbiostat.github.io/OmicsProcessing/articles/developer-guidelines.md)

## Quick start

``` r
# install.packages("remotes")
remotes::install_github("farnudia/OmicsProcessing")
library(OmicsProcessing)
```

Run the semi-automated pipeline with your three input tables:

``` r
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

- Data filtering vignette: [Filtering
  missingness](https://iarcbiostat.github.io/OmicsProcessing/articles/data-filtering.md)
- Outlier removal vignette: [PCA + LOF outlier
  detection](https://iarcbiostat.github.io/OmicsProcessing/articles/outlier-removal.md)
- Hybrid imputation vignette: [Random Forest +
  LCMD](https://iarcbiostat.github.io/OmicsProcessing/articles/hybrid-imputation.md)
- Function reference index: [All
  functions](https://iarcbiostat.github.io/OmicsProcessing/reference/index.md)
- Semi-automated pipeline details: [Semi-automated pipeline
  vignette](https://iarcbiostat.github.io/OmicsProcessing/articles/process_data.md)
- Log-transform features: [Log transformation
  (log1p)](https://iarcbiostat.github.io/OmicsProcessing/articles/log-transformation.md)
- SERRF batch correction: [Batch correction using
  SERRF](https://iarcbiostat.github.io/OmicsProcessing/articles/serrf-normalisation.md)
- Feature clustering: [Retention-time
  clustering](https://iarcbiostat.github.io/OmicsProcessing/articles/feature-clustering.md)
- Developers & contributors: [Developer
  guide](https://iarcbiostat.github.io/OmicsProcessing/articles/developer-guidelines.md)
- Modular workflow deep dive: [README
  section](https://github.com/farnudia/OmicsProcessing#modular-workflow)
