# OmicsProcessing

## Overview

The `OmicsProcessing` package offers two flexible ways to pre-process omics data (e.g., metabolomics or proteomics):

## Choose your workflow

### 1. Semi-Automated Pipeline

Use the main `process_data()` function to run your data through a sequential, customizable pipeline with minimal setup. [Jump to the `process_data()` documentation](#the-process_data-documentation)

### 2. Modular Workflow

Build your own custom pipeline by combining individual functions. This provides maximum control over every processing step. [Jump to the Modular Workflow Guide](#modular-workflow)

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

Or build your one analysis pipeline using teh follwoing modules:

1) Data filtering vignette: [Filtering missingness](https://iarcbiostat.github.io/OmicsProcessing/articles/data-filtering.html)
2) Outlier removal vignette: [PCA + LOF outlier detection](https://iarcbiostat.github.io/OmicsProcessing/articles/outlier-removal.html)
3) Log-transform vignette: [Log transformation (log1p)](https://iarcbiostat.github.io/OmicsProcessing/articles/log-transformation.html)
4) Hybrid imputation vignette: [Random Forest + LCMD](https://iarcbiostat.github.io/OmicsProcessing/articles/hybrid-imputation.html)
5) SERRF batch correction vignette: [Batch correction using SERRF](https://iarcbiostat.github.io/OmicsProcessing/articles/serrf-normalisation.html)
6) Feature clustering vignette: [Retention-time clustering](https://iarcbiostat.github.io/OmicsProcessing/articles/feature-clustering.html)


## Developers & Contributors

We welcome contributions to **OmicsProcessing**! Our highest priority is
**clean code** and **good documentation** — with emphasis on *good*.

PLease follow these guidlines to contribute: [Developers & Contributors](https://iarcbiostat.github.io/OmicsProcessing/articles/developer-guidelines.html)




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
- Modular workflow deep dive: [README section](https://github.com/farnudia/OmicsProcessing#modular-workflow)
