# Process feature data

This function processes feature data given specified metadata. It
supports exclusion of features with extreme missingness, various
imputation methods, transformation, plate correction, centering, and
case-control data handling.

## Usage

``` r
process_data(
  data,
  data_meta_features = NULL,
  data_meta_samples = NULL,
  col_samples,
  col_features = NULL,
  save = FALSE,
  path_out = NULL,
  path_outliers = NULL,
  exclusion_extreme_feature = FALSE,
  missing_pct_feature = NULL,
  exclusion_extreme_sample = FALSE,
  missing_pct_sample = NULL,
  imputation = FALSE,
  imputation_method = NULL,
  col_LOD = NULL,
  transformation = FALSE,
  transformation_method = NULL,
  outlier = FALSE,
  plate_correction = FALSE,
  cols_listRandom = NULL,
  cols_listFixedToKeep = NULL,
  cols_listFixedToRemove = NULL,
  col_HeteroSked = NULL,
  centre_scale = FALSE,
  case_control = FALSE,
  col_case_control = NULL
)
```

## Arguments

- data:

  A data frame with the first column as sample IDs and remaining columns
  containing feature values, where feature IDs are the column names.

- data_meta_features:

  A data frame containing metadata for the features, including
  information such as limit of detection and missingness percentage.

- data_meta_samples:

  A data frame containing metadata for the samples, including
  information necessary for plate correction and case-control analysis.

- col_samples:

  A string specifying the column name in `data` that contains sample IDs
  (e.g., `"Idepic_Bio"`).

- col_features:

  A string specifying the column name in `data_meta_features` that
  contains feature IDs, which should match the column names of `data`
  (e.g., `"UNIPROT"`).

- save:

  A logical for whether you want to save the feature data, plots, and
  exclusion info. Default is `FALSE`/.

- path_out:

  A string specifying the output directory where the processed data will
  be saved.

- path_outliers:

  A string specifying the output directory where outlier information
  will be saved.

- exclusion_extreme_feature:

  A logical flag indicating whether to exclude features with extreme
  missingness. Default is `FALSE`.

- missing_pct_feature:

  A numeric value specifying the threshold percentage for missingness
  above which features will be excluded (e.g., `0.9`).

- exclusion_extreme_sample:

  A logical flag indicating whether to exclude samples with extreme
  missingness. Default is `FALSE`.

- missing_pct_sample:

  A numeric value specifying the threshold percentage for missingness
  above which samples will be excluded (e.g., `0.9`).

- imputation:

  A logical flag indicating whether imputation should be performed.
  Default is `FALSE`.

- imputation_method:

  A string specifying the method to use for imputation. Options include
  `"LOD"`, `"1/5th"`, `"KNN"`, `"PPCA"`, `"median"`, `"mean"`, `"RF"`,
  and `"LCMD"`.

- col_LOD:

  A string specifying the column name in `data_meta_features` that
  contains the limit of detection (LOD) values, required if
  `imputation_method` is `"LOD"`.

- transformation:

  A logical flag indicating whether transformation should be performed.
  Default is `FALSE`.

- transformation_method:

  A string specifying the method to use for transformation. Options
  include `"InvRank"`, `"Log10"`, `"Log10Capped"`, and
  `"Log10ExclExtremes"`.

- outlier:

  A logical flag indicating whether outlier exclusion should be
  performed across features and samples. Default is `FALSE`.

- plate_correction:

  A logical flag indicating whether plate correction should be
  performed. Default is `FALSE`.

- cols_listRandom:

  A string or vector specifying columns in `data_meta_samples` to be
  treated as random effects in plate correction (e.g., `"batch_plate"`).

- cols_listFixedToKeep:

  A vector specifying columns in `data_meta_samples` to be treated as
  fixed effects in plate correction and retained in the model (e.g.,
  `c("Center", "Country")`).

- cols_listFixedToRemove:

  A vector specifying columns in `data_meta_samples` to be treated as
  fixed effects in plate correction and removed from the model. Default
  is `NULL`.

- col_HeteroSked:

  A string specifying the column in `data_meta_samples` to be used for
  heteroskedasticity correction.

- centre_scale:

  A logical flag indicating whether to center and scale the data.
  Default is `FALSE`.

- case_control:

  A logical flag indicating whether the data are case-control and if
  matched samples should be handled accordingly. Default is `FALSE`.

- col_case_control:

  A string specifying the column name in `data_meta_samples` that
  contains case-control matching information (e.g., `"Match_Caseset"`).

## Value

The processed data is returned and saved as a `.rds` file to the
specified output directory.

## Details

This function performs several data processing steps (in order):

- Excludes features with extreme missingness based on a specified
  threshold.

- Excludes samples with extreme missingness based on a specified
  threshold.

- Imputes missing values using various methods.

- Transforms the data using specified methods.

- Excludes outlying samples using PCA and LOF.

- Handles case-control data to ensure matched samples are treated
  appropriately.

- Corrects for plate effects using specified random and fixed effects.

- Centers and scales the data if `centre_scale` is `TRUE`.
