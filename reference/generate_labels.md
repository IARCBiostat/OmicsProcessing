# generate labels

This function creates labels for saving based on several data processing
options (e.g., exclusion, imputation, transformation, outlier handling,
etc.). It returns a list of character labels that indicate the specified
settings, which can be used for file naming or tracking parameter
choices.

## Usage

``` r
generate_labels(
  exclusion_extreme_feature,
  missing_pct_feature = NULL,
  exclusion_extreme_sample,
  missing_pct_sample = NULL,
  imputation,
  imputation_method = NULL,
  transformation,
  transformation_method = NULL,
  outlier,
  plate_correction,
  centre_scale
)
```

## Arguments

- exclusion_extreme_feature:

  Logical, whether extreme feature exclusion is applied.

- missing_pct_feature:

  Numeric, the percentage threshold for excluding features based on
  missing data (only used if `exclusion_extreme_feature` is TRUE).

- exclusion_extreme_sample:

  Logical, whether extreme sample exclusion is applied.

- missing_pct_sample:

  Numeric, the percentage threshold for excluding samples based on
  missing data (only used if `exclusion_extreme_sample` is TRUE).

- imputation:

  Logical, whether imputation is applied.

- imputation_method:

  Character, the method used for imputation (only used if `imputation`
  is TRUE).

- transformation:

  Logical, whether data transformation is applied.

- transformation_method:

  Character, the method used for transformation (only used if
  `transformation` is TRUE).

- outlier:

  Logical, whether outlier handling is applied.

- plate_correction:

  Logical, whether plate correction is applied.

- centre_scale:

  Logical, whether centering and scaling are applied.

## Value

A list of character labels indicating the settings for each parameter.
