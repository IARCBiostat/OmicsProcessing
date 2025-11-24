# Normalize feature Data Using Residual Mixed Models

This function normalizes feature data by removing unwanted effects using
mixed models. It accounts for random effects and fixed effects specified
by the user, and optionally corrects for heteroskedasticity. Residuals
are calculated with
[`FUNnormalization_residualMixedModels()`](https://iarcbiostat.github.io/OmicsProcessing/reference/FUNnormalization_residualMixedModels.md).
The method is details in <https://www.mdpi.com/2218-1989/11/9/631>

## Usage

``` r
normalization_residualMixedModels(
  list,
  identifier = c("ID_sample"),
  listRandom = NULL,
  listFixedToKeep = NULL,
  listFixedToRemove = NULL,
  HeteroSked = NULL
)
```

## Arguments

- list:

  A list containing the following elements:

  data_features

  :   A data frame or tibble of dimensions n x (K+p) with:

      n

      :   Number of observations.

      p

      :   Number of features.

      K

      :   Number of variables used for unique identification of
          individuals.

  data_samples

  :   A data frame or tibble of dimensions n x (K+d) with:

      n

      :   Number of observations.

      K

      :   Number of unique identifiers.

      d

      :   Additional variables useful in final analysis (e.g., country,
          age, BMI).

  data_meta_features

  :   A p x 3 matrix indicating each feature's Name, Class, and Type.

- identifier:

  A character vector of strings indicating the names of variables used
  for unique identification of individuals.

- listRandom:

  A character vector of strings containing variable names modeled as
  random effects to be removed. If not NULL, should be either of length
  1 or contain nested variables.

- listFixedToKeep:

  A character vector of strings containing variable names modeled as
  fixed effects to be kept.

- listFixedToRemove:

  A character vector of strings containing variable names modeled as
  fixed effects to be removed.

- HeteroSked:

  A string or NULL. If not NULL, the name of the variable for which
  heteroskedasticity will be accounted for. Must be included in
  `listRandom`.

## Value

A list with:

- data:

  A tibble with unwanted variation removed.

- data_samples:

  The input 'data_samples' data frame, ordered by IdentifierPipeline.

- data_meta_features:

  The input 'data_meta_features' matrix.
