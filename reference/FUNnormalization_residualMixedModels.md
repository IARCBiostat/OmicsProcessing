# Compute Residuals Using Mixed Models

This function calculates residuals from a mixed model to remove unwanted
effects.

## Usage

``` r
FUNnormalization_residualMixedModels(
  df,
  listRandom,
  listFixedToKeep,
  listFixedToRemove,
  HeteroSked,
  i
)
```

## Arguments

- df:

  A data frame with feature measurements and relevant variables.

- listRandom:

  A character vector of strings specifying random effect variables.

- listFixedToKeep:

  A character vector of strings specifying fixed effect variables to
  keep.

- listFixedToRemove:

  A character vector of strings specifying fixed effect variables to
  remove.

- HeteroSked:

  A string or NULL. If not NULL, specifies the variable for
  heteroskedasticity correction.

- i:

  A string specifying the current feature being processed.

## Value

A numeric vector of residuals for the given feature.
