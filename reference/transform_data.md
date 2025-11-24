# Apply Data Transformation to Data Frame

This function applies various data transformations to a numeric data
frame based on a specified transformation method. Supported methods
include "InvRank", "Log10", "Log10Capped", and "Log10ExclExtremes".

## Usage

``` r
transform_data(df, transformation_method)
```

## Arguments

- df:

  A data frame containing numeric data for transformation.

- transformation_method:

  A string specifying the transformation method to apply. Supported
  values are "InvRank", "Log10", "Log10Capped", and "Log10ExclExtremes".

## Value

A list containing:

- df:

  The transformed data frame with the applied method.

- transformation_method:

  The label of the applied transformation method.

- centre_scale:

  A label indicating whether the data was centered and scaled (TRUE or
  NA).

If conditions are not met for the specified transformation method, the
original data frame will be returned in the list.
