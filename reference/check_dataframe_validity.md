# Validate a Data Frame for LCMD Imputation

Ensures the input is a data frame with at least 1 column, 2 rows, and
that all columns are numeric.

## Usage

``` r
check_dataframe_validity(df)
```

## Arguments

- df:

  An object to be validated.

## Value

TRUE if the input passes all checks; otherwise throws an error.
