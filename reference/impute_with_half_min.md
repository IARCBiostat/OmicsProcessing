# Impute Missing Values with Half the Minimum

This function imputes missing values in numeric columns by replacing
each `NA` with half the minimum non-missing value of that column. You
can restrict imputation to a subset of columns using the `cols`
argument.

## Usage

``` r
impute_with_half_min(df, cols = NULL)
```

## Arguments

- df:

  A data frame containing numeric columns to impute.

- cols:

  Optional. A character vector of column names to impute. If `NULL`, all
  numeric columns are imputed.

## Value

A data frame with missing values imputed in specified (or all numeric)
columns.

## Examples

``` r
if (FALSE) { # \dontrun{
impute_with_half_min(df) # Impute all numeric columns
impute_with_half_min(df, cols = c("a", "b")) # Impute only columns a and b
} # }
```
