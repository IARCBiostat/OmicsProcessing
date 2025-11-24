# Resolve Target Columns from Names or Regex

This utility function determines which columns from a data frame should
be selected for downstream operations based on a user-supplied
specification. The specification can be:

## Usage

``` r
resolve_target_cols(df, target_cols)
```

## Arguments

- df:

  A data frame from which columns will be selected.

- target_cols:

  A character vector of column names or a single regular expression
  string.

## Value

A character vector of resolved column names present in `df`.

## Details

- `NULL`: Selects all columns.

- A character vector of column names: Selects those columns explicitly.

- A single string with a regular expression: Selects all matching column
  names.

## Examples

``` r
df <- data.frame(a = 1, b = 2, c_score = 3)
resolve_target_cols(df, NULL)         # returns all columns
#> [1] "a"       "b"       "c_score"
resolve_target_cols(df, "c_")         # regex match (returns "c_score")
#> [1] "c_score"
resolve_target_cols(df, c("a", "b"))  # exact names
#> [1] "a" "b"
```
