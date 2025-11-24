# Filter a list to match the formal arguments of a target function

Removes elements from a list that are not formal arguments of a
specified function. Optionally emits a warning for discarded elements.

## Usage

``` r
filter_function_args(args, fun, warn = TRUE)
```

## Arguments

- args:

  A named list of arguments (e.g., user-supplied control list).

- fun:

  A function object or function name to match arguments against.

- warn:

  Logical; if TRUE, warns about arguments that are dropped. Default is
  TRUE.

## Value

A filtered list containing only arguments accepted by the function.

## Examples

``` r
control <- list(ntree = 100, bogus = 1)
clean_args <- filter_function_args(control, missForest::missForest)
#> Warning: The following arguments are not accepted by missForest::missForest and were ignored: bogus
# bogus is removed
```
