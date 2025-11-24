# Parse mass/retention-time strings

Extract mass and retention time (RT) values from strings of the form
"mass@rt". Accepts a single string or a vector.

## Usage

``` r
parse_mass_rt(x, what = c("mass", "rt", "both"), clean_rt_suffix = TRUE)
```

## Arguments

- x:

  Character vector with entries like "123.4@56.7" or, when
  `clean_rt_suffix = TRUE`, possibly "123.4@56.7:1".

- what:

  Which values to return: "mass", "rt", or "both".

- clean_rt_suffix:

  Logical. If TRUE, removes suffixes such as ":1" or ":2" from the RT
  part.

## Value

Numeric vector (for "mass" or "rt") or a data.frame with two columns
(for "both").

## Examples

``` r
parse_mass_rt("100.5@23.1", what = "mass")
#> [1] 100.5
parse_mass_rt(c("10.2@1.5", "20.4@2.5"), what = "rt")
#> [1] 1.5 2.5
parse_mass_rt("50@7.5", what = "both")
#>   mass  rt
#> 1   50 7.5

parse_mass_rt("100.5@23.1:1",
  what = "rt",
  clean_rt_suffix = TRUE
)
#> [1] 23.1
```
