# PCA-Based Outlier Detection Using LOF

Performs Principal Component Analysis (PCA) on a data frame and
identifies sample outliers using the Local Outlier Factor (LOF) method.
Optionally returns a `ggplot2` pair plot of the PCA scores coloured by
LOF. Samples detected as outliers are removed from the returned data.

## Usage

``` r
outlier_pca_lof(df, return_ggplot = TRUE, verbose = TRUE)
```

## Arguments

- df:

  A data frame with samples in rows and features in columns. Must not
  contain missing values.

- return_ggplot:

  Logical; if `TRUE`, returns a `ggplot2` pair plot highlighting
  outliers.

- verbose:

  Logical; if `TRUE`, prints progress messages.

## Value

A list with the following elements:

- df:

  Filtered data frame with outliers removed. If missing values are
  present, the original input is returned unchanged.

- plot_samples_outlier:

  A `ggplot2` object showing PCA and LOF scores, or `NULL` if
  `return_ggplot = FALSE` or if missing values were found.

- id_samples_outlier:

  A character vector of sample IDs (rownames) identified as outliers, or
  `NULL` if skipped.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- outlier_pca_lof(df = my_data)
head(result$df)
print(result$plot_samples_outlier)
result$id_samples_outlier
} # }
```
