# Plot feature intensities across run order

Generates scatter plots of feature intensities against run order from a
long-format data frame. Points are coloured by plate and styled by QC
status. Optional vertical lines indicate batch boundaries.

## Usage

``` r
plot_scatter_omics_feature(df_long, title = NULL, batch = NULL, point_size = 1)
```

## Arguments

- df_long:

  A long-format data frame produced by
  [`OmicsProcessing::prepare_df_long()`](https://iarcbiostat.github.io/OmicsProcessing/reference/prepare_df_long.md).

- title:

  Optional plot title.

- batch:

  Optional character scalar specifying the batch column. If provided,
  vertical dotted lines are drawn at batch boundaries.

- point_size:

  Numeric scalar controlling base point size.

## Value

A `ggplot2` object.

## Examples

``` r
p <- plot_scatter_omics_feature(
  df_long = df_long,
  title = "Before normalisation",
  batch = "batch",
  point_size = 1
)
#> Error in plot_scatter_omics_feature(df_long = df_long, title = "Before normalisation",     batch = "batch", point_size = 1): could not find function "plot_scatter_omics_feature"
print(p)
#> Error: object 'p' not found
```
