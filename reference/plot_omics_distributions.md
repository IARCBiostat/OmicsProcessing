# Plot feature values by run order for reference and comparison data

Visualise omics feature values across run order for a set of target
features, highlighting QC versus non-QC samples. The function is
designed to assess technical variation associated with run order and to
evaluate the influence of stratification factors such as plate or batch
on feature measurements. It generates scatter plots of feature values
over run order, optionally colouring points by plate and marking batch
boundaries with vertical dotted lines.

If `df_comp` is supplied, the function returns a side-by-side comparison
of the reference and comparison data with a shared legend. Otherwise, it
returns only the reference plot. This feature can be used to assess the
effect of post-processing on the data (for example, normalisation).

## Usage

``` r
plot_omics_distributions(
  df,
  target_cols,
  run_order,
  is_qc = NULL,
  batch = NULL,
  plate = NULL,
  title = NULL,
  df_comp = NULL,
  title_comp = NULL,
  point_size = 1
)
```

## Arguments

- df:

  A data frame containing the reference data. It must include the
  columns referenced by `target_cols` and the column named in
  `run_order`. It may also include columns named in `batch` and `plate`.

- target_cols:

  Character vector giving the feature columns to plot. These columns are
  pivoted to long format and faceted with one panel per feature.

- run_order:

  Character scalar giving the name of the column encoding run order.
  This column is coerced to numeric and used as the x-axis.

- is_qc:

  Optional character scalar specifying the QC indicator column. If
  `NULL`, all samples are treated as non-QC.

- batch:

  Optional character scalar giving the name of the column encoding batch
  membership. If provided, vertical dotted lines are drawn at the end of
  each batch. If `NULL`, all samples are treated as a single batch and
  no boundary lines are drawn. Default is `NULL`.

- plate:

  Optional character scalar giving the name of the column encoding plate
  membership. If provided, points are coloured by plate. If `NULL`, all
  points are assigned to a single plate level `"all"`. Default is
  `NULL`.

- title:

  Optional title for the reference plot. If `df_comp` is not `NULL` and
  `title_ref` is `NULL`, the default title is `"Reference"`. Default is
  `NULL`.

- df_comp:

  Optional data frame containing comparison data, for example normalised
  values. If provided, it must include the columns referenced by
  `target_cols` and the column named in `run_order`. It may also include
  columns named in `batch` and `plate`. Default is `NULL`.

- title_comp:

  Optional title for the comparison plot. If `df_comp` is not `NULL` and
  `title_comp` is `NULL`, the default title is `"Comparison"`. Default
  is `NULL`.

- point_size:

  Numeric scalar giving the base point size for non-QC samples. QC
  points are drawn one unit larger. Default is `1`.

## Value

Invisibly returns a plot object:

- If `df_comp` is `NULL`, a single `ggplot2` plot for the reference
  data.

- Otherwise, a combined `patchwork` plot with the reference and
  comparison plots shown side by side and a shared legend.

## Details

Internally, the function:

- Selects the columns
  `{batch, plate, run_order} \eqn{\cup} target_cols`, using
  [`dplyr::any_of()`](https://tidyselect.r-lib.org/reference/all_of.html)
  for optional columns and
  [`dplyr::all_of()`](https://tidyselect.r-lib.org/reference/all_of.html)
  for required feature columns.

- Adds an `is_qc` flag and pivots the data to long format with columns
  `feature` and `value`.

- Coerces the run-order column to numeric and sorts rows by run order.

- Creates a scatter plot of `value` versus `run_order`, faceted by
  feature with `scales = "free_y"`.

- Maps point colour to plate, shape to QC status, alpha to QC status,
  and size to QC status.

- Uses a rainbow palette when multiple plate levels are present and
  `"grey40"` when only one plate level is present.

- Optionally adds vertical dotted lines at the maximum run order within
  each batch.

- If `df_comp` is provided, combines the reference and comparison plots
  with
  [`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html)
  and collects guides into a shared legend.

QC samples are shown as filled circles and non-QC samples as triangles.
Non-QC samples are also plotted with lower alpha.

## Author

Original version developed by Carlota Castro Espin. Modified by Felix
Boekstegers.
