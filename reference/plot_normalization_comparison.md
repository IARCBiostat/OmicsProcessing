# Plot feature values before/after normalization by run order

This function visualizes the effect of normalization (e.g., SERRF) on
omics feature values across batches and plates. It creates scatter plots
of feature values over **run order** for a set of target features,
highlighting QC vs. non‑QC sample, optionally with batch boundaries
indicated by vertical dotted lines and points colored by plate. If
`df_after` is supplied, the function returns a side‑by‑side comparison
(before vs. after normalization) with a shared legend. Otherwise, it
returns only the "before" plot.

## Usage

``` r
plot_normalization_comparison(
  df_before,
  df_after = NULL,
  target_cols,
  is_qc_before,
  is_qc_after = NULL,
  batch = NULL,
  plate = NULL,
  run_order,
  title_before = "Before normalization",
  title_after = "After normalization"
)
```

## Arguments

- df_before:

  A data frame containing the data **before preprocessing**. Must
  include the columns referenced by `target_cols` and the column named
  in `run_order`. Optionally includes columns named in `batch` and/or
  `plate`.

- df_after:

  An optional data frame containing the data **after preprocessing**. If
  provided, must include the columns referenced by `target_cols` and the
  column named in `run_order`. Optionally includes columns named in
  `batch` and/or `plate`. Default: `NULL`.

- target_cols:

  Feature columns to normalize. Character vector or selector passed to
  `resolve_target_cols`). These columns are pivoted to long format and
  faceted (one facet per feature).

- is_qc_before:

  Logical vector (length `nrow(df_before)`) indicating which rows are QC
  samples (`TRUE`) vs. regular samples (`FALSE`) in the data before
  preprocessing. Used to control point shape and size.

- is_qc_after:

  Optional logical vector (length `nrow(df_before)`) indicating which
  rows are QC samples (`TRUE`) vs. regular samples (`FALSE`) in the data
  after preprocessing. Required if `df_after` is provided. Default:
  `NULL`.

- batch:

  Optional string giving the **name of the column** in
  `df_before`/`df_after` that encodes batch membership (e.g.,
  `"batch_id"`). If provided, vertical dotted lines mark batch
  boundaries (computed from the maximum run order per batch). If `NULL`,
  all samples are treated as a single batch and no boundary lines are
  drawn. Default: `NULL`.

- plate:

  Optional string giving the **name of the column** in
  `df_before`/`df_after` that encodes plate ID. If provided, points are
  colored by plate. If `NULL`, all points use a single color label
  `"all"`. Default: `NULL`.

- run_order:

  String giving the **name of the column** that encodes the run order.
  This column is coerced to numeric and used as the x‑axis, and the data
  are sorted by this column prior to plotting.

- title_before:

  Title for the "before" panel/plot. Default: `"Before normalization"`.

- title_after:

  Title for the "after" panel/plot. Default: `"After normalization"`.

## Value

Invisibly returns a **patchwork/ggplot object**:

- If `df_after` is `NULL`: a single plot for the "before" data (with
  legend).

- Otherwise: a two‑panel (before \| after) plot with a shared legend on
  the right.

## Details

Internally, the function:

- Selects columns `{batch, plate, run_order} ∪ target_cols` if present
  (using
  [`dplyr::any_of`](https://tidyselect.r-lib.org/reference/all_of.html)
  for optional columns and
  [`dplyr::all_of`](https://tidyselect.r-lib.org/reference/all_of.html)
  for required features).

- Adds an `is_qc` flag and pivots features to long format (`feature`,
  `value`).

- Coerces `run_order` to numeric and orders rows accordingly.

- Creates a scatter plot (`value` vs. `run_order`), faceted by `feature`
  (`scales = "free_y"`), with:

  - Color mapped to `plate` (rainbow palette if multiple plates; grey if
    single plate),

  - Shape and size mapped to `is_qc` (QC = filled circle, larger; Sample
    = triangle, smaller),

  - Optional vertical dotted lines at batch boundaries (end of each
    batch).

- If `df_after` is provided, returns a side‑by‑side comparison (before
  \| after) using `patchwork::plot_layout(guides = "collect")` to share
  the legend.

**Inputs as column names (strings):** `batch`, `plate`, and `run_order`
are expected to be **character scalars naming columns** in the data
frames, not vectors.

**Optional columns:** If `batch` or `plate` is `NULL`, a placeholder
factor `"all"` is used; if `batch` is `NULL`, no boundary lines are
drawn.
