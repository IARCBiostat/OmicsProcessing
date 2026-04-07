#' Plot feature values by run order for reference and comparison data
#'
#' @description
#' Visualise omics feature values across run order for a set of target
#' features, highlighting QC versus non-QC samples. The function is
#' designed to assess technical variation associated with run order and
#' to evaluate the influence of stratification factors such as plate or
#' batch on feature measurements. It generates scatter plots of feature 
#' values over run order, optionally colouring points by plate and marking 
#' batch boundaries with vertical dotted lines.
#' 
#' If `df_comp` is supplied, the function returns a side-by-side comparison
#' of the reference and comparison data with a shared legend. Otherwise, it
#' returns only the reference plot. This feature can be used to assess the
#' effect of post-processing on the data (for example, normalisation).
#'
#' @param df A data frame containing the reference data. It must include the
#'   columns referenced by `target_cols` and the column named in
#'   `run_order`. It may also include columns named in `batch` and `plate`.
#' @param target_cols Character vector giving the feature columns to plot.
#'   These columns are pivoted to long format and faceted with one panel per
#'   feature.
#' @param is_qc Optional character scalar specifying the QC indicator
#'   column. If \code{NULL}, all samples are treated as non-QC.
#' @param run_order Character scalar giving the name of the column encoding
#'   run order. This column is coerced to numeric and used as the x-axis.
#' @param df_comp Optional data frame containing comparison data, for example
#'   normalised values. If provided, it must include the columns referenced by
#'   `target_cols` and the column named in `run_order`. It may also include
#'   columns named in `batch` and `plate`. Default is `NULL`.
#' @param batch Optional character scalar giving the name of the column
#'   encoding batch membership. If provided, vertical dotted lines are drawn
#'   at the end of each batch. If `NULL`, all samples are treated as a single
#'   batch and no boundary lines are drawn. Default is `NULL`.
#' @param plate Optional character scalar giving the name of the column
#'   encoding plate membership. If provided, points are coloured by plate. If
#'   `NULL`, all points are assigned to a single plate level `"all"`.
#'   Default is `NULL`.
#' @param title_ref Optional title for the reference plot. If `df_comp` is not
#'   `NULL` and `title_ref` is `NULL`, the default title is `"Reference"`.
#'   Default is `NULL`.
#' @param title_comp Optional title for the comparison plot. If `df_comp` is
#'   not `NULL` and `title_comp` is `NULL`, the default title is
#'   `"Comparison"`. Default is `NULL`.
#' @param point_size Numeric scalar giving the base point size for non-QC
#'   samples. QC points are drawn one unit larger. Default is `1`.
#'
#' @details
#' Internally, the function:
#' \itemize{
#'   \item Selects the columns `{batch, plate, run_order} \eqn{\cup}
#'   target_cols`, using `dplyr::any_of()` for optional columns and
#'   `dplyr::all_of()` for required feature columns.
#'   \item Adds an `is_qc` flag and pivots the data to long format with
#'   columns `feature` and `value`.
#'   \item Coerces the run-order column to numeric and sorts rows by run
#'   order.
#'   \item Creates a scatter plot of `value` versus `run_order`, faceted by
#'   feature with `scales = "free_y"`.
#'   \item Maps point colour to plate, shape to QC status, alpha to QC
#'   status, and size to QC status.
#'   \item Uses a rainbow palette when multiple plate levels are present and
#'   `"grey40"` when only one plate level is present.
#'   \item Optionally adds vertical dotted lines at the maximum run order
#'   within each batch.
#'   \item If `df_comp` is provided, combines the reference and comparison
#'   plots with `patchwork::wrap_plots()` and collects guides into a shared
#'   legend.
#' }
#'
#' QC samples are shown as filled circles and non-QC samples as triangles.
#' Non-QC samples are also plotted with lower alpha.
#'
#' @return
#' Invisibly returns a plot object:
#' \itemize{
#'   \item If `df_comp` is `NULL`, a single `ggplot2` plot for the reference
#'   data.
#'   \item Otherwise, a combined `patchwork` plot with the reference and
#'   comparison plots shown side by side and a shared legend.
#' }
#'
#' @author
#' Original version developed by Carlota Castro Espin. Modified by Felix
#' Boekstegers.
plot_normalization_comparison <- function(
  df,
  target_cols,
  run_order,
  is_qc = NULL,
  batch = NULL,
  plate = NULL,
  title_ref = NULL,
  df_comp = NULL,
  title_comp = NULL,
  point_size = 1
) {
  if (!is.null(df_comp)) {
    if (is.null(title_ref)) title_ref <- "Reference"
    if (is.null(title_comp)) title_comp <- "Comparison"
  }

  df_long_ref <- prepare_df_long(
    df = df,
    target_cols = target_cols,
    run_order = run_order,
    is_qc = is_qc,
    batch = batch,
    plate = plate
  )

  p_ref <- plot_scatter_normalization(
    df_long = df_long_ref,
    title = title_ref,
    batch = batch,
    point_size = point_size
  )

  if (is.null(df_comp)) {
    p_ref <- p_ref +
      ggplot2::theme(
        legend.position = "right",
        legend.key.size = grid::unit(1.2, "lines"),
        legend.text = ggplot2::element_text(size = 11),
        legend.title = ggplot2::element_text(size = 12)
      ) +
      ggplot2::guides(
        color = ggplot2::guide_legend(
          override.aes = list(size = 4)
        )
      )

    return(invisible(p_ref))
  }

  df_long_comp <- prepare_df_long(
    df = df_comp,
    target_cols = target_cols,
    run_order = run_order,
    is_qc = is_qc,
    batch = batch,
    plate = plate
  )

  p_comp <- plot_scatter_normalization(
    df_long = df_long_comp,
    title = title_comp,
    batch = batch,
    point_size = point_size
  )

  combined_plot <- patchwork::wrap_plots(
    p_ref,
    p_comp,
    ncol = 2,
    guides = "collect"
  ) &
    ggplot2::theme(
      legend.position = "right",
      legend.key.size = grid::unit(1.2, "lines"),
      legend.text = ggplot2::element_text(size = 11),
      plot.background = ggplot2::element_rect(fill = NA, colour = NA),
      panel.background = ggplot2::element_rect(fill = NA, colour = NA),
      legend.title = ggplot2::element_text(size = 12)
    ) &
    ggplot2::guides(
      color = ggplot2::guide_legend(
        override.aes = list(size = 4)
      )
    )

  invisible(combined_plot)
}


#' Prepare long-format data for normalisation plots
#'
#' Reshapes a wide data frame of feature intensities into long format
#' suitable for plotting against run order. Adds standardised columns for
#' run order, feature name, value, QC status, batch, and plate.
#'
#' @param df A data frame containing feature measurements and metadata.
#' @param target_cols Character vector of feature column names to include.
#' @param run_order Character scalar specifying the run order column.
#' @param is_qc Optional character scalar specifying the QC indicator
#'   column. If \code{NULL}, all samples are treated as non-QC.
#' @param batch Optional character scalar specifying the batch column.
#'   If \code{NULL}, a single batch level \code{"all"} is used.
#' @param plate Optional character scalar specifying the plate column.
#'   If \code{NULL}, a single plate level \code{"all"} is used.
#'
#' @return A long-format \code{data.frame} with columns:
#' \describe{
#'   \item{run_order}{Numeric run order.}
#'   \item{feature}{Feature name.}
#'   \item{value}{Feature intensity.}
#'   \item{is_qc}{Logical QC indicator.}
#'   \item{batch}{Factor batch assignment.}
#'   \item{plate}{Factor plate assignment.}
#' }
#'
#' @examples
#' df_long <- prepare_df_long(
#'   df = df,
#'   target_cols = c("feat1", "feat2"),
#'   run_order = "injection_order",
#'   is_qc = "qc_flag",
#'   batch = "batch",
#'   plate = "plate"
#' )
prepare_df_long <- function(
  df,
  target_cols,
  run_order,
  is_qc = NULL,
  batch = NULL,
  plate = NULL
) {
  df_long <- df %>%
    dplyr::select(
      dplyr::any_of(c(batch, plate, run_order, is_qc)),
      dplyr::all_of(target_cols)
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(target_cols),
      names_to = "feature",
      values_to = "value"
    )

  df_long$run_order <- as.numeric(df_long[[run_order]])

  df_long <- df_long %>%
    dplyr::arrange(.data$run_order)

  # QC handling (column or default)
  if (is.null(is_qc)) {
    df_long$is_qc <- FALSE
  } else {
    df_long$is_qc <- as.logical(df_long[[is_qc]])
  }

  # Batch handling
  if (is.null(batch)) {
    df_long$batch <- factor("all")
  } else {
    df_long$batch <- factor(df_long[[batch]])
  }

  # Plate handling
  if (is.null(plate)) {
    df_long$plate <- factor("all")
  } else {
    df_long$plate <- factor(df_long[[plate]])
  }

  df_long
}

#' Plot feature intensities across run order
#'
#' Generates scatter plots of feature intensities against run order from a
#' long-format data frame. Points are coloured by plate and styled by QC
#' status. Optional vertical lines indicate batch boundaries.
#'
#' @param df_long A long-format data frame produced by
#'   \code{OmicsProcessing::prepare_df_long()}.
#' @param title Optional plot title.
#' @param batch Optional character scalar specifying the batch column.
#'   If provided, vertical dotted lines are drawn at batch boundaries.
#' @param point_size Numeric scalar controlling base point size.
#'
#' @return A \code{ggplot2} object.
#'
#' @examples
#' p <- plot_scatter_normalization(
#'   df_long = df_long,
#'   title = "Before normalisation",
#'   batch = "batch",
#'   point_size = 1
#' )
#' print(p)
plot_scatter_normalization <- function(
  df_long,
  title = NULL,
  batch = NULL,
  point_size = 1
) {
  if (is.null(batch)) {
    batch_boundaries <- NULL
  } else {
    batch_boundaries <- df_long %>%
      dplyr::group_by(.data$batch) %>%
      dplyr::summarise(
        max_run = max(.data$run_order, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::pull(.data$max_run)
  }

  num_plates <- length(unique(df_long$plate))
  plate_colors <- if (num_plates > 1) {
    grDevices::rainbow(num_plates)
  } else {
    "grey40"
  }

  fig <- ggplot2::ggplot(
    df_long,
    ggplot2::aes(
      x = .data$run_order,
      y = .data$value,
      color = .data$plate,
      shape = .data$is_qc,
      alpha = .data$is_qc,
      size = .data$is_qc
    )
  ) +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~feature, ncol = 1, scales = "free_y") +
    ggplot2::scale_color_manual(values = plate_colors) +
    ggplot2::scale_shape_manual(
      name = "SampleType",
      values = c(`TRUE` = 16, `FALSE` = 17),
      breaks = c(TRUE, FALSE),
      labels = c("QC", "Sample")
    ) +
    ggplot2::scale_size_manual(
      values = c(`TRUE` = point_size + 1, `FALSE` = point_size),
      guide = "none"
    ) +
    ggplot2::scale_alpha_manual(
      values = c(`TRUE` = 1, `FALSE` = 0.3),
      guide = "none"
    ) +
    ggplot2::labs(
      title = title,
      x = "Run order",
      y = "Feature intensity",
      color = "Plate"
    ) +
    ggplot2::theme_minimal(base_size = 9) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )

  if (!is.null(batch)) {
    fig <- fig +
      ggplot2::geom_vline(
        xintercept = batch_boundaries[!is.na(batch_boundaries)] + 0.5,
        color = "black",
        linetype = "dotted",
        linewidth = 0.5
      )
  }

  fig
}

