#' Plot feature values before/after normalization by run order
#'
#' @description
#' This function visualizes the effect of normalization (e.g., SERRF) on
#' omics feature values across batches and plates.  It creates scatter plots of feature values 
#' over **run order** for a set of target features, highlighting QC vs. non‑QC sample,
#' optionally with batch boundaries indicated by vertical dotted lines and points colored by plate. 
#' If `df_after` is supplied, the function returns a side‑by‑side comparison (before vs. after normalization)
#' with a shared legend. Otherwise, it returns only the "before" plot.
#'
#' @param df_before A data frame containing the data **before preprocessing**. 
#' Must include the columns referenced by `target_cols` and the column named in
#' `run_order`. Optionally includes columns named in `batch` and/or `plate`.
#' @param df_after An optional data frame containing the data **after preprocessing**. 
#' If provided, must include the columns referenced by `target_cols` and the column named 
#' in `run_order`. Optionally includes columns named in `batch` and/or `plate`. Default: `NULL`.
#' @param target_cols Feature columns to normalize. Character vector or selector 
#' passed to `resolve_target_cols`). These columns are pivoted to long format and faceted 
#' (one facet per feature).
#' @param is_qc_before Logical vector (length `nrow(df_before)`) indicating which rows are 
#' QC samples (`TRUE`) vs. regular samples (`FALSE`) in the data before preprocessing. 
#' Used to control point shape and size.
#' @param is_qc_after Optional logical vector (length `nrow(df_before)`) indicating which rows are 
#' QC samples (`TRUE`) vs. regular samples (`FALSE`) in the data after preprocessing. 
#' Required if `df_after` is provided. Default: `NULL`.
#' @param batch Optional string giving the **name of the column** in `df_before`/`df_after`
#'   that encodes batch membership (e.g., `"batch_id"`). If provided, vertical dotted
#'   lines mark batch boundaries (computed from the maximum run order per batch).
#'   If `NULL`, all samples are treated as a single batch and no boundary lines are drawn.
#'   Default: `NULL`.
#' @param plate Optional string giving the **name of the column** in `df_before`/`df_after`
#'   that encodes plate ID. If provided, points are colored by plate. If `NULL`,
#'   all points use a single color label `"all"`. Default: `NULL`.
#' @param run_order String giving the **name of the column** that encodes the
#'   run order. This column is coerced to numeric and used as the x‑axis,
#'   and the data are sorted by this column prior to plotting.
#' @param title_before Title for the "before" panel/plot. Default: `"Before normalization"`.
#' @param title_after Title for the "after" panel/plot. Default: `"After normalization"`.
#'
#' @details
#' Internally, the function:
#' - Selects columns `{batch, plate, run_order} ∪ target_cols` if present (using
#'   `dplyr::any_of` for optional columns and `dplyr::all_of` for required features).
#' - Adds an `is_qc` flag and pivots features to long format (`feature`, `value`).
#' - Coerces `run_order` to numeric and orders rows accordingly.
#' - Creates a scatter plot (`value` vs. `run_order`), faceted by `feature`
#'   (`scales = "free_y"`), with:
#'   * Color mapped to `plate` (rainbow palette if multiple plates; grey if single plate),
#'   * Shape and size mapped to `is_qc` (QC = filled circle, larger; Sample = triangle, smaller),
#'   * Optional vertical dotted lines at batch boundaries (end of each batch).
#' - If `df_after` is provided, returns a side‑by‑side comparison (before | after)
#'   using `patchwork::plot_layout(guides = "collect")` to share the legend.
#'
#' **Inputs as column names (strings):** `batch`, `plate`, and `run_order` are
#' expected to be **character scalars naming columns** in the data frames, not vectors.
#'
#' **Optional columns:** If `batch` or `plate` is `NULL`, a placeholder factor
#' `"all"` is used; if `batch` is `NULL`, no boundary lines are drawn.
#'
#' @return
#' Invisibly returns a **patchwork/ggplot object**:
#' - If `df_after` is `NULL`: a single plot for the "before" data (with legend).
#' - Otherwise: a two‑panel (before | after) plot with a shared legend on the right.
#'
#' @credits Original version developed by Carlota Castro Espin.
#'   This version contains modifications by Felix Boekstegers.
#' 
#' @examples
#' \dontrun{
# library(dplyr)
# library(tidyr)
# library(ggplot2)
# library(patchwork)
# 
# # Minimal reproducible example
# set.seed(1)
# n <- 60
# n_batch <- 2
# n_plate_per_batch <- 2
# df_before <- data.frame(
#   #batch_id = rep(letters[1:3], each = 20),
#   #plate_id = rep(c("P1", "P2"), length.out = n),
#   plate_id = factor(sample(seq_len(n_plate_per_batch*n_batch), n, replace = TRUE)),
#   F1 = rnorm(n, 100, 10),
#   F2 = rnorm(n, 500, 30),
#   F3 = rnorm(n, 50, 3)
# )
# df_before <- df_before[order(df_before$plate_id),]
# df_before$run_ord <- seq_len(n)
# df_before$batch_id <- (as.integer(df_before$plate_id) - 1L) %/% n_plate_per_batch + 1L
# is_qc_before <- rep(c(TRUE, FALSE), length.out = n)
# 
# # Before only
# p1 <- plot_normalization_comparison(
#   df_before = df_before,
#   df_after = NULL,
#   target_cols = c("F1", "F2","F3"),
#   is_qc_before = is_qc_before,
#   is_qc_after = NULL,
#   batch = "batch_id",
#   plate = "plate_id",
#   run_order = "run_ord"
# )
# p1
# 
# # With an "after" data frame (e.g., normalized values)
# df_after <- df_before %>%
#   mutate(
#     F1 = log1p(F1),
#     F2 = log1p(F2),
#     F3 = log1p(F3)
#   ) %>% OmicsProcessing::normalise_SERRF(
#     target_cols = "F",
#     is_qc = is_qc_before,
#     strata_col = "batch_id",
#     num_screen_SERRF = 2
#   ) %>%
#   dplyr::mutate(across(
#     tidyselect::all_of(c("F1","F2","F3")),
#     ~ as.numeric(scale(.)),
#     .names = "{.col}"
#   ))
# 
# is_qc_after <- is_qc_before
# 
# p2 <- plot_normalization_comparison(
#   df_before = df_before,
#   df_after = df_after,
#   target_cols = c("F1", "F2","F3"),
#   is_qc_before = is_qc_before,
#   is_qc_after = is_qc_after,
#   batch = "batch_id",
#   plate = "plate_id",
#   run_order = "run_ord"
# )
# p2
# }
#'

plot_normalization_comparison <- function(df_before, 
                                               df_after = NULL, 
                                               target_cols, 
                                               is_qc_before, 
                                               is_qc_after = NULL, 
                                               batch = NULL, 
                                               plate = NULL, 
                                               run_order, 
                                               title_before = "Before normalization", 
                                               title_after = "After normalization") {
  
  prepare_df_long <- function(df, is_qc) {
    df_long <- df %>% 
      dplyr::select(any_of(c(batch, plate, run_order)), all_of(target_cols)) %>% 
      dplyr::mutate(is_qc = is_qc) %>%
      tidyr::pivot_longer(cols = all_of(target_cols),names_to = "feature", values_to = "value")
    
    df_long$run_order <- as.numeric(df_long[[run_order]])
    df_long <- df_long %>%
      arrange(run_order)
 
      if(is.null(batch)){
        df_long$batch <- factor("all")
      } else {
        df_long$batch <- factor(df_long[[batch]])
      }
      if(is.null(plate)){
        df_long$plate <- factor("all")
      } else {
        df_long$plate <- factor(df_long[[plate]])
      }
    return(df_long)
  }
  
  plot_scatter <- function(df_long, title) { 

    if (is.null(batch)) {
      batch_boundaries <- NULL
    } else {
    batch_boundaries <- df_long %>% 
      dplyr::group_by(batch) %>%
      dplyr::summarise(max_run = max(run_order, na.rm = TRUE)) %>%
      dplyr::pull(max_run)
    }
    
    num_plates <- length(unique(df_long$plate))
    plate_colors <- if (num_plates > 1) grDevices::rainbow(num_plates) else "grey40"
    
    fig <- ggplot(df_long, aes(x = run_order, y = value, color = plate, shape = is_qc, size = is_qc)) +
      geom_point(alpha = 0.5) +
      facet_wrap(~feature, ncol = 1, scales = "free_y") +
      scale_color_manual(values = plate_colors) +
      scale_shape_manual(
        name   = "SampleType",            
        values = c(`TRUE` = 16, `FALSE` = 17),  # 16=circle, 17=triangle
        breaks = c(TRUE, FALSE),        
        labels = c("QC", "Sample")  
      ) +
      scale_size_manual(
        values = c(`TRUE` = 3, `FALSE` = 1),  
        guide = "none"
      ) +
      labs(title = title,
           x = "Run order", y = "Feature intensity", color = "Plate") + 
      theme_minimal(base_size = 9) +
      theme(panel.grid.major.x = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom")
    
    if (!is.null(batch)) {
      fig <- fig +
        ggplot2::geom_vline(xintercept = batch_boundaries[!is.na(batch_boundaries)] + 0.5, 
                   color = "black", linetype = "dotted", linewidth = 0.5)
    }
    
    return(fig)
  }
  
  df_long_before <- prepare_df_long(df_before, is_qc_before)
  p_before <- plot_scatter(df_long_before, title_before)
  
  # If no df_after is given, return only p_before + legend
  if (is.null(df_after)) {
    combined_plot <- (p_before) &  
      theme(
        legend.position = "right",        
        legend.key.size = unit(1.2, "lines"),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 12)
      ) &
      guides(color = guide_legend(override.aes = list(size = 4))) 
    return(invisible(combined_plot))
  }
  
  df_long_after  <- prepare_df_long(df_after, is_qc_after)
  p_after  <- plot_scatter(df_long_after, title_after)

  combined_plot <- (p_before | p_after) +
    plot_layout(widths = c(1, 1), guides = "collect") &  # shared legend
    theme(
      legend.position = "right",        # move legend to the side
      legend.key.size = unit(1.2, "lines"),
      legend.text = element_text(size = 11),
      legend.title = element_text(size = 12)
    ) &
    guides(color = guide_legend(override.aes = list(size = 4))) 
  
  invisible(combined_plot) 
}





