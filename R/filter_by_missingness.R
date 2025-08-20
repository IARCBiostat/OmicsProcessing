#' Select target columns to keep based on missingness
#'
#' Computes per-column NA proportions on the provided non-QC rows of the dataset
#' and returns the subset of target columns that meet the threshold.
#'
#' @param df A data.frame.
#' @param rows_idx_non_qc Integer indices of rows to include in the calculation (should exclude QC rows).
#' @param target_cols Character vector of target column names.
#' @param col_thresh Numeric. Maximum allowed proportion of missing values per column.
#'
#' @return Character vector of target column names to keep.
#' @keywords internal
cols_keep_from_rows <- function(df, rows_idx_non_qc, target_cols, col_thresh) {
  if (length(target_cols) == 0L) return(character(0))
  if (length(rows_idx_non_qc) == 0L) return(character(0))
  props <- colMeans(is.na(df[rows_idx_non_qc, target_cols, drop = FALSE]))
  names(props)[props <= col_thresh]
}

#' Select rows to keep based on missingness
#'
#' Computes per-row NA proportions across the provided target columns.  
#' QC rows (`is_qc_full == TRUE`) are always retained.
#'
#' @param df A data.frame.
#' @param cols_target Character vector of target column names (subset of `names(df)`).
#' @param row_thresh Numeric. Maximum allowed proportion of missing values per row.
#' @param is_qc_full Logical vector of length `nrow(df)`; TRUE for QC rows.
#'
#' @return Integer indices of rows to keep.
#' @keywords internal
rows_keep_from_cols <- function(df, cols_target, row_thresh, is_qc_full) {
  n <- nrow(df)
  if (n == 0L) return(integer(0))
  if (length(cols_target) == 0L) {
    return(seq_len(n)) # keep all rows if no target columns
  }
  na_prop <- rowMeans(is.na(df[, cols_target, drop = FALSE]))
  keep_non_qc <- which(!is_qc_full & (na_prop <= row_thresh))
  keep_qc     <- which(is_qc_full)
  sort(unique(c(keep_non_qc, keep_qc)))
}

#' Finalize kept columns
#'
#' Always keeps the non-target columns alongside the retained target columns.
#'
#' @param target_cols_kept Character vector of retained target columns.
#' @param non_target_cols Character vector of non-target column names.
#' @param all_names Character vector of all column names in the dataset.
#'
#' @return Character vector of final kept column names.
#' @keywords internal
final_cols <- function(target_cols_kept, non_target_cols, all_names) {
  intersect(all_names, unique(c(non_target_cols, target_cols_kept)))
}

#' Filter a data.frame by missingness in rows and columns
#'
#' Applies different strategies to filter rows and columns of a dataset based on
#' missingness thresholds. Supports iterative refinement until stable.
#'
#' @param df A data.frame.
#' @param row_thresh Proportion of missing values allowed per (non-QC) row in target columns.
#' @param col_thresh Proportion of missing values allowed per column in target columns.
#' @param target_cols Character vector of target columns. If NULL, resolved by `resolve_target_cols()`.
#' @param is_qc Logical vector the same length as `nrow(df)` indicating QC rows (always retained).
#' @param filter_order One of `"iterative"` (default), `"col_then_row"`, `"row_then_col"`, or `"simultaneous"`.
#'   * `"iterative"`: alternately filter rows and columns until stable or `max_iter` is reached.
#'   * `"col_then_row"`: filter columns first, then rows.
#'   * `"row_then_col"`: filter rows first, then columns.
#'   * `"simultaneous"`: filter rows and columns independently, then intersect results.
#' @param max_iter Maximum number of iterations when `filter_order="iterative"`. Default 10.
#'
#' @return Filtered data.frame with a subset of rows and/or columns.
#'
#' @examples
#' # Example dataset with deliberate missingness
#' df <- data.frame(
#'   a = c(NA, 1, NA, 1, NA),
#'   b = c(NA, NA, 2, 2, NA),
#'   c = c(3, NA, NA, 3, NA),
#'   d = 1
#' )
#'
#' # Mark row 2 and 5 as QC (always kept, but excluded from filtering thresholds)
#' is_qc <- c(FALSE, TRUE, FALSE, FALSE, TRUE)
#' target_cols <- c("a","b","c")
#'
#' # Original
#' print(df)
#' #    a  b  c d
#' # 1 NA NA  3 1
#' # 2  1 NA NA 1
#' # 3 NA  2 NA 1
#' # 4  1  2  3 1
#' # 5 NA NA NA 1
#'
#' # Iterative filtering
#' print(filter_by_missingness(df, row_thresh=0.5, col_thresh=0.5,
#'                             target_cols=target_cols, is_qc=is_qc,
#'                             filter_order="iterative"))
#' # [1] "iteration"
#' #    b  c d
#' # 1 NA  3 1
#' # 2 NA NA 1
#' # 3  2 NA 1
#' # 4  2  3 1
#' # 5 NA NA 1
#'
#' # Simultaneous filtering
#' print(filter_by_missingness(df, row_thresh=0.5, col_thresh=0.5,
#'                             target_cols=target_cols, is_qc=is_qc,
#'                             filter_order="simultaneous"))
#' # [1] "simultaneous"
#' #    b  c d
#' # 2 NA NA 1
#' # 4  2  3 1
#' # 5 NA NA 1
#'
#' # Column-then-row filtering
#' print(filter_by_missingness(df, row_thresh=0.5, col_thresh=0.5,
#'                             target_cols=target_cols, is_qc=is_qc,
#'                             filter_order="col_then_row"))
#' #    b  c d
#' # 1 NA  3 1
#' # 2 NA NA 1
#' # 3  2 NA 1
#' # 4  2  3 1
#' # 5 NA NA 1
#'
#' # Row-then-column filtering
#' print(filter_by_missingness(df, row_thresh=0.5, col_thresh=0.5,
#'                             target_cols=target_cols, is_qc=is_qc,
#'                             filter_order="row_then_col"))
#' #    a  b  c d
#' # 2  1 NA NA 1
#' # 4  1  2  3 1
#' # 5 NA NA NA 1
#'
#' For this simple data set the "iteration" and the "col_then_row" filtering results are the same.
#' @export
filter_by_missingness <- function(df,
                                  row_thresh = 0.5,
                                  col_thresh = 0.5,
                                  target_cols = NULL,
                                  is_qc = NULL,
                                  filter_order = c("iterative", "simultaneous", "col_then_row", "row_then_col"),
                                  max_iter = 10) {
  filter_order <- match.arg(filter_order)

  if (is.null(is_qc)) {
    is_qc <- rep(FALSE, nrow(df))
  }
  if (length(is_qc) != nrow(df)) {
    stop("`is_qc` must be a logical vector with the same length as nrow(df).")
  }

  target_cols <- resolve_target_cols(df, target_cols)
  non_target_cols <- setdiff(names(df), target_cols)

  rows_non_qc <- which(!is_qc)

  if (filter_order == "simultaneous") {
    target_cols_kept <- cols_keep_from_rows(df, rows_non_qc, target_cols, col_thresh)
    rows_keep_idx <- rows_keep_from_cols(df, target_cols, row_thresh, is_qc)


  } else if (filter_order == "col_then_row") {
    target_cols_kept <- cols_keep_from_rows(df, rows_non_qc, target_cols, col_thresh)
    rows_keep_idx <- rows_keep_from_cols(df[, target_cols_kept, drop = FALSE], target_cols_kept, row_thresh, is_qc)

  } else if (filter_order == "row_then_col") {
    rows_keep_idx <- rows_keep_from_cols(df, target_cols, row_thresh, is_qc)
    rows_non_qc_kept <- intersect(rows_keep_idx, rows_non_qc)
    target_cols_kept <- cols_keep_from_rows(df, rows_non_qc_kept, target_cols, col_thresh)

  } else if (filter_order == "iterative") {
    n <- nrow(df)
    rows_keep_idx <- seq_len(n)
    
    target_cols_kept <- cols_keep_from_rows(df, rows_non_qc, target_cols, col_thresh)
    kept_cols <- final_cols(target_cols_kept, non_target_cols, names(df))

    prev_rows <- integer()
    prev_cols <- character()
    iter <- 0L

    repeat {
      iter <- iter + 1L
      kept_target_cols <- intersect(kept_cols, target_cols)
      rows_keep_idx <- rows_keep_from_cols(df[, kept_cols, drop = FALSE], kept_target_cols, row_thresh, is_qc)

      rows_non_qc_now <- intersect(rows_keep_idx, rows_non_qc)
      target_cols_kept <- cols_keep_from_rows(df, rows_non_qc_now, target_cols, col_thresh)
      kept_cols <- final_cols(target_cols_kept, non_target_cols, names(df))

      kept_target_cols <- intersect(kept_cols, target_cols)
      rows_keep_idx <- rows_keep_from_cols(df[, kept_cols, drop = FALSE], kept_target_cols, row_thresh, is_qc)

      cur_cols_target <- sort(intersect(kept_cols, target_cols))
      stable <- identical(sort(rows_keep_idx), sort(prev_rows)) &&
                identical(cur_cols_target, sort(prev_cols))
      if (stable || iter >= max_iter) break

      prev_rows <- rows_keep_idx
      prev_cols <- cur_cols_target
    } 
  }

  rows_keep_idx <- sort(unique(c(rows_keep_idx, which(is_qc))))
  kept_cols <- final_cols(target_cols_kept, non_target_cols, names(df))
  return(df[rows_keep_idx, kept_cols, drop = FALSE])
}
