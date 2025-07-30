#' Filter a data frame by missingness in selected rows and columns
#'
#' This function filters rows and columns from a data frame based on missing 
#' value thresholds. Missingness is calculated using a selected subset of 
#' columns, defined either by a regular expression or an explicit list of 
#' column names. QC samples can optionally be excluded from the filtering 
#' process and are always retained in the output.
#'
#' The function works on any data frame, including those that do not contain 
#' QC samples (set \code{is_qc = NULL} or leave it empty), and can also be 
#' applied to all columns (by setting \code{target_cols = NULL} or leaving 
#' it empty).
#'
#' QC samples (identified using the \code{is_qc} logical vector) are excluded 
#' from all missingness calculations, but retained in the output. This ensures 
#' that filtering is based solely on analytical samples, while preserving QC 
#' rows for downstream inspection or diagnostics.
#'
#' @param df A data frame to filter.
#' @param row_thresh Numeric value between 0 and 1. Maximum allowed proportion 
#'   of missing values per row (excluding QC rows). Default is 0.5.
#' @param col_thresh Numeric value between 0 and 1. Maximum allowed proportion 
#'   of missing values per column (calculated across non-QC rows). Default is 0.5.
#' @param target_cols Either a character vector of column names or a single 
#'   string interpreted as a regular expression. Used to select columns for 
#'   missingness filtering. If \code{NULL}, all columns are used.
#' @param is_qc Optional logical vector indicating QC rows. If \code{NULL}, 
#'   all rows are treated as analytical samples (i.e., no QC rows).
#'
#' @return A filtered data frame with:
#' \itemize{
#'   \item Retained columns: all non-target columns, plus only target columns 
#'         that pass the \code{col_thresh}.
#'   \item Retained rows: all QC rows, plus non-QC rows that pass the 
#'         \code{row_thresh} based on selected columns.
#' }
#'
#' @examples
#' # Sample data frame
#' df <- data.frame(
#'   A = c(1, NA, 3, NA),
#'   B = c(1, 2, NA, NA),
#'   C = 1:4,
#'   qc = c(FALSE, TRUE, FALSE, TRUE)
#' )
#'
#' # 1. Use explicit column list and QC exclusion
#' filter_by_missingness(df, target_cols = c("A", "B"), is_qc = df$qc)
#'
#' # 2. Use regex to select columns
#' filter_by_missingness(df, target_cols = "^A|B$", is_qc = df$qc)
#'
#' # 3. Apply to all columns and assume all rows are non-QC
#' filter_by_missingness(df)
#'
#' # 4. No QC, using only columns A and B
#' filter_by_missingness(df, target_cols = c("A", "B"))
#'
#' @export
filter_by_missingness <- function(df,
                                  row_thresh = 0.5,
                                  col_thresh = 0.5,
                                  target_cols = NULL,
                                  is_qc = NULL) {
  if (is.null(is_qc)) {
    is_qc <- rep(FALSE, nrow(df))
  }
  if (length(is_qc) != nrow(df)) {
    stop("`is_qc` must be a logical vector with the same length as nrow(df).")
  }

  # Interpret target_cols
  if (is.null(target_cols)) {
    target_cols <- names(df)
  } else if (length(target_cols) == 1) {
    # Treat as regex
    target_cols_matched <- grep(target_cols, names(df), value = TRUE)
    if (length(target_cols_matched) == 0) {
      stop("No columns matched the regular expression in `target_cols`.")
    }
    target_cols <- target_cols_matched
  } else {
    # Treat as column name list
    missing_cols <- setdiff(target_cols, names(df))
    if (length(missing_cols) > 0) {
      stop("The following `target_cols` are not in the dataframe: ", 
           paste(missing_cols, collapse = ", "))
    }
  }

  df_non_qc <- df[!is_qc, , drop = FALSE]

  # --- Column filtering ---
  col_na_prop <- colMeans(is.na(df_non_qc[, target_cols, drop = FALSE]))
  target_cols_kept <- names(col_na_prop)[col_na_prop <= col_thresh]

  # --- Row filtering ---
  row_na_prop <- rowMeans(is.na(df_non_qc[, target_cols, drop = FALSE]))
  rows_to_keep_non_qc <- which(row_na_prop <= row_thresh)
  
  # Retain QC + passing non-QC rows
  all_rows_to_keep <- which(!is_qc)[rows_to_keep_non_qc]
  all_rows_to_keep <- c(all_rows_to_keep, which(is_qc))
  all_rows_to_keep <- sort(unique(all_rows_to_keep))

  # Final columns: non-target + retained target columns
  non_target_cols <- setdiff(names(df), target_cols)
  final_cols <- c(non_target_cols, target_cols_kept)
  
  df_filtered <- df[all_rows_to_keep, final_cols, drop = FALSE]
  return(df_filtered)
}