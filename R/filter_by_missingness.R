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
#' Users can optionally control the order in which row and column filtering is 
#' applied using the \code{filter_order} argument. The default is 
#' \code{"simultaneous"}, which performs both row and column filtering at once. 
#' Sequential filtering modes (\code{"col_then_row"} or \code{"row_then_col"}) 
#' may be useful for fine-tuning missingness cleanup in certain datasets.
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
#' @param filter_order Character string indicating the filtering mode.
#'   One of \code{"simultaneous"} (default), \code{"col_then_row"}, or 
#'   \code{"row_then_col"}.
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
#' # 4. Remove columns first, then rows
#' filter_by_missingness(df, filter_order = "col_then_row")
#'
#' # 5. Remove rows first, then columns
#' filter_by_missingness(df, filter_order = "row_then_col")
#'
#' @export
filter_by_missingness <- function(df,
                                  row_thresh = 0.5,
                                  col_thresh = 0.5,
                                  target_cols = NULL,
                                  is_qc = NULL,
                                  filter_order = c("simultaneous", "col_then_row", "row_then_col")) {
  filter_order <- match.arg(filter_order) #set default to "simultaneous"

  if (is.null(is_qc)) {
    is_qc <- rep(FALSE, nrow(df))
  }
  if (length(is_qc) != nrow(df)) {
    stop("`is_qc` must be a logical vector with the same length as nrow(df).")
  }

  # Determine target columns
  if (is.null(target_cols)) {
    target_cols <- names(df)
  } else if (length(target_cols) == 1) { # Assume regex
    target_cols_matched <- grep(target_cols, names(df), value = TRUE)
    if (length(target_cols_matched) == 0) {
      stop("No columns matched the regular expression in `target_cols`.")
    }
    target_cols <- target_cols_matched
  } else {  # Explicit column names
    missing_cols <- setdiff(target_cols, names(df))
    if (length(missing_cols) > 0) {
      stop("The following `target_cols` are not in the dataframe: ", 
           paste(missing_cols, collapse = ", "))
    }
  }

  df_filtered <- df
  non_target_cols <- setdiff(names(df), target_cols)

  if (filter_order == "simultaneous") {
    df_non_qc <- df[!is_qc, , drop = FALSE]
    col_na_prop <- colMeans(is.na(df_non_qc[, target_cols, drop = FALSE]))
    target_cols_kept <- names(col_na_prop)[col_na_prop <= col_thresh]
    row_na_prop <- rowMeans(is.na(df_non_qc[, target_cols, drop = FALSE]))
    rows_to_keep_non_qc <- which(row_na_prop <= row_thresh)
    all_rows_to_keep <- c(which(!is_qc)[rows_to_keep_non_qc], which(is_qc))
    all_rows_to_keep <- sort(unique(all_rows_to_keep))
    final_cols <- c(non_target_cols, target_cols_kept)
    df_filtered <- df[all_rows_to_keep, final_cols, drop = FALSE]

  } else if (filter_order == "col_then_row") {
    df_filtered <- filter_columns(df_filtered, is_qc, target_cols, col_thresh, non_target_cols)
    target_cols <- intersect(names(df_filtered), target_cols)
    row_filter_out <- filter_rows(df_filtered, is_qc, target_cols, row_thresh)
    df_filtered <- row_filter_out$data 

  } else if (filter_order == "row_then_col") {
    row_filter_out <- filter_rows(df_filtered, is_qc, target_cols, row_thresh)
    is_qc <- row_filter_out$mask_qc
    df_filtered <- row_filter_out$data
    df_filtered <- filter_columns(df_filtered, is_qc, target_cols, col_thresh, non_target_cols)
  }

  return(df_filtered)
}

#' Filter columns by missingness threshold
#'
#' Filters columns in a data frame by calculating the proportion of missing 
#' values in the selected target columns, excluding QC samples. Only target 
#' columns with missingness less than or equal to the column threshold are kept.
#' All non-target columns are always retained.
#'
#' @param data A data frame to filter.
#' @param mask_qc Logical vector indicating QC rows. These rows are excluded 
#'   from the column-wise missingness calculation.
#' @param target_cols Character vector of column names to assess for filtering.
#' @param col_thresh Maximum allowed missingness in a column (0–1).
#' @param non_target_cols Character vector of column names that are always 
#'   retained regardless of missingness.
#'
#' @return A filtered data frame with selected target columns removed.
#' @keywords internal
filter_columns <- function(data, mask_qc, target_cols, col_thresh, non_target_cols) {
  col_na_prop <- colMeans(is.na(data[!mask_qc, target_cols, drop = FALSE]))
  print(col_na_prop)
  target_cols_kept <- names(col_na_prop)[col_na_prop <= col_thresh]
  print(col_na_prop <= col_thresh)
  kept_cols <- c(non_target_cols, target_cols_kept)
  data[, kept_cols, drop = FALSE]
}

#' Filter rows by missingness threshold
#'
#' Filters rows in a data frame by calculating the proportion of missing 
#' values across the selected target columns, excluding QC rows from the 
#' filtering. QC rows are always retained in the output.
#'
#' @param data A data frame to filter.
#' @param mask_qc Logical vector indicating QC rows.
#' @param target_cols Character vector of column names to use in row filtering.
#' @param row_thresh Maximum allowed missingness in a row (0–1).
#'
#' @return A list with two elements:
#' \itemize{
#'   \item \code{data}: The filtered data frame.
#'   \item \code{mask_qc}: The logical QC vector corresponding to the rows 
#'         retained in the filtered data.
#' }
#' @keywords internal
filter_rows <- function(data, mask_qc, target_cols, row_thresh) {
  row_na_prop <- rowMeans(is.na(data[!mask_qc, target_cols, drop = FALSE]))
  keep_idx_non_qc <- which(row_na_prop <= row_thresh)
  final_rows <- c(which(!mask_qc)[keep_idx_non_qc], which(mask_qc))
  final_rows <- sort(unique(final_rows))
  filtered_data <- data[final_rows, , drop = FALSE]
  filtered_mask_qc <- mask_qc[final_rows]

  list(data = filtered_data, mask_qc = filtered_mask_qc)
}
