#' Remove Outliers from a Data Frame
#'
#' Detects and removes outliers from the non-QC portion of a data frame
#' using a specified method (currently supports `"pca-lof-overall"`).
#' Users can specify which columns to analyse using a regular expression
#' or a character vector of column names. Optional imputation is supported
#' for missing values, and original `NA`s can be restored after filtering.
#'
#' @param df A data frame with samples in rows and features in columns.
#' @param target_cols Either a character vector of column names to include
#'        in the analysis, or a single regular expression string to match
#'        column names. If `NULL`, all columns are used.
#' @param is_qc A logical vector indicating which rows are QC samples. If
#'        `NULL`, all samples are treated as non-QC.
#' @param method Outlier detection method. Currently only
#'        `"pca-lof-overall"` is supported.
#' @param impute_method Optional method for imputing missing values prior to
#'        outlier detection. One of `NULL` (no imputation) or
#'        `"half-min-value"`.
#' @param restore_missing_values Logical; if `TRUE`, restores original `NA`
#'        values to the filtered data.
#' @param return_ggplots Logical; if `TRUE`, returns a ggplot of the
#'        outlier detection.
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{df_filtered}{Data frame with outliers removed from non-QC samples,
#'                      QC rows retained, and original row order preserved
#'                      (excluding removed outliers).}
#'   \item{plot_samples_outlier}{A `ggplot2` object visualising outlier detection.}
#'   \item{excluded_ids}{Character vector of sample IDs (rownames) identified
#'                       and removed as outliers.}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result <- remove_outliers(
#'     df = my_data,
#'     target_cols = "^metab_",
#'     is_qc = my_data$SampleType == "QC",
#'     method = "pca-lof-overall",
#'     impute_method = "half-min-value"
#' )
#'
#' head(result$df_filtered)
#' print(result$plot_samples_outlier)
#' result$excluded_ids
#' }
remove_outliers <- function(df,
                            target_cols = NULL,
                            is_qc = NULL,
                            method = c("pca-lof-overall"),
                            impute_method = c(NULL, "half-min-value"),
                            restore_missing_values = TRUE,
                            return_ggplots = FALSE) {
    if (is.null(is_qc)) {
        is_qc <- rep(FALSE, nrow(df))
    }
    if (length(is_qc) != nrow(df)) {
        stop("`is_qc` must be a logical vector with the same length as nrow(df).")
    }

    target_cols <- resolve_target_cols(df, target_cols)

    # Split QC and non-QC
    df_qc <- df[is_qc, , drop = FALSE]
    df_non_qc <- df[!is_qc, , drop = FALSE]

    # Save NA mask before imputation
    na_mask <- is.na(df_non_qc[, target_cols, drop = FALSE])

    # Imputation (optional)
    impute_method <- match.arg(impute_method)
    if (impute_method == "half-min-value") {
        df_non_qc[, target_cols] <- impute_with_half_min(df_non_qc[, target_cols])
    }
    
    # Outlier detection
    outlier_removal_method <- match.arg(method)
    if (outlier_removal_method == "pca-lof-overall") {
        outlier_result <- outlier_pca_lof(df_non_qc[, target_cols, drop = FALSE], return_ggplot = return_ggplots, verbose = FALSE)
    }
    

    # Remove detected outliers
    excluded_ids <- outlier_result$id_samples_outlier
    df_filtered_non_qc <- df_non_qc[!rownames(df_non_qc) %in% excluded_ids, , drop = FALSE]
    
    # Restore NA values (optional)
    if (restore_missing_values) {
        na_mask_filtered <- na_mask[!rownames(df_non_qc) %in% excluded_ids, , drop = FALSE]
        df_filtered_non_qc[, target_cols][na_mask_filtered] <- NA
    }

    # Combine with QC samples
    df_final <- rbind(df_filtered_non_qc, df_qc)

    # restore original row order, excluding the outliers
    df_final <- df_final[intersect(rownames(df), rownames(df_final)), , drop = FALSE]
    # Return with original row order
    return(
        list(
            df_filtered = df_final,
            plot_samples_outlier = outlier_result$plot_samples_outlier,
            excluded_ids = excluded_ids
        )
    )
}



#' Impute Missing Values with Half the Minimum
#'
#' This function imputes missing values in numeric columns by replacing each `NA`
#' with half the minimum non-missing value of that column. You can restrict imputation
#' to a subset of columns using the `cols` argument.
#'
#' @param df A data frame containing numeric columns to impute.
#' @param cols Optional. A character vector of column names to impute. If `NULL`, all numeric columns are imputed.
#'
#' @return A data frame with missing values imputed in specified (or all numeric) columns.
#' @export
#'
#' @examples
#' \dontrun{
#' impute_with_half_min(df) # Impute all numeric columns
#' impute_with_half_min(df, cols = c("a", "b")) # Impute only columns a and b
#' }
impute_with_half_min <- function(df, cols = NULL) {
    if (!is.null(cols)) {
        df <- df %>%
            dplyr::mutate(
                dplyr::across(
                    all_of(cols),
                    ~ ifelse(is.na(.x), 0.5 * min(.x, na.rm = TRUE), .x)
                )
            )
    } else {
        df <- df %>%
            dplyr::mutate(
                dplyr::across(
                    where(is.numeric),
                    ~ ifelse(is.na(.x), 0.5 * min(.x, na.rm = TRUE), .x)
                )
            )
    }
    return(df)
}
