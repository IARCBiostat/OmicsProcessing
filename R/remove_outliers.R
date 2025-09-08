#' Remove outliers (optionally stratified) using PCA + LOF
#'
#' @description
#' Wrapper around `outlier_pca_lof()` with conveniences:
#' - Select a subset of columns (`target_cols`)
#' - Exclude QC rows from outlier detection
#' - Temporarily impute missing values (half of min) for detection
#' - **New:** apply detection independently within strata (`strata`)
#'
#' @param df A data.frame with features in columns and samples in rows.
#' @param target_cols Character vector of column names (or tidyselect helpers if supported by `resolve_target_cols()`).
#'   If `NULL`, uses `resolve_target_cols(df, NULL)` to infer targets.
#' @param is_qc Logical vector (length `nrow(df)`) marking QC rows to exclude from detection. Defaults to all `FALSE`.
#' @param method Character; currently supports `"pca-lof-overall"` (default behavior).
#' @param impute_method `NULL` or `"half-min-value"`. If set, imputation is applied **within each stratum** on `target_cols`.
#' @param restore_missing_values Logical; if `TRUE`, original `NA`s in `target_cols` are restored after filtering.
#' @param return_ggplots Logical; if `TRUE`, returns a named list of ggplots per stratum.
#' @param strata `NULL` (default), a single column name in `df`, or an external vector of length `nrow(df)`.
#'   When provided, outlier detection is run independently within each stratum (QC rows always excluded within the stratum).
#'
#' @return A list with:
#' \describe{
#'   \item{df_filtered}{`df` with detected outlier rows removed (QC rows always retained).}
#'   \item{excluded_ids}{Character vector of row names removed (union across strata).}
#'   \item{plot_samples_outlier}{If `return_ggplots = TRUE`, a named list of ggplot objects per stratum; otherwise `NULL`.}
#' }
#'
#' @export
#' @examples
#' # No stratification (current behavior)
#' # remove_outliers(df, target_cols = c("f1","f2"))
#'
#' # Stratify by a column
#' # remove_outliers(df, target_cols = c("f1","f2"), strata = "batch")
#'
#' # Stratify by an external vector
#' # grp <- ifelse(df$batch %in% c("A","B"), "AB", "C")
#' # remove_outliers(df, target_cols = c("f1","f2"), strata = grp)
#'
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
    check_dataframe_validity(df_non_qc[, target_cols])

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

#' Remove outliers (optionally stratified) using PCA + LOF
#'
#' @description
#' Wrapper around `outlier_pca_lof()` with conveniences:
#' - Select a subset of columns (`target_cols`)
#' - Exclude QC rows from outlier detection
#' - Temporarily impute missing values (half of min) for detection
#' - **New:** apply detection independently within strata (`strata`)
#'
#' @param df A data.frame with features in columns and samples in rows.
#' @param target_cols Character vector of column names (or tidyselect helpers if supported by `resolve_target_cols()`).
#'   If `NULL`, uses `resolve_target_cols(df, NULL)` to infer targets.
#' @param is_qc Logical vector (length `nrow(df)`) marking QC rows to exclude from detection. Defaults to all `FALSE`.
#' @param method Character; currently supports `"pca-lof-overall"` (default behavior).
#' @param impute_method `NULL` or `"half-min-value"`. If set, imputation is applied **within each stratum** on `target_cols`.
#' @param restore_missing_values Logical; if `TRUE`, original `NA`s in `target_cols` are restored after filtering.
#' @param return_ggplots Logical; if `TRUE`, returns a named list of ggplots per stratum.
#' @param strata `NULL` (default), a single column name in `df`, or an external vector of length `nrow(df)`.
#'   When provided, outlier detection is run independently within each stratum (QC rows always excluded within the stratum).
#'
#' @return A list with:
#' \describe{
#'   \item{df_filtered}{`df` with detected outlier rows removed (QC rows always retained).}
#'   \item{excluded_ids}{Character vector of row names removed (union across strata).}
#'   \item{plot_samples_outlier}{If `return_ggplots = TRUE`, a named list of ggplot objects per stratum; otherwise `NULL`.}
#' }
#'
#' @examples
#' # No stratification (current behavior)
#' # remove_outliers(df, target_cols = c("f1","f2"))
#'
#' # Stratify by a column
#' # remove_outliers(df, target_cols = c("f1","f2"), strata = "batch")
#'
#' # Stratify by an external vector
#' # grp <- ifelse(df$batch %in% c("A","B"), "AB", "C")
#' # remove_outliers(df, target_cols = c("f1","f2"), strata = grp)
#'
remove_outliers <- function(df,
                            target_cols = NULL,
                            is_qc = NULL,
                            method = c("pca-lof-overall"),
                            impute_method = c(NULL, "half-min-value"),
                            restore_missing_values = TRUE,
                            return_ggplots = FALSE,
                            strata = NULL) {
    if (is.null(is_qc)) {
        is_qc <- rep(FALSE, nrow(df))
    }
    if (!is.logical(is_qc) || length(is_qc) != nrow(df)) {
        stop("`is_qc` must be a logical vector with length nrow(df).")
    }

    target_cols <- resolve_target_cols(df, target_cols)

    df_qc <- df[is_qc, , drop = FALSE]
    df_non_qc <- df[!is_qc, , drop = FALSE]
    check_dataframe_validity(df_non_qc[, target_cols, drop = FALSE])

    outlier_removal_method <- match.arg(method)
    if (!identical(outlier_removal_method, "pca-lof-overall")) {
        stop("Currently only `method = 'pca-lof-overall'` is supported.")
    }
    impute_method <- match.arg(impute_method)

    if (is.null(strata)) {
        strata_vec <- factor(rep(".all", nrow(df))) # single stratum
    } else if (is.character(strata) && length(strata) == 1L) {
        if (!strata %in% colnames(df)) {
            stop("`strata` was a single character but not a column of your data.frame: ", strata)
        }
        strata_vec <- df[[strata]]
    } else {
        # external vector
        if (length(strata) != nrow(df)) {
            stop("External `strata` vector must have the same length as your input data.frame.")
        }
        strata_vec <- strata
    }
    strata_vec <- as.factor(strata_vec)
    names(strata_vec) <- rownames(df)

    excluded_ids_all <- character(0)
    plot_list <- if (isTRUE(return_ggplots)) {
        list()
    } else {
        NULL
    }

    # Iterate over levels; detection is done on NON-QC rows within each stratum
    for (lev in levels(strata_vec)) {
        idx_all <- which(strata_vec == lev)

        idx_non_qc_stratum <- idx_all[!is_qc[idx_all]]
        if (length(idx_non_qc_stratum) == 0L) {
            next
        }

        df_s <- df[idx_non_qc_stratum, , drop = FALSE]

        rn_s <- rownames(df_s) # Keep rownames for back-mapping
        na_mask_s <- is.na(df_s[, target_cols, drop = FALSE]) # Save NA mask BEFORE imputation (only on target cols)

        if (!is.null(impute_method) && identical(impute_method, "half-min-value")) {
            df_s[, target_cols] <- impute_with_half_min(df_s[, target_cols, drop = FALSE])
        }

        # Minimum size guard: PCA/LOF won’t behave with very small n
        # (you may tune this threshold to what the pca-lof detecter expects)
        if (nrow(df_s) < 5L) {
            # Skip detection for this stratum if too few samples; no rows excluded
            if (isTRUE(return_ggplots)) {
                plot_list[[as.character(lev)]] <- NULL
            }
            next
        }
        outlier_result_s <- outlier_pca_lof(
            df_s[, target_cols, drop = FALSE],
            return_ggplot = return_ggplots,
            verbose = FALSE
        )

        excluded_ids_s <- outlier_result_s$id_samples_outlier
        excluded_ids_all <- c(excluded_ids_all, excluded_ids_s)

        if (isTRUE(return_ggplots)) {
            plot_list[[as.character(lev)]] <- outlier_result_s$plot_samples_outlier
        }

        # Restore NA values is done globally after we know final exclusions
        # (to avoid doing it twice)
    }
    excluded_ids_all <- unique(excluded_ids_all)

    df_filtered_non_qc <- df_non_qc[!rownames(df_non_qc) %in% excluded_ids_all, , drop = FALSE]

    if (restore_missing_values) {
        na_mask_non_qc <- is.na(df_non_qc[, target_cols, drop = FALSE])

        na_mask_filtered <- na_mask_non_qc[!rownames(df_non_qc) %in% excluded_ids_all, , drop = FALSE]
        df_filtered_non_qc[, target_cols][na_mask_filtered] <- NA
    }

    df_final <- rbind(df_filtered_non_qc, df_qc)
    df_final <- df_final[intersect(rownames(df), rownames(df_final)), , drop = FALSE]

    list(
        df_filtered = df_final,
        plot_samples_outlier = if (isTRUE(return_ggplots)) plot_list else NULL,
        excluded_ids = excluded_ids_all
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
