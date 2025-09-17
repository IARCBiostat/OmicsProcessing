#' Remove outliers (optionally stratified) using PCA + LOF
#'
#' @description
#' Wrapper around `outlier_pca_lof()` with conveniences:
#' - Select a subset of columns (`target_cols`)
#' - Exclude QC rows from outlier detection
#' - Temporarily impute missing values (half of column minimum) for detection
#' - Apply detection independently within user-defined strata (`strata`)
#'
#' @details
#' ## Stratification
#' Set `strata` to:
#' - `NULL` (default) to run detection once over all non-QC rows, or
#' - a single column name in `df`, or
#' - an external vector (length `nrow(df)`) to group samples.
#'
#' Outlier detection is performed **independently within each stratum** on non-QC rows
#' (QC rows are always excluded from detection but retained in the output). Strata with
#' fewer than 5 non-QC samples are skipped (no outliers removed for that stratum).
#'
#' ## Missing-data policy
#' - If `impute_method = NULL` and any `target_cols` contain missing values among non-QC rows,
#'   the function **errors** and lists the affected columns with counts. Enable
#'   `impute_method = "half-min-value"` or resolve missingness beforehand.
#' - If `impute_method = "half-min-value"`:
#'   - The function first checks for target columns that are **entirely `NA` across all non-QC rows**.
#'     If any exist, it **errors** (a half-minimum cannot be computed).
#'   - It then checks, **per stratum**, for target columns that are entirely `NA` **within that stratum**.
#'     If any are found, a **warning** is emitted listing the affected strata and columns, and
#'     **temporary imputation is applied on the whole non-QC dataset (ignoring stratification)**,
#'     while outlier detection still runs **per stratum** as requested.
#' - After outlier removal, if `restore_missing_values = TRUE`, the original `NA`s in `target_cols`
#'   are restored in the returned data.
#'
#' @param df A data.frame with features in columns and samples in rows.
#' @param target_cols Character vector of column names (or tidyselect helpers if supported by
#'   `resolve_target_cols()`). If `NULL`, uses `resolve_target_cols(df, NULL)` to infer targets.
#' @param is_qc Logical vector (length `nrow(df)`) marking QC rows to exclude from detection.
#'   Defaults to all `FALSE`.
#' @param method Character; currently supports `"pca-lof-overall"` (default behavior).
#' @param impute_method `NULL` or `"half-min-value"`. When set, missing values in `target_cols`
#'   are imputed as half the minimum non-missing value—by default **within each stratum**; if any
#'#'   stratum has a target column entirely `NA`, imputation is performed **globally** on non-QC rows
#'   (see Missing-data policy).
#' @param restore_missing_values Logical; if `TRUE`, original `NA`s in `target_cols` are restored
#'   after filtering.
#' @param return_ggplots Logical; if `TRUE`, returns a named list of ggplots per stratum.
#' @param strata `NULL` (default), a single column name in `df`, or an external vector of length
#'   `nrow(df)`. When provided, outlier detection is run independently within each stratum
#'   (QC rows excluded within the stratum).
#'
#' @return A list with:
#' \describe{
#'   \item{df_filtered}{`df` with detected outlier rows removed (QC rows always retained).}
#'   \item{excluded_ids}{Character vector of row names removed (union across strata).}
#'   \item{plot_samples_outlier}{If `return_ggplots = TRUE`, a named list of ggplot objects per stratum; otherwise `NULL`.}
#' }
#'
#' @export
#'
#' @examples
#' # 1) No stratification
#' remove_outliers(
#'   df,
#'   target_cols = c("f1","f2"),
#'   impute_method = "half-min-value"
#' )
#'
#' # 2) Stratify by a column in df
#' remove_outliers(
#'   df,
#'   target_cols = c("f1","f2"),
#'   strata = "batch",
#'   impute_method = "half-min-value"
#' )
#'
#' # 3) Stratify by an external vector
#' my_strata <- c("A", "A", "B", "B", "B", "C", "C")
#' remove_outliers(
#'   df,
#'   target_cols = c("f1","f2"),
#'   strata = grp,
#'   impute_method = "half-min-value"
#' )
#'
#' # 4) Stratum with all-NA target columns -> triggers global temporary imputation (warning)
#' \donttest{
#' df2 <- data.frame(
#'   f1 = c(1, 2, 3, NA, NA, NA),
#'   f2 = c(2, 3, 4, NA, NA, NA),
#'   batch = c("A","A","A","B","B","B")
#' )
#' rownames(df2) <- paste0("s", seq_len(nrow(df2)))
#' remove_outliers(
#'   df2,
#'   target_cols = c("f1","f2"),
#'   strata = "batch",
#'   impute_method = "half-min-value"
#' )
#' }
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
    if (!is.null(impute_method)) {
        impute_method <- match.arg(impute_method, choices = c("half-min-value"))
        # impute_method <- NULL
    } 
    # else {
    # }

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

    tgt_non_qc <- df_non_qc[, target_cols, drop = FALSE]

    if (is.null(impute_method)) {
        na_counts <- colSums(is.na(tgt_non_qc))
        if (any(na_counts > 0)) {
            na_counts <- na_counts[na_counts > 0]
            na_counts <- na_counts[order(na_counts, decreasing = TRUE)]
            cols_with_na <- names(na_counts)
            details <- paste0(cols_with_na, " (", na_counts, ")")
            stop(
                paste0(
                    "Missing values detected in `target_cols` among non-QC rows while `impute_method = NULL`.\n",
                    "Columns with missing values [n_missing]: ",
                    paste(details, collapse = ", "), ".\n",
                    "Either remove missingness beforehand or call with `impute_method = \"half-min-value\"`."
                ),
                call. = FALSE
            )
        }
    }
    
    use_global_imputation <- FALSE
    per_stratum_all_na <- list()

    if (identical(impute_method, "half-min-value")) {
        # If any target column is all NA across the whole non-QC dataset -> cannot impute
        cols_all_na_glob <- vapply(tgt_non_qc, function(x) all(is.na(x)), logical(1))
        if (any(cols_all_na_glob)) {
            stop(
                "The following `target_cols` are entirely missing across non-QC rows: ",
                paste(names(cols_all_na_glob)[cols_all_na_glob], collapse = ", "),
                ". Temporary imputation with half-min cannot proceed."
            )
        }

        # Scan strata: if any (stratum, col) is all NA (non-QC rows within stratum), we flip to global imputation
        levs <- levels(strata_vec)
        for (lev in levs) {
            idx_all <- which(strata_vec == lev)
            idx_non_qc_stratum <- idx_all[!is_qc[idx_all]]
            if (length(idx_non_qc_stratum) == 0L) next

            sub <- df[idx_non_qc_stratum, target_cols, drop = FALSE]
            cols_all_na <- vapply(sub, function(x) all(is.na(x)), logical(1))
            if (any(cols_all_na)) {
                use_global_imputation <- TRUE
                per_stratum_all_na[[as.character(lev)]] <- names(cols_all_na)[cols_all_na]
            }
        }

        if (use_global_imputation) {
            msg <- paste0(
                "\n\nDetected strata with columns entirely missing among non-QC rows. ",
                "Applying temporary imputation on the whole non-QC dataset (ignoring stratification). ",
                "Affected strata and columns:\n",
                "strat : columns\n",
                paste(
                    vapply(names(per_stratum_all_na), function(k) {
                        paste0("  - ", k, ": ", paste(per_stratum_all_na[[k]], collapse = ", "))
                    }, character(1)),
                    collapse = "\n"
                )
            )
            warning(msg, call. = FALSE)

            # Prepare a globally imputed copy for non-QC rows
            imputed_non_qc <- df_non_qc
            imputed_non_qc[, target_cols] <- impute_with_half_min(tgt_non_qc)
        }
    }

    excluded_ids_all <- character(0)
    plot_list <- if (isTRUE(return_ggplots)) list() else NULL

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

        if (identical(impute_method, "half-min-value")) {
            if (isTRUE(use_global_imputation)) {
                # take from globally-imputed non-QC copy
                # map rownames to non-QC rows
                rn <- rownames(df_s)
                df_s[, target_cols] <- imputed_non_qc[rn, target_cols, drop = FALSE]
            } else {
                # per-stratum imputation
                df_s[, target_cols] <- impute_with_half_min(df_s[, target_cols, drop = FALSE])
            }
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
