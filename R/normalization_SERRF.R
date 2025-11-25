#' SERRF Normalization
#'
#' Applies the SERRF normalization method to remove unwanted variation (Fan 2019, Anal Chem).
#'
#' @param df A data frame or tibble.
#' @param target_cols Feature columns to normalize (character vector or selector passed to `resolve_target_cols`).
#' @param is_qc Logical vector indicating which rows are QC samples. Must be the same length as the number of rows in `df`.
#' @param strata_col Name of the column containing batch/strata IDs. Must exist in `df` and be a factor with no NA values.
#' @param num_screen_SERRF Number of correlated features to use in model fitting. Default is 10.
#'
#' @return A normalized data frame.
#' @export
normalise_SERRF <- function(df, target_cols = NULL, is_qc = NULL, strata_col, num_screen_SERRF = 10) {
    # ---- Validation ----
    if (missing(strata_col) || is.null(strata_col) || !nzchar(strata_col)) {
        stop("You must provide the name of the batch/strata column using the `strata_col` argument.")
    }
    if (!strata_col %in% colnames(df)) {
        stop(paste0("Column '", strata_col, "' was not found in the input data frame."))
    }

    if (!is.factor(df[[strata_col]])) {
        strata_col_factor <- paste0(strata_col, "_factor")
        df[[strata_col_factor]] <- as.factor(df[[strata_col]])
        strata_col <- strata_col_factor
    }

    target_cols <- resolve_target_cols(df, target_cols)

    na_in_features <- sapply(df[, target_cols, drop = FALSE], anyNA)
    if (any(na_in_features)) {
        stop(paste0(
            "Some feature columns contain NA values:\n",
            paste(names(na_in_features)[na_in_features], collapse = ", "),
            "\nPlease impute or remove missing values before normalization."
        ))
    }

    non_numeric <- !vapply(df[, target_cols], is.numeric, logical(1))
    if (any(non_numeric)) {
        stop(
            "All selected feature columns must be numeric. Non-numeric columns: ",
            paste(target_cols[non_numeric], collapse = ", ")
        )
    }

    # ---- Initialization ----
    batch_levels <- levels(df[[strata_col]])
    df[["__is_qc"]] <- is_qc

    qc_orig <- as.matrix(df[df[["__is_qc"]], target_cols, drop = FALSE])
    sample_orig <- as.matrix(df[!df[["__is_qc"]], target_cols, drop = FALSE])

    df_normalised <- df
    corrs_train <- list()
    corrs_target <- list()

    # ---- Precompute Correlations ----
    for (batch in batch_levels) {
        batch_mask <- df[[strata_col]] == batch
        train <- df[batch_mask & df[["__is_qc"]], target_cols, drop = FALSE] |> as.matrix()
        target <- df[batch_mask & !df[["__is_qc"]], target_cols, drop = FALSE] |> as.matrix()

        corrs_train[[batch]] <- cor(train, method = "spearman")
        corrs_target[[batch]] <- cor(target, method = "spearman")
    }

    # ---- Normalize Each Feature ----
    for (j in seq_along(target_cols)) {
        if (j %% 250 == 1) cat("Feature", j, "\n")
        feature_name <- target_cols[j]

        for (batch in batch_levels) {
            batch_mask <- df[[strata_col]] == batch
            batch_data <- df[batch_mask, target_cols, drop = FALSE] |> as.matrix()
            train_data <- df[batch_mask & df[["__is_qc"]], target_cols, drop = FALSE]
            target_data <- df[batch_mask & !df[["__is_qc"]], target_cols, drop = FALSE]
            qc_flags <- df[["__is_qc"]][batch_mask]

            corr_train_order <- order(abs(corrs_train[[batch]][, j]), decreasing = TRUE)
            corr_target_order <- order(abs(corrs_target[[batch]][, j]), decreasing = TRUE)

            sel_var <- c()
            l <- num_screen_SERRF
            while (length(sel_var) < num_screen_SERRF) {
                sel_var <- intersect(corr_train_order[1:l], corr_target_order[1:l])
                sel_var <- sel_var[sel_var != j]
                l <- l + 1
            }

            sel_names <- target_cols[sel_var]

            y_train <- scale(train_data[[feature_name]], scale = FALSE)
            x_train <- scale(train_data[, sel_names, drop = FALSE])
            x_test <- scale(target_data[, sel_names, drop = FALSE])

            model <- ranger::ranger(y ~ ., data = data.frame(y = y_train, x_train))
            pred_qc <- model$predictions
            pred_sample <- predict(model, data = data.frame(x_test))$predictions

            norm <- batch_data[, j]
            qc_idx <- which(qc_flags)
            sample_idx <- which(!qc_flags)

            norm[qc_idx] <- norm[qc_idx] / ((pred_qc + mean(norm[qc_idx], na.rm = TRUE)) / mean(norm[qc_idx], na.rm = TRUE))
            norm[sample_idx] <- norm[sample_idx] / ((pred_sample + mean(norm[sample_idx], na.rm = TRUE)) / median(norm[sample_idx], na.rm = TRUE))

            norm[qc_idx] <- norm[qc_idx] / (median(norm[qc_idx], na.rm = TRUE) / median(qc_orig[, j], na.rm = TRUE))
            norm[sample_idx] <- norm[sample_idx] / (median(norm[sample_idx], na.rm = TRUE) / median(sample_orig[, j], na.rm = TRUE))

            norm[!is.finite(norm)] <- rnorm(sum(!is.finite(norm)), sd = sd(norm[is.finite(norm)], na.rm = TRUE) * 0.01)
            df_normalised[batch_mask, feature_name] <- norm
        }

        # Clean final output
        feat_values <- df_normalised[[feature_name]]
        feat_qc <- feat_values[is_qc]
        feat_sample <- feat_values[!is_qc]

        feat_qc[is.na(feat_qc)] <- rnorm(sum(is.na(feat_qc)), mean = min(feat_qc, na.rm = TRUE), sd = sd(feat_qc, na.rm = TRUE) * 0.1)
        feat_qc[feat_qc < 0] <- runif(1) * min(feat_qc[feat_qc > 0], na.rm = TRUE)

        feat_sample[is.na(feat_sample)] <- rnorm(sum(is.na(feat_sample)), mean = min(feat_sample, na.rm = TRUE), sd = sd(feat_sample, na.rm = TRUE) * 0.1)
        feat_sample[feat_sample < 0] <- runif(1) * min(feat_sample[feat_sample > 0], na.rm = TRUE)

        df_normalised[[feature_name]][is_qc] <- feat_qc
        df_normalised[[feature_name]][!is_qc] <- feat_sample
    }

    df_normalised <- df_normalised[, !(names(df_normalised) %in% "__is_qc")]

    return(df_normalised)
}
