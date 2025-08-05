#' Hybrid Imputation: Random Forest + LCMD
#'
#' This function performs hybrid imputation on selected columns of a data frame by combining
#' Random Forest (RF) imputation (via \code{missForest}) and left-censored missing data (LCMD)
#' imputation (via \code{imputeLCMD}). It selects the method per feature based on the
#' out-of-bag error (OOBE) from the RF model.
#'
#' @param df A data frame with samples (rows) and features (columns).
#' @param target_cols A character vector of column names or a single regular expression to identify target columns for imputation. If NULL, all columns are considered.
#' @param is_qc A logical vector indicating which rows are QC samples. Must match \code{nrow(df)}.
#' @param method Imputation strategy to use (currently only \code{"RF-LCMD"} supported).
#' @param oobe_threshold Numeric. Features with OOBE below this threshold will use RF, others will use LCMD.
#' @param control_RF A named list of control arguments for \code{missForest::missForest()}. Also supports \code{n_cores} (internal).
#' @param control_LCMD A named list of control arguments for \code{imputeLCMD::impute.MAR.MNAR()}, including \code{mode = "overall"} or \code{"column-wise"}.
#'
#' @return A named list with the following components:
#' \describe{
#'   \item{hybrid_rf_lcmd}{The fully imputed data frame combining RF and LCMD decisions.}
#'   \item{rf}{The RF-imputed data frame (non-QC rows only, in full column structure).}
#'   \item{lcmd}{The LCMD-imputed data frame (non-QC rows only, in full column structure).}
#'   \item{oob}{A named numeric vector of feature-level OOB errors from RF.}
#' }
#'
#' @seealso \code{\link{run_rf_imputation}}, \code{\link{run_lcmd_imputation}}
#'
#' @export
hybrid_imputation <- function(df,
                              target_cols = NULL,
                              is_qc = NULL,
                              method = c("RF-LCMD"),
                              oobe_threshold = 0.1,
                              control_RF = list(),
                              control_LCMD = list()) {
    method <- match.arg(method)

    if (is.null(is_qc)) {
        is_qc <- rep(FALSE, nrow(df))
    }
    if (length(is_qc) != nrow(df)) {
        stop("`is_qc` must be a logical vector with the same length as nrow(df).")
    }

    target_cols <- resolve_target_cols(df, target_cols)

    df_non_qc <- df[!is_qc, , drop = FALSE]
    df_qc <- df[is_qc, , drop = FALSE]



    # --- RF Imputation ---
    rf_result <- run_rf_imputation(df_non_qc, target_cols, control_RF)
    df_rf <- rf_result$imputed
    oob <- rf_result$oob

    # --- LCMD Imputation ---
    df_lcmd <- run_lcmd_imputation(df_non_qc, target_cols, control_LCMD)

    # --- Combine RF and LCMD based on OOBE ---
    hybrid <- df_non_qc[, target_cols, drop = FALSE]

    for (feature in target_cols) {
        if (!is.na(oob[feature]) && oob[feature] < oobe_threshold) {
            hybrid[[feature]] <- df_rf[[feature]]
        } else {
            hybrid[[feature]] <- df_lcmd[[feature]]
        }
    }

    return(
        list(
            hybrid_rf_lcmd = {
                df_hybrid <- df
                df_hybrid[!is_qc, target_cols] <- hybrid
                df_hybrid
            },
            rf = {
                df_rf_all <- df
                df_rf_all[!is_qc, target_cols] <- df_rf
                df_rf_all
            },
            lcmd = {
                df_lcmd_all <- df
                df_lcmd_all[!is_qc, target_cols] <- df_lcmd
                df_lcmd_all
            },
            oob = oob
        )
    )
}
