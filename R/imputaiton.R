#' Run Random Forest Imputation with Optional Parallelization
#'
#' This function performs missing value imputation using the \code{missForest} package
#' on a subset of columns in a data frame. Column selection can be explicit or based on a
#' regular expression, using \code{\link{resolve_target_cols}} internally.
#'
#' The function supports optional parallel execution via \code{missForest}'s \code{parallelize}
#' argument. If \code{n_cores} is less than or equal to 1, parallelization is disabled automatically
#' and the imputation runs sequentially.
#'
#' @param df A data frame containing the data to be imputed. Only non-QC rows should be passed here.
#' @param target_cols A character vector of column names or a regex string to identify columns to impute.
#' @param control_RF A named list of additional or overriding arguments passed to \code{missForest::missForest()}.
#'                   Also supports an internal control argument \code{n_cores} (numeric), used to determine
#'                   parallelism when \code{parallelize} is not "no".
#'
#'   Accepted entries include (see \code{missForest::missForest} for details):
#'   \describe{
#'     \item{\code{mtry}}{Number of variables randomly sampled at each split.}
#'     \item{\code{ntree}}{Number of trees to grow.}
#'     \item{\code{maxiter}}{Maximum number of imputation iterations.}
#'     \item{\code{parallelize}}{Character string: "no", "variables", or "forests". Enables parallelization.}
#'     \item{\code{variablewise}}{Logical. If TRUE, variable-wise error is returned.}
#'     \item{\code{verbose}}{Logical. If TRUE, progress messages are printed.}
#'     \item{\code{replace}}{Logical. Whether sampling of observations is done with replacement.}
#'     \item{\code{classwt}, \code{cutoff}, \code{strata}, \code{sampsize}, etc.}{Other optional arguments passed to \code{randomForest::randomForest}.}
#'     \item{\code{n_cores}}{(Internal) Number of CPU cores to use if \code{parallelize} is not "no". This is removed before calling \code{missForest()}.}
#'   }
#'
#' @return A list with two components:
#' \describe{
#'   \item{imputed}{A data frame of the same shape as the input subset, with imputed values.}
#'   \item{oob}{A named numeric vector of out-of-bag error estimates for each feature (if available).}
#' }
#'
#' @seealso \code{\link[missForest]{missForest}}, \code{\link{resolve_target_cols}},
#'   \code{register_cluster}, \code{unregister_cluster}
#'
#' @examples
#' \dontrun{
#' df <- data.frame(a = c(1, NA, 3), b = c(4, 5, NA))
#'
#' # Default imputation on selected columns
#' run_rf_imputation(df, target_cols = c("a", "b"))
#'
#' # Use regex to select columns and customize RF parameters
#' run_rf_imputation(
#'   df,
#'   target_cols = "^a|b$",
#'   control_RF = list(ntree = 200, mtry = 1, n_cores = 4)
#' )
#' }
#'
#' @export
run_rf_imputation <- function(df, target_cols, control_RF = list()) {
    target_cols <- resolve_target_cols(df, target_cols)

    rf_defaults <- list(
        xmis = as.data.frame(df[, target_cols, drop = FALSE]),
        parallelize = "variables",
        mtry = floor(sqrt(length(target_cols))),
        ntree = 100,
        maxiter = 10,
        variablewise = TRUE,
        verbose = TRUE,
        n_cores = parallel::detectCores()
    )
    rf_args_full <- modifyList(rf_defaults, control_RF)

    n_cores <- rf_args_full$n_cores
    if (is.null(n_cores) || !is.numeric(n_cores) || n_cores <= 1) {
        n_cores <- 1
        rf_args_full$parallelize <- "no"
    }

    if (rf_args_full$parallelize != "no") {
        n_cores <- min(n_cores, length(target_cols))
        cl <- register_cluster(n_cores)
        message(paste0("Running missForest::missForest in parallel with ", n_cores, " cores."))
    } else {
        cl <- NULL
        message("Running missForest::missForest in sequential mode (parallelize = 'no').")
    }


    rf_args <- filter_function_args(rf_args_full, missForest::missForest)

    rf_result <- do.call(missForest::missForest, rf_args)

    if (!is.null(cl)) {
        unregister_cluster(cl)
    }

    oob <- rf_result$OOBerror
    if (!is.null(oob) && is.numeric(oob) && length(oob) == length(target_cols)) {
        feature_oob <- setNames(as.numeric(oob), target_cols)
    } else if (!is.null(oob) && is.matrix(oob) && nrow(oob) > 0) {
        # fallback if it's a matrix (older missForest versions)
        oob_df <- as.data.frame(oob)
        colnames(oob_df) <- target_cols
        feature_oob <- oob_df[nrow(oob_df), , drop = TRUE]
    } else {
        warning("No valid OOBError returned from missForest.")
        feature_oob <- setNames(rep(NA, length(target_cols)), target_cols)
    }

    list(imputed = rf_result$ximp, oob = feature_oob)
}
