#' Normalize Feature Data Using Residual Mixed Models
#'
#' Normalises omics feature data by fitting a per-feature linear mixed model
#' (`Y ~ listFixedToKeep + listFixedToRemove + (1 | listRandom)`) via
#' \code{lme4::lmer()} (or \code{nlme::lme()} with \code{varIdent} variance
#' weights when \code{HeteroSked} is supplied), and reconstructing each feature
#' as the fitted contribution of \code{listFixedToKeep} plus the model
#' residuals. This removes technical random effects (\code{listRandom}) and
#' unwanted fixed effects (\code{listFixedToRemove}) while preserving biological
#' covariates (\code{listFixedToKeep}). Residuals are computed with
#' \code{FUNnormalization_residualMixedModels()}. The method is detailed in
#' \url{https://www.mdpi.com/2218-1989/11/9/631}.
#'
#' @param list A named list containing three elements:
#' \describe{
#'   \item{data_features}{A data frame or tibble of dimensions n x (K+p) with:
#'     \describe{
#'       \item{n}{Number of observations.}
#'       \item{p}{Number of features.}
#'       \item{K}{Number of variables used for unique identification of individuals.}
#'     }}
#'   \item{data_samples}{A data frame or tibble of dimensions n x (K+d) containing
#'     the sample identifier(s) and all covariates referenced in
#'     \code{listRandom}, \code{listFixedToKeep}, \code{listFixedToRemove}, and
#'     \code{HeteroSked}.}
#'   \item{data_meta_features}{A data frame containing at least a \code{Name}
#'     column listing the feature column names in \code{data_features} to be
#'     normalised (other metadata columns are not used by the mixed model).}
#' }
#' @param identifier A character vector of column name(s) uniquely identifying
#'   samples in both \code{data_features} and \code{data_samples}.
#' @param listRandom Character vector of column name(s) in \code{data_samples}
#'   modelled as random intercepts and **removed** (e.g., \code{"plate"},
#'   \code{"batch"}, or nested grouping variables).
#' @param listFixedToKeep Character vector of column name(s) in
#'   \code{data_samples} modelled as fixed effects whose contribution is
#'   **retained** in the normalised output (e.g., biological covariates such as
#'   \code{c("age", "center")}).
#' @param listFixedToRemove Optional character vector of column name(s) in
#'   \code{data_samples} modelled as fixed effects and **removed** (e.g.,
#'   nuisance covariates such as \code{"sex"} or \code{"fasting_status"}).
#' @param HeteroSked Optional character scalar naming a grouping column in
#'   \code{data_samples} (must also be in \code{listRandom}) for which
#'   group-specific residual variances are modelled via \code{nlme::lme()}.
#'
#' @return A tibble containing the sample identifier column(s) and the
#'   normalised feature columns in the same column order as
#'   \code{list$data_features}.
#'
#' @export
normalization_residualMixedModels <- function(list,
                                              identifier = c("ID_sample"),
                                              listRandom = NULL,
                                              listFixedToKeep = NULL,
                                              listFixedToRemove = NULL,
                                              HeteroSked = NULL) {

  # Convert list of data frames to tibble and unite identifiers ====
  data_features <- tibble::as_tibble(list$data_features) %>%
    tidyr::unite(IdentifierPipeline, tidyselect::all_of(identifier), sep = "", remove = FALSE) %>%
    dplyr::arrange(IdentifierPipeline)
  data_samples <- tibble::as_tibble(list$data_samples) %>%
    tidyr::unite(IdentifierPipeline, tidyselect::all_of(identifier), sep = "", remove = FALSE) %>%
    dplyr::arrange(IdentifierPipeline)
  data_meta_features <- tibble::as_tibble(list$data_meta_features)

  # Check for consistency between data_features and data_samples ====
  if (sum(data_features$IdentifierPipeline == data_samples$IdentifierPipeline) < nrow(data_features)) {
    stop("col_samples should be the same in data_features and data_meta_samples")
  }

  # Merge data_features with data_samples and select relevant columns ====
  var.context <- unique(c("IdentifierPipeline", identifier, listRandom, listFixedToKeep, listFixedToRemove))
  data <- dplyr::left_join(data_features,
                           data_samples %>%
                             dplyr::select(tidyselect::all_of(var.context)) %>%
                             dplyr::select(-tidyselect::any_of(identifier)),
                           by = "IdentifierPipeline") %>%
    dplyr::select(tidyselect::all_of(var.context), tidyselect::all_of(colnames(list$data_features)[which(colnames(list$data_features) %in% data_meta_features$Name)]))

  # Prepare data for modeling
  data <- reshape2::melt(data,
                         id.vars = which(colnames(data) %in% var.context),
                         value.name = "Y",
                         variable.name = "id_feature")
  data <- data %>%
    dplyr::arrange(dplyr::across(c("id_feature", tidyselect::all_of(var.context))))
  data.context <- data[1:table(data$id_feature)[[1]], 1:length(var.context)]

  # Remove unwanted effects using mixed models
  id_feature <- unique(data$id_feature)
  for (i in id_feature) {
    cat(i, "\n")
    df <- data %>%
      dplyr::filter(id_feature == i)
    res <- FUNnormalization_residualMixedModels(df = df,
                                                listRandom = listRandom,
                                                listFixedToKeep = listFixedToKeep,
                                                listFixedToRemove = listFixedToRemove,
                                                HeteroSked = HeteroSked,
                                                i = i)
    data.context <- cbind(data.context, res)
  }

  # Finalize output
  colnames(data.context) <- c(var.context, as.character(id_feature))
  data.context <- tibble::as_tibble(data.context) %>%
    dplyr::select(dplyr::all_of(names(list$data_features)))

  return(data.context)
}

#' Compute Residuals Using Mixed Models
#'
#' This function calculates residuals from a mixed model to remove unwanted effects.
#'
#' @param df A data frame with feature measurements and relevant variables.
#' @param listRandom A character vector of strings specifying random effect variables.
#' @param listFixedToKeep A character vector of strings specifying fixed effect variables to keep.
#' @param listFixedToRemove A character vector of strings specifying fixed effect variables to remove.
#' @param HeteroSked A string or NULL. If not NULL, specifies the variable for heteroskedasticity correction.
#' @param i A string specifying the current feature being processed.
#'
#' @return A numeric vector of residuals for the given feature.
#'
#' @export
FUNnormalization_residualMixedModels <- function(df,
                                                 listRandom,
                                                 listFixedToKeep,
                                                 listFixedToRemove,
                                                 HeteroSked,
                                                 i) {

  listFixedInit <- c(listFixedToKeep, listFixedToRemove)
  listFixed <- if (is.null(listFixedInit)) "1" else listFixedInit
  y <- df$Y

  # Prepare data for model fitting
  df_lmer0 <- cbind.data.frame(y = y, df[, c(listRandom, listFixedInit)])
  colnames(df_lmer0)[-1] <- c(listRandom, listFixedInit)
  indNoNAs <- which(rowSums(is.na(df_lmer0)) == 0)
  df_lmer <- df_lmer0[indNoNAs, ]
  Y1 <- rep(NA, length(y))

  if (!is.null(listFixedToKeep)) {
    df_temp <- data.frame(df_lmer[, listFixedToKeep])
    colnames(df_temp) <- listFixedToKeep
    XFixedToKeep <- stats::model.matrix(~ ., data = df_temp)
  }

  if (is.null(HeteroSked)) {
    # Fit mixed model without heteroskedasticity correction
    textformulaMixed <- paste("y ~", paste0(listFixed, collapse = "+"),
                              paste0(" + (1|", paste0(listRandom, collapse = ") + (1|"), ")"))
    tochecklmer <- tryCatch(lme4::lmer(stats::as.formula(textformulaMixed), data = df_lmer,
                                       control = lme4::lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))),
                            error = function(e) NULL)

    if (!is.null(tochecklmer)) {
      if (!is.null(listFixedToKeep)) {
        Y1[indNoNAs] <- XFixedToKeep %*% matrix(lme4::fixef(tochecklmer)[names(lme4::fixef(tochecklmer)) %in% colnames(XFixedToKeep)], ncol = 1) + stats::resid(tochecklmer)
      } else {
        Y1[indNoNAs] <- mean(y) + stats::resid(tochecklmer)
      }

      # Check for collinearity (VIF)
      cat("## Collinearity (VIF) for:", i, "\n")
      vif_values <- car::vif(tochecklmer)
      print(vif_values)

      # Check the random effects variance
      cat("## Random effects variance for:", i, "\n")
      rand_var <- lme4::VarCorr(tochecklmer)
      print(rand_var)

      # Check for singularity
      cat("## Model for", i, "is singular:", lme4::isSingular(tochecklmer), "\n")

    } else {
      cat("## Model did not converge for:", i, "\n")
    }
  } else {
    # Fit mixed model with heteroskedasticity correction
    listrandomeffect <- lapply(listRandom, function(x) stats::as.formula(paste0("~1|", x)))
    forweights <- stats::as.formula(paste0("~1|", HeteroSked))
    textformulaFixedHeteroSked <- paste("y ~", paste0(listFixed, collapse = "+"))

    tochecklme <- tryCatch(nlme::lme(stats::as.formula(textformulaFixedHeteroSked), random = listrandomeffect, weights = nlme::varIdent(form = forweights),
                                     data = df_lmer,
                                     control = list(maxIter = 900, msMaxIter = 900, niterEM = 900, msMaxEval = 900, opt = "optim")),
                           error = function(e) NULL)

    if (!is.null(tochecklme)) {
      forrescale <- stats::sd(stats::residuals(tochecklme, type = "pearson")) / stats::sd(stats::residuals(tochecklme))
      if (!is.null(listFixedToKeep)) {
        Y1[indNoNAs] <- XFixedToKeep %*% matrix(nlme::fixed.effects(tochecklme)[names(nlme::fixed.effects(tochecklme)) %in% colnames(XFixedToKeep)], ncol = 1) +
          stats::residuals(tochecklme, type = "pearson") / forrescale
      } else {
        Y1[indNoNAs] <- mean(y) + stats::residuals(tochecklme, type = "pearson") / forrescale
      }

      # Check for collinearity (VIF)
      cat("## Collinearity (VIF) for:", i, "\n")
      vif_values <- car::vif(tochecklme)
      print(vif_values)

      # Check the random effects variance
      cat("## Random effects variance for:", i, "\n")
      rand_var <- nlme::VarCorr(tochecklme)
      print(rand_var)

      # Check for singularity (only applies to lmer models, so not applicable here)
      cat("## Singular check not applicable to nlme models for:", i, "\n")
    } else {
      cat("## Model did not converge for:", i, "\n")
    }
  }

  return(Y1)
}
