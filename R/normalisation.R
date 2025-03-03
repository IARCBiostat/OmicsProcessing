#' Normalize feature Data Using Residual Mixed Models
#'
#' This function normalizes feature data by removing unwanted effects using mixed models.
#' It accounts for random effects and fixed effects specified by the user, and optionally
#' corrects for heteroskedasticity. Residuals are calculated with `FUNnormalization_residualMixedModels()`.
#' The method is details in [https://www.mdpi.com/2218-1989/11/9/631](https://www.mdpi.com/2218-1989/11/9/631)
#'
#' @param list A list containing the following elements:
#' \describe{
#'   \item{data_features}{A data frame or tibble of dimensions n x (K+p) with:
#'     \describe{
#'       \item{n}{Number of observations.}
#'       \item{p}{Number of features.}
#'       \item{K}{Number of variables used for unique identification of individuals.}
#'     }}
#'   \item{data_samples}{A data frame or tibble of dimensions n x (K+d) with:
#'     \describe{
#'       \item{n}{Number of observations.}
#'       \item{K}{Number of unique identifiers.}
#'       \item{d}{Additional variables useful in final analysis (e.g., country, age, BMI).}
#'     }}
#'   \item{data_meta_features}{A p x 3 matrix indicating each feature's Name, Class, and Type.}
#' }
#' @param forIdentifier A character vector of strings indicating the names of variables used for unique identification of individuals.
#' @param listRandom A character vector of strings containing variable names modeled as random effects to be removed. If not NULL, should be either of length 1 or contain nested variables.
#' @param listFixedToKeep A character vector of strings containing variable names modeled as fixed effects to be kept.
#' @param listFixedToRemove A character vector of strings containing variable names modeled as fixed effects to be removed.
#' @param HeteroSked A string or NULL. If not NULL, the name of the variable for which heteroskedasticity will be accounted for. Must be included in `listRandom`.
#'
#' @return A list with:
#'   \item{data}{A tibble with unwanted variation removed.}
#'   \item{data_samples}{The input 'data_samples' data frame, ordered by IdentifierPipeline.}
#'   \item{data_meta_features}{The input 'data_meta_features' matrix.}
#'
#' @export
normalization_residualMixedModels <- function(list,
                                              forIdentifier = c("Study", "Batch", "CaseSet", "Idepic"),
                                              listRandom = NULL,
                                              listFixedToKeep = NULL,
                                              listFixedToRemove = NULL,
                                              HeteroSked = NULL) {

  # Convert list of data frames to tibble and unite identifiers ====
  data_features <- tibble::as_tibble(list$data_features) %>%
    tidyr::unite(IdentifierPipeline, tidyselect::all_of(forIdentifier), sep = "", remove = FALSE) %>%
    dplyr::arrange(IdentifierPipeline)
  data_samples <- tibble::as_tibble(list$data_samples) %>%
    tidyr::unite(IdentifierPipeline, tidyselect::all_of(forIdentifier), sep = "", remove = FALSE) %>%
    dplyr::arrange(IdentifierPipeline)
  data_meta_features <- tibble::as_tibble(list$data_meta_features)

  # Check for consistency between data_features and data_samples ====
  if (sum(data_features$IdentifierPipeline == data_samples$IdentifierPipeline) < nrow(data_features)) {
    stop("col_samples should be the same in data_features and data_samples")
  }

  # Merge data_features with data_samples and select relevant columns ====
  var.context <- unique(c("IdentifierPipeline", forIdentifier, listRandom, listFixedToKeep, listFixedToRemove))
  data <- dplyr::left_join(data_features,
                           data_samples %>%
                             dplyr::select(tidyselect::all_of(var.context)) %>%
                             dplyr::select(-tidyselect::any_of(forIdentifier)),
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
    dplyr::arrange(IdentifierPipeline) %>%
    dplyr::select(-IdentifierPipeline)
  data_samples <- data_samples %>%
    dplyr::arrange(IdentifierPipeline) %>%
    dplyr::select(-IdentifierPipeline)

  return(list(data = data.context, data_samples = data_samples, data_meta_features = data_meta_features))
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
