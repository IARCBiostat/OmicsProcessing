#' Calculate Intraclass Correlation Coefficient (ICC) parallel
#'
#' This function calculates the Intraclass Correlation Coefficient (ICC) for a set of samples and quality controls
#' using a linear mixed-effects model.
#' From: https://pmc.ncbi.nlm.nih.gov/articles/PMC6570933/
#' Based on: https://github.com/courtneyschiffman/Metabolomics-Filtering/blob/master/ICC.R
#'
#' @param df A data frame where rows = features and columns = samples
#' column names should be sample IDs; row names should be feature IDs
#' @param id_samples A vector of column names or indices representing the sample columns in `df`.
#' @param id_qc A vector of column names or indices representing the quality control columns in `df`.
#'
#' @return A named numeric vector of ICC values for each row in `df`.
#' @export
calculate_ICC_parallel <- function(df, id_samples, id_qc) {
  vars1 <- foreach::foreach(i = 1:nrow(df),
                            .packages = 'nlme',
                            .combine = 'rbind', export = 'i') %dopar% {
                              reps <- factor(c(1:length(id_samples),
                                               rep(length(id_samples) + 1,
                                                   length(id_qc))))

                              # Create a data frame
                              data <- data.frame(y = c(as.numeric(df[i, id_samples]),
                                                       as.numeric(df[i, id_qc])),
                                                 reps = reps)

                              # Fit the linear mixed-effects model
                              mm <- nlme::lme(y ~ 1, random = ~1 | reps,
                                              data = data,
                                              na.action = stats::na.omit)

                              # Return variance components
                              return(as.numeric(nlme::VarCorr(mm)[1:2]))
                            }

  # Calculate ICC
  ICC <- apply(vars1, 1, function(x) x[1] / sum(x))
  names(ICC) <- rownames(df)

  return(ICC)
}

#' Calculate Intraclass Correlation Coefficient (ICC)
#'
#' This function calculates the Intraclass Correlation Coefficient (ICC) for a set of samples and quality controls
#' using a linear mixed-effects model.
#' From: https://pmc.ncbi.nlm.nih.gov/articles/PMC6570933/
#' Based on: https://github.com/courtneyschiffman/Metabolomics-Filtering/blob/master/ICC.R
#'
#' @param df A data frame where rows = features and columns = samples
#' column names should be sample IDs; row names should be feature IDs
#' @param id_samples A vector of column names or indices representing the sample columns in `df`.
#' @param id_qc A vector of column names or indices representing the quality control columns in `df`.
#'
#' @return A named numeric vector of ICC values for each row in `df`.
#' @export
calculate_ICC <- function(df, id_samples, id_qc) {
  vars1 <- matrix(NA, nrow = nrow(df), ncol = 2) # Initialize vars1
  
  for (i in 1:nrow(df)) {
    reps <- factor(c(1:length(id_samples),
                     rep(length(id_samples) + 1,
                         length(id_qc))))
    
    # Create a data frame
    data <- data.frame(y = c(as.numeric(df[i, id_samples]),
                             as.numeric(df[i, id_qc])),
                       reps = reps)
    
    # Fit the linear mixed-effects model
    mm <- tryCatch({
      nlme::lme(y ~ 1, random = ~1 | reps,
                data = data,
                na.action = stats::na.omit)
    }, error = function(e){
      cat("Error in iteration", i, ":", conditionMessage(e), "\n")
      return(NULL)
    })
    
    if(!is.null(mm)){
      # Return variance components
      vars1[i, ] <- as.numeric(nlme::VarCorr(mm)[1:2])
    } else {
      vars1[i, ] <- c(NA, NA)
    }
  }
  
  # Calculate ICC, handling NA values
  ICC <- apply(vars1, 1, function(x) {
    if (any(is.na(x))) {
      return(NA)
    } else {
      return(x[1] / sum(x))
    }
  })
  
  names(ICC) <- rownames(df)
  
  return(ICC)
}
