% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normalisation.R
\name{FUNnormalization_residualMixedModels}
\alias{FUNnormalization_residualMixedModels}
\title{Compute Residuals Using Mixed Models}
\usage{
FUNnormalization_residualMixedModels(
  df,
  listRandom,
  listFixedToKeep,
  listFixedToRemove,
  HeteroSked,
  i
)
}
\arguments{
\item{df}{A data frame with feature measurements and relevant variables.}

\item{listRandom}{A character vector of strings specifying random effect variables.}

\item{listFixedToKeep}{A character vector of strings specifying fixed effect variables to keep.}

\item{listFixedToRemove}{A character vector of strings specifying fixed effect variables to remove.}

\item{HeteroSked}{A string or NULL. If not NULL, specifies the variable for heteroskedasticity correction.}

\item{i}{A string specifying the current feature being processed.}
}
\value{
A numeric vector of residuals for the given feature.
}
\description{
This function calculates residuals from a mixed model to remove unwanted effects.
}
