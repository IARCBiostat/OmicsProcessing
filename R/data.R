#' Simulated Dataframe 1
#'
#' A simulated dataframe with 100 rows and 101 columns. Col1 is an ID column.
#' The remaining columns are features with randomly assigned values. Once values
#' were assigned, feature missingness was introduced (5 percent) randomly for feature_2-feature_92.
#' feature_93-feature_100 contains missing at 10, 20, 30, 40, 50, 60, 70, 80, and 90 perceent, respectively.
#' Sample missingness was then intriduced for ID_90-ID_100 at 10, 20, 30, 40, 50, 60, 70, 80, 90, and 100 perceent, respectively.
#' Missing values for ID_1 were then replaced with the mean of the feature to give ID_1 no missingness
#'
#' @format A data frame with 100 rows and 101 variables:
#' @usage data(data_features)
"data_features"

#' Simulated Dataframe 2
#'
#' A dataframe summarizing missing values and minimum values from df1.
#'
#' @format A data frame with 100 rows and 3 variables:
#' \describe{
#'   \item{ID_feature}{Feature names from data}
#'   \item{missing_pct}{percentage of missing values for each feature}
#'   \item{LOD}{limit of detection is the minimum feature value / 2}
#' }
#' @usage data(data_meta_features)
"data_meta_features"

#' Simulated Dataframe 3
#'
#' A dataframe with ID, batch, sex, and age information.
#'
#' @format A data frame with 100 rows and 4 variables:
#' \describe{
#'   \item{ID_sample}{ID from ID_sample column in data}
#'   \item{batch}{randomly assigned levels (N = 10)}
#'   \item{sex}{randomly assigned 1 or 2}
#'   \item{age}{mean 55; SD 10}
#' }
#' @usage data(data_meta_samples)
"data_meta_samples"
