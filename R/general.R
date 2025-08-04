#' Resolve Target Columns from Names or Regex
#'
#' This utility function determines which columns from a data frame should be
#' selected for downstream operations based on a user-supplied specification.
#' The specification can be:
#' 
#' - `NULL`: Selects all columns.
#' - A character vector of column names: Selects those columns explicitly.
#' - A single string with a regular expression: Selects all matching column names.
#'
#' @param df A data frame from which columns will be selected.
#' @param target_cols A character vector of column names or a single regular expression string.
#'
#' @return A character vector of resolved column names present in `df`.
#'
#' @examples
#' df <- data.frame(a = 1, b = 2, c_score = 3)
#' resolve_target_cols(df, NULL)         # returns all columns
#' resolve_target_cols(df, "c_")         # regex match (returns "c_score")
#' resolve_target_cols(df, c("a", "b"))  # exact names
resolve_target_cols <- function(df, target_cols) {
  if (is.null(target_cols)) {
    return(names(df))
  }

  if (length(target_cols) == 1) {
    # Treat as regex pattern
    matched <- grep(target_cols, names(df), value = TRUE)
    if (length(matched) == 0) {
      stop("No columns matched the regular expression in `target_cols`.")
    }
    return(matched)
  }

  # Treat as explicit column names
  missing_cols <- setdiff(target_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("The following `target_cols` are not in the dataframe: ",
         paste(missing_cols, collapse = ", "))
  }

  return(target_cols)
}
