#' Cluster features by retention time and summarise within clusters
#'
#' Groups features into clusters based on their retention time using
#' hierarchical clustering, then selects a representative feature for each
#' cluster either via scores or via correlation-based aggregation.
#'
#' @param df
#'   A data.frame (or tibble) with feature data in columns and samples in
#'   rows.
#' @param target_cols
#'   Character vector of feature/column names to use. If \code{NULL},
#'   [resolve_target_cols()] is used to determine the set of features.
#' @param is_qc
#'   Logical vector marking QC samples. Must have length equal to
#'   \code{nrow(df)}. If \code{NULL}, all samples are treated as non-QC.
#'   (Currently only used for consistency with other interfaces; it is not
#'   used directly in this function.)
#' @param rt_height
#'   Numeric height at which to cut the dendrogram in retention-time
#'   space. Smaller values create more clusters.
#' @param method
#'   Character string indicating how to obtain a single representative per
#'   retention-time cluster. Either \code{"correlations"} or \code{"scores"}.
#'   With \code{"scores"}, one existing feature per cluster is selected based
#'   on pre-computed scores. With \code{"correlations"}, clusters are further
#'   processed using correlation-based summarisation.
#' @param scores
#'   Named numeric vector of feature scores, required when
#'   \code{method = "scores"}. Names must coincide with \code{target_cols}.
#'   For each retention-time cluster, the feature  are subclusterd based on the 
#'   input correlation threshold. The feature with the highest score is selected 
#'   as the representative of each subcluster.
#' @param cut_height
#'   Numeric height used inside \code{cluster_features_based_on_correlations}
#'   when \code{method = "correlations"}. Passed on to that function's
#'   \code{cut_height} argument.
#' @param corr_thresh
#'   Numeric threshold used inside \code{cluster_features_based_on_correlations}
#'   when \code{method = "correlations"}. Passed on to that function's
#'   \code{corr_thresh} argument.
#'
#' @return
#'   A list with two elements:
#'
#'   \describe{
#'     \item{\code{clustered_df}}{
#'       A data.frame with one column per final representative feature.
#'       When \code{method = "scores"}, this consists of a subset of the
#'       original feature columns. When \code{method = "correlations"}, it
#'       may contain synthetic features created by
#'       \code{cluster_features_based_on_correlations()}.
#'     }
#'     \item{\code{representatives_map}}{
#'       A named list mapping each representative feature (original or
#'       synthetic) to the character vector of feature names that it
#'       represents within its retention-time cluster.
#'     }
#'   }
#'
#' @details
#' The function proceeds in two steps:
#'
#' \enumerate{
#'   \item Retention-time clustering: feature names in \code{target_cols}
#'     are first parsed using [parse_mass_rt()] to extract retention times,
#'     which are then clustered by \code{cluster_hierarchical()} at height
#'     \code{rt_height}. This yields groups of features with similar
#'     retention times.
#'   \item Within-cluster summarisation:
#'     \itemize{
#'       \item If \code{method = "scores"}, each retention-time cluster is
#'         reduced to a single original feature using
#'         \code{get_features_representatives_based_on_scores()}, based on
#'         the supplied \code{scores}.
#'       \item If \code{method = "correlations"}, each retention-time
#'         cluster is passed as a feature list to
#'         \code{cluster_features_based_on_correlations()}, which may
#'         further split or summarise the cluster into synthetic variables
#'         based on correlations.
#'     }
#' }
#'
#' @seealso
#'   \code{\link{resolve_target_cols}},
#'   \code{\link{parse_mass_rt}},
#'   \code{\link{cluster_hierarchical}},
#'   \code{\link{get_features_representatives_based_on_scores}},
#'   \code{\link{cluster_features_based_on_correlations}}
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   `100@150` = rnorm(20),
#'   `100@151` = rnorm(20),
#'   `101@200` = rnorm(20)
#' )
#'
#' target_cols <- c("100@150", "100@151", "101@200")
#' is_qc <- rep(FALSE, nrow(df))
#'
#' ## Using scores to pick one feature per RT cluster
#' scores <- c(
#'   `100@150` = 0.2,
#'   `100@151` = 0.8,
#'   `101@200` = 0.5
#' )
#'
#' res_scores <- cluster_features_by_retention_time(
#'   df = df,
#'   target_cols = target_cols,
#'   is_qc = is_qc,
#'   rt_height = 0.07,
#'   method = "scores",
#'   scores = scores
#' )
#'
#' ## Using correlation-based summarisation within RT clusters
#' res_corr <- cluster_features_by_retention_time(
#'   df = df,
#'   target_cols = target_cols,
#'   is_qc = is_qc,
#'   rt_height = 0.07,
#'   method = "correlations",
#'   cut_height = 0.26,
#'   corr_thresh = 0.75
#' )
#' }
#'
#' @export
cluster_features_by_retention_time <- function(
    df,
    target_cols = NULL,
    is_qc = NULL,
    rt_height = 0.07,
    method = c("correlations", "scores"),
    scores = NULL,
    cut_height = NULL,
    corr_thresh = NULL) {
  method <- match.arg(method)

  if (is.null(is_qc)) {
    is_qc <- rep(FALSE, nrow(df))
  }
  if (length(is_qc) != nrow(df)) {
    stop("`is_qc` must be a logical vector with the same length as nrow(df).")
  }
  target_cols <- resolve_target_cols(df, target_cols)

  if (method == "scores") {
    if (is.null(names(scores))) {
      stop("`scores` must be a named numeric vector.")
    }
    if (length(target_cols) != length(scores)) {
      error_message <- paste0(
        "Error: `scores` must be the same length as the input `target_cols`:\n",
        "length(scores): ", length(scores), ".\n",
        "length(target_cols): ", length(target_cols), ".\nPlease make sure you are using the same columns.\n",
        "The OmicsProcessing::resolve_target_cols() is used to construct the `target_cols`, please check the output of this funciton agains the input `scores`.\n"
      )
      stop(error_message)
    }
    if (!identical(sort(target_cols), sort(names(scores)))) {
      error_message <- paste0(
        "Error: `scores` names must be the same the input `target_cols.` The following `scores` names were not in `target_cols`:\n",
        setdiff(names(scores), target_cols), ".\n"
      )
      if (length(setdiff(names(scores), target_cols)) > 0L) {
        error_message <- paste0(
          error_message,
          "The following `target_cols` were not in among the `scores` names:\n",
          setdiff(target_cols, names(scores)), ".\n"
        )
        stop(error_message)
      }
    }
  }

  retention_times <- parse_mass_rt(target_cols, "rt")

  cluster_ids <- cluster_hierarchical(retention_times, height = rt_height)

  cluster_feature_list <- split(as.character(target_cols), cluster_ids)
  names(cluster_feature_list) <- sprintf("ClustRT%s", names(cluster_feature_list))


  if (method == "scores") {
    representatives <- get_features_representatives_based_on_scores(
      df,
      cluster_feature_list,
      corr_thresh = corr_thresh,
      scores
    )
    clustered_df <- df[, representatives$representatives]
    representatives_map <- representatives$representatives_map
  }
  if (method == "correlations") {
    results <- cluster_features_based_on_correlations(
      df,
      cluster_feature_list,
      cut_height = cut_height,
      corr_thresh = corr_thresh
    )
    clustered_df <- results$clustered_df
    representatives_map <- results$representatives_map
  }

  return(
    list(
      clustered_df = clustered_df,
      representatives_map = representatives_map
    )
  )
}


#' General hierarchical clustering
#'
#' Performs hierarchical clustering on a numeric vector, matrix or
#' data.frame and returns cluster memberships. The clustering can be cut
#' either at a given height or into a specified number of clusters.
#'
#' @param x Numeric vector, matrix or data.frame to cluster.
#' @param height Numeric dendrogram height at which to cut. Exactly one
#'   of `height` or `k` must be supplied.
#' @param k Integer number of clusters to cut into. Exactly one of
#'   `height` or `k` must be supplied.
#' @param dist_method Character string specifying the distance measure
#'   for [stats::dist()].
#' @param hclust_method Character string specifying the linkage method
#'   for [stats::hclust()].
#'
#' @return An integer vector giving cluster assignments.
#'
#' @examples
#' # Vector input
#' cluster_hierarchical(c(1, 1.1, 5, 5.2), height = 0.3)
#'
#' # Matrix input
#' m <- cbind(
#'   a = c(1, 1.1, 5, 5.2),
#'   b = c(10, 11, 30, 31)
#' )
#' cluster_hierarchical(m, k = 2)
#'
#' @noRd
cluster_hierarchical <- function(x,
                                 height = NULL,
                                 k = NULL,
                                 dist_method = "euclidean",
                                 hclust_method = "complete") {
  if (is.null(height) && is.null(k)) {
    stop("Supply exactly one of `height` or `k`.", call. = FALSE)
  }
  if (!is.null(height) && !is.null(k)) {
    stop("Supply only one of `height` or `k`.", call. = FALSE)
  }

  x_mat <- as.matrix(x)

  dist_obj <- stats::dist(x_mat, method = dist_method)
  tree <- stats::hclust(dist_obj, method = hclust_method)

  if (!is.null(height)) {
    return(stats::cutree(tree, h = height))
  } else {
    return(stats::cutree(tree, k = k))
  }
}


#' Parse mass/retention-time strings
#'
#' Extract mass and retention time (RT) values from strings of the form
#' "mass@rt". Accepts a single string or a vector.
#'
#' @param x Character vector with entries like "123.4@56.7" or, when
#'   \code{clean_rt_suffix = TRUE}, possibly "123.4@56.7:1".
#' @param what Which values to return: "mass", "rt", or "both".
#' @param clean_rt_suffix Logical. If TRUE, removes suffixes such as ":1" or ":2" from the RT part.
#'
#' @return Numeric vector (for "mass" or "rt") or a data.frame with two
#'   columns (for "both").
#'
#' @examples
#' parse_mass_rt("100.5@23.1", what = "mass")
#' parse_mass_rt(c("10.2@1.5", "20.4@2.5"), what = "rt")
#' parse_mass_rt("50@7.5", what = "both")
#'
#' parse_mass_rt("100.5@23.1:1",
#'   what = "rt",
#'   clean_rt_suffix = TRUE
#' )
#'
#' @export
parse_mass_rt <- function(x,
                          what = c("mass", "rt", "both"),
                          clean_rt_suffix = TRUE) {
  what <- match.arg(what)

  parts <- strsplit(x, "@", fixed = TRUE)

  mass_chr <- vapply(parts, function(p) p[1], character(1))
  rt_chr <- vapply(parts, function(p) {
    if (length(p) < 2L) {
      return(NA_character_)
    }
    p[2]
  }, character(1))

  if (clean_rt_suffix) {
    has_suffix <- grepl(":", rt_chr, fixed = TRUE)
    if (any(has_suffix, na.rm = TRUE)) {
      pb <- rt_chr[has_suffix]
      pb_split <- strsplit(pb, ":", fixed = TRUE)
      pb_first <- vapply(
        pb_split,
        function(z) z[1],
        character(1)
      )
      rt_chr[has_suffix] <- pb_first
    }
  }

  masses <- as.numeric(mass_chr)
  rts <- as.numeric(rt_chr)

  if (what == "mass") {
    return(masses)
  }
  if (what == "rt") {
    return(rts)
  }

  return(data.frame(mass = masses, rt = rts))
}





#' Select representative features per cluster using correlation threshold and scores
#'
#' This function selects representative features from pre-defined clusters of
#' variables, using a correlation-based rule and a scoring vector:
#'
#' \itemize{
#'   \item If a cluster contains a single feature, that feature is selected.
#'   \item If a cluster contains two features, the absolute pairwise correlation
#'         is computed. If \code{|cor| >= corr_thresh}, only the highest-scoring
#'         feature is selected; otherwise, both features are kept as representatives.
#'   \item If a cluster contains more than two features, hierarchical clustering
#'         is performed using the distance \code{1 - |cor|} with
#'         \code{method = "average"}. The dendrogram is cut at
#'         \code{h = 1 - corr_thresh} to form subclusters. From each subcluster,
#'         the highest-scoring feature is selected as the representative.
#' }
#'
#' In all cases where a unique representative is chosen for a (sub)cluster,
#' \code{representatives_map} stores the mapping from the representative feature
#' to the set of original features it represents.
#'
#' @param df
#'   A data frame containing the feature columns referenced in
#'   \code{cluster_feature_list}. Column names must match the feature names.
#'
#' @param cluster_feature_list
#'   A list where each element is a non-empty character vector of feature names
#'   belonging to one cluster (e.g., the output of a prior clustering step).
#'
#' @param corr_thresh
#'   A numeric threshold in \code{[0, 1]}. For two-feature clusters, if
#'   \code{|cor| >= corr_thresh}, the feature with the highest score is selected.
#'   For clusters with more than two features, subclusters are created by cutting
#'   the hierarchical tree at \code{h = 1 - corr_thresh}.
#'
#' @param scores
#'   A named numeric vector of feature scores. All features appearing in
#'   \code{cluster_feature_list} must be present in \code{names(scores)}.
#'
#' @return
#'   A list with two components:
#'   \describe{
#'     \item{\code{representatives}}{
#'       A character vector of selected representative features (one per
#'       single-feature cluster; zero, one, or more per multi-feature cluster,
#'       depending on the correlation structure and threshold).
#'     }
#'     \item{\code{representatives_map}}{
#'       A named list mapping each representative feature (when uniquely chosen)
#'       to the character vector of features it represents.
#'     }
#'   }
#'
#' @details
#'   For clusters of size greater than two, the distance matrix is computed as
#'   \code{1 - abs(cor(X))} on the selected columns, followed by
#'   \code{hclust(..., method = "average")}. Subclusters are extracted via
#'   \code{cutree(hc, h = 1 - corr_thresh)}. Within each subcluster, the
#'   representative is the feature with the maximum score.
#'
#'   When a two-feature cluster has \code{|cor| < corr_thresh}, both features
#'   are returned as representatives and no mapping is stored for that pair,
#'   as neither represents the other by design.
#'
#' @examples
#' # Toy data frame
#' set.seed(1)
#' df <- data.frame(
#'   feat_a = rnorm(100),
#'   feat_b = rnorm(100),
#'   feat_c = rnorm(100),
#'   feat_d = rnorm(100),
#'   feat_e = rnorm(100),
#'   feat_f = rnorm(100)
#' )
#'
#' # Example clusters
#' cluster_feature_list <- list(
#'   c("feat_a", "feat_b", "feat_c"), # size > 2 -> hierarchical subclusters
#'   "feat_d", # size = 1 -> itself
#'   c("feat_e", "feat_f") # size = 2 -> pairwise rule
#' )
#'
#' # Scores for all features
#' scores <- c(
#'   feat_a = 0.1,
#'   feat_b = 0.4,
#'   feat_c = 0.3,
#'   feat_d = 0.9,
#'   feat_e = 0.2,
#'   feat_f = 0.8
#' )
#'
#' # Choose a correlation threshold
#' corr_thresh <- 0.7
#'
#' res <- get_features_representatives_based_on_scores(
#'   df = df,
#'   cluster_feature_list = cluster_feature_list,
#'   corr_thresh = corr_thresh,
#'   scores = scores
#' )
#'
#' res$representatives
#' res$representatives_map
#'
#' @export
get_features_representatives_based_on_scores <- function(
    df,
    cluster_feature_list,
    corr_thresh,
    scores) {
  if (!is.list(cluster_feature_list)) {
    stop("`cluster_feature_list` must be a list.")
  }

  if (is.null(names(scores))) {
    stop("`scores` must be a named numeric vector.")
  }
  if (!is.numeric(corr_thresh) || length(corr_thresh) != 1L || is.na(corr_thresh) || corr_thresh < 0 || corr_thresh > 1) {
    stop("`corr_thresh` must be a single numeric value in [0, 1].")
  }

  representatives <- c()
  representatives_map <- vector("list")

  for (i in seq_along(cluster_feature_list)) {
    cluster <- cluster_feature_list[[i]]

    if (!is.character(cluster)) {
      stop(
        "Each element of `cluster_feature_list` must be a ",
        "character vector of feature names."
      )
    }

    missing_scores <- setdiff(cluster, names(scores))
    if (length(missing_scores) > 0L) {
      stop(
        "Some features in cluster ", i,
        " have no score: ",
        paste(missing_scores, collapse = ", ")
      )
    }

    if (length(cluster) == 1L) {
      rep_feature <- cluster[1L]
      representatives <- c(representatives, rep_feature)
    } else if (length(cluster) == 2L) {
      feature_df <- df %>%
        dplyr::select(dplyr::all_of(cluster))

      pair_cor <- stats::cor(feature_df)[1, 2]

      if (pair_cor >= corr_thresh) {
        cluster_scores <- scores[cluster]
        max_idx <- which.max(cluster_scores)
        rep_feature <- cluster[max_idx]
        representatives <- c(representatives, rep_feature)
        representatives_map[[rep_feature]] <- cluster
      } else {
        representatives <- c(representatives, cluster)
      }
    } else {
      feature_matrix <- df %>%
        dplyr::select(dplyr::all_of(cluster)) %>%
        as.matrix()

      dist_mat <- as.dist(1 - abs(cor(feature_matrix)))
      hc <- hclust(dist_mat, method = "average")
      subclusters <- cutree(hc, h = 1 - corr_thresh)
      cluster_list <- split(names(subclusters), subclusters)

      for (j in seq_along(cluster_list)) {
        cluster_j <- cluster_list[[j]]
        if (length(cluster_j) == 1L) {
          rep_feature <- cluster_j[1L]
          representatives <- c(representatives, rep_feature)
        } else {
          cluster_scores <- scores[cluster_j]
          max_idx <- which.max(cluster_scores)
          rep_feature <- cluster_j[max_idx]
          representatives <- c(representatives, rep_feature)
          representatives_map[[rep_feature]] <- cluster_j
        }
      }
    }
  }
  list(
    representatives = representatives,
    representatives_map = representatives_map
  )
}



#' Cluster feature groups based on correlations
#'
#' Given a list of pre-defined feature groups, construct a data set of
#' representative features by collapsing groups with high correlations into
#' synthetic variables.
#'
#' For groups of size one, the original feature is kept as is. For groups of
#' size two, the decision is based on their pairwise correlation. For larger
#' groups, clustering is based on the decrease in homogeneity as in the
#' \pkg{ClustOfVar} package. The homogeneity of a cluster is the squared
#' correlation (for quantitative variables) between the variables and the
#' cluster centre (the first principal component).
#'
#' @param df A tibble or data.frame containing the feature columns (and
#'   possibly other variables not used here). All variables referenced in
#'   \code{cluster_feature_list} must be columns of \code{df}.
#' @param cluster_feature_list A list where each element is a character vector
#'   giving the names of the features that belong to one initial cluster.
#'   Feature names are typically of the form \code{"RT@mass"}, where
#'   \code{RT} and \code{mass} are real numbers corresponding to retention
#'   time and mass, but any valid column names are accepted.
#' @param cut_height Numeric. Cut-off height for the correlation-based
#'   clustering within groups of size three or more (as implemented in
#'   \pkg{ClustOfVar}). A suggested value is \code{0.26}, which corresponds
#'   roughly to a pairwise correlation of about \code{0.75}. You can use a
#'   dedicated helper (e.g. \code{simul_eval_link_hhh_corr}) to explore the
#'   correspondence between pairwise correlations and the tree height returned
#'   by \code{ClustOfVar::hclustvar()}.
#' @param corr_thresh Numeric. Cut-off for the absolute pairwise correlation
#'   used when the number of features in a group is exactly two. If the
#'   absolute correlation between the two features is at least
#'   \code{corr_thresh}, they are replaced by a single synthetic feature
#'   (first principal component); otherwise both original features are kept.
#'
#' @return A list with the following components:
#' \describe{
#'   \item{clustered_df}{A tibble with one row per observation and one column
#'     per final representative feature. For clusters of size one, the column
#'     is the original feature. For size two with correlation above
#'     \code{corr_thresh}, the column is the first principal component of
#'     the two features. For larger clusters, columns are synthetic variables
#'     returned by \pkg{ClustOfVar}, oriented to have a positive correlation
#'     with the first feature of the corresponding sub-cluster.}
#'   \item{representatives_map}{A named list mapping each synthetic feature
#'     (columns whose names start with \code{"SynthFeat@"}) to the character
#'     vector of original feature names that it summarises. Original features
#'     that are kept as is do not appear in this map.}
#' }
#'
#' @details
#' This function is typically used after a pre-grouping step (for example,
#' after retention-time-based grouping). Each element of
#' \code{cluster_feature_list} corresponds to one such pre-defined group and
#' may contain one, two, or more feature names; all cases are handled.
#'
#' For groups of size three or more, a hierarchical clustering tree is first
#' obtained using \code{ClustOfVar::hclustvar()}. The tree is then cut at
#' a data-driven position derived from \code{cut_height}, and
#' \code{ClustOfVar::cutreevar()} is used to obtain synthetic scores for the
#' resulting sub-clusters. Each score is multiplied by \eqn{+1} or \eqn{-1}
#' so that it has a positive correlation with the first feature in its
#' sub-cluster.
#'
#' @importFrom FactoMineR PCA
#' @importFrom ClustOfVar hclustvar cutreevar
#' @importFrom dplyr select all_of
#' @importFrom tibble new_tibble
#' @importFrom stats cor
#'
#' @examples
#' set.seed(123)
#' data_tbl <- tibble::tibble(
#'   `100@150` = rnorm(20),
#'   `100@151` = rnorm(20),
#'   `101@200` = rnorm(20)
#' )
#'
#' # Pre-defined feature groups (e.g. after a retention-time grouping step)
#' cluster_feature_list <- list(
#'   c("100@150", "100@151"), # group of size 2
#'   "101@200" # group of size 1
#' )
#'
#' res <- cluster_features_based_on_correlations(
#'   df = data_tbl,
#'   cluster_feature_list = cluster_feature_list,
#'   cut_height = 0.26,
#'   corr_thresh = 0.75
#' )
#'
#' res$clustered_df
#' res$representatives_map
#'
#' @export
cluster_features_based_on_correlations <- function(
    df,
    cluster_feature_list,
    cut_height,
    corr_thresh) {
  `%>%` <- dplyr::`%>%`


  representatives_map <- vector("list")
  clustered_df <- tibble::new_tibble(list(), nrow = nrow(df))
  synthetic_feature_count <- 1L

  for (i in seq_along(cluster_feature_list)) {
    cluster <- cluster_feature_list[[i]]

    if (length(cluster) == 1L) {
      rep_feature <- cluster[1L]
      clustered_df <- cbind(clustered_df, df[rep_feature])
    } else if (length(cluster) == 2L) {
      feature_df <- df %>%
        dplyr::select(dplyr::all_of(cluster))

      pair_cor <- stats::cor(feature_df)[1, 2]

      if (pair_cor >= corr_thresh) {
        pca_res <- FactoMineR::PCA(
          feature_df,
          ncp = 1,
          graph = FALSE
        )

        loadings <- pca_res$var$cor /
          sqrt(pca_res$eig[1, 1])

        representative <- as.matrix(feature_df) %*% loadings

        cor_dir <- stats::cor(cbind(representative, feature_df))[1, 2]
        representative <- as.data.frame(representative * sign(cor_dir))

        synth_feat_name <- paste0("SynthFeat@", synthetic_feature_count)
        synthetic_feature_count <- synthetic_feature_count + 1L
        names(representative) <- synth_feat_name

        clustered_df <- cbind(clustered_df, representative)
        representatives_map[[synth_feat_name]] <- cluster
      } else {
        clustered_df <- cbind(clustered_df, feature_df)
      }
    } else {
      feature_matrix <- df %>%
        dplyr::select(dplyr::all_of(cluster)) %>%
        as.matrix()

      hc_tree <- ClustOfVar::hclustvar(feature_matrix)

      below_cut <- which(hc_tree$height < cut_height)

      cluster_index <- if (length(below_cut) > 0L) {
        length(hc_tree$labels) - max(below_cut)
      } else {
        length(hc_tree$labels)
      }

      n_clusters <- length(unique(hc_tree$clusmat[, cluster_index]))

      partition <- ClustOfVar::cutreevar(
        obj = hc_tree,
        k = n_clusters,
        matsim = TRUE
      )

      unique_compounds_tmp <- lapply(seq_len(max(partition$cluster)), function(i) {
        col_names <- colnames(feature_matrix)[partition$cluster == i]
        split_names <- strsplit(col_names, "__", fixed = TRUE)

        vapply(
          X = split_names,
          FUN = function(x) {
            if (length(x) == 1L) x[1] else x[2]
          },
          FUN.VALUE = character(1)
        )
      })

      sign_matrix_list <- vector("list", length(unique_compounds_tmp))

      for (i in seq_along(unique_compounds_tmp)) {
        first_feature <- unique_compounds_tmp[[i]][1]
        tmp_cor <- stats::cor(cbind(
          feature_matrix[, first_feature],
          partition$scores[, i]
        ))[1, 2]

        sign_matrix_list[[i]] <- rep(sign(tmp_cor), nrow(df))
      }

      sign_matrix <- do.call(cbind, sign_matrix_list)
      feature_cols <- partition$scores * sign_matrix
      for (i in seq_along(unique_compounds_tmp)) {
        cluster_i <- unique_compounds_tmp[[i]]
        if (length(cluster_i) == 1L) {
          rep_feature <- cluster_i[1L]
          colnames(feature_cols)[i] <- rep_feature
        } else {
          synth_feat_name <- paste0("SynthFeat@", synthetic_feature_count)
          synthetic_feature_count <- synthetic_feature_count + 1L
          colnames(feature_cols)[i] <- synth_feat_name
          representatives_map[[synth_feat_name]] <- cluster_i
        }
      }
      clustered_df <- cbind(clustered_df, feature_cols)
    }
  }

  return(list(
    clustered_df = clustered_df,
    representatives_map = representatives_map
  ))
}
