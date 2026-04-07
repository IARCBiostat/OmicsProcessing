# Synthetic omics data generator used for examples and vignettes in
# OmicsProcessing.

build_omics_synthetic <- function(
  seed = 1,
  n = 2000,
  n_batch = 2,
  n_plate_per_batch = 2,
  n_features = 10,
  qc_frac = 0.05
) {
  set.seed(seed)

  feature_cols <- paste0("F", seq_len(n_features))
  feature_means <- stats::runif(n_features, min = 100, max = 700)
  feature_sds <- stats::runif(n_features, min = 3, max = 30)

  feature_matrix <- vapply(
    seq_len(n_features),
    FUN.VALUE = numeric(n),
    FUN = function(j) {
      stats::rnorm(n, mean = feature_means[j], sd = feature_sds[j])
    }
  )
  colnames(feature_matrix) <- feature_cols

  omics_synthetic <- data.frame(
    plate_id = factor(sample.int(n_plate_per_batch * n_batch, n, replace = TRUE)),
    feature_matrix,
    check.names = FALSE
  )

  omics_synthetic <- omics_synthetic[order(omics_synthetic$plate_id), , drop = FALSE]
  rownames(omics_synthetic) <- NULL

  omics_synthetic$run_ord <- seq_len(nrow(omics_synthetic))
  omics_synthetic$batch_id <-
    (as.integer(omics_synthetic$plate_id) - 1L) %/% n_plate_per_batch + 1L

  n_qc <- ceiling(qc_frac * nrow(omics_synthetic))
  qc_idx <- sample.int(nrow(omics_synthetic), size = n_qc, replace = FALSE)
  omics_synthetic$is_qc <- FALSE
  omics_synthetic$is_qc[qc_idx] <- TRUE

  drift_slopes <- stats::runif(n_features, min = 0.01, max = 0.1)
  for (j in seq_len(n_features)) {
    omics_synthetic[[feature_cols[j]]] <-
      omics_synthetic[[feature_cols[j]]] - drift_slopes[j] * omics_synthetic$run_ord
  }

  jump_info <- vector("list", length = n_features)
  names(jump_info) <- feature_cols

  for (j in seq_len(n_features)) {
    n_jumps <- sample(1:2, size = 1)
    jump_points <- sort(sample(100:(n - 100), size = n_jumps))
    jump_sizes <- stats::runif(n_jumps, min = 0.5, max = 1.5) * feature_sds[j] * 2

    for (k in seq_len(n_jumps)) {
      idx <- omics_synthetic$run_ord >= jump_points[k]
      omics_synthetic[[feature_cols[j]]][idx] <-
        omics_synthetic[[feature_cols[j]]][idx] + jump_sizes[k]
    }

    jump_info[[j]] <- data.frame(
      feature = feature_cols[j],
      jump_id = seq_len(n_jumps),
      jump_point = jump_points,
      jump_size = jump_sizes
    )
  }

  jump_info <- do.call(rbind, jump_info)
  rownames(jump_info) <- NULL

  list(
    omics_synthetic = omics_synthetic,
    jump_info = jump_info,
    parameters = list(
      seed = seed,
      n = n,
      n_batch = n_batch,
      n_plate_per_batch = n_plate_per_batch,
      n_features = n_features,
      qc_frac = qc_frac,
      feature_means = feature_means,
      feature_sds = feature_sds,
      drift_slopes = drift_slopes
    )
  )
}

# Example usage after locating this script with
# system.file("scripts", "omics_synthetic.R", package = "OmicsProcessing"):
#
# source(script_path)
# generated <- build_omics_synthetic()
# str(generated$omics_synthetic)
