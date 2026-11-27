#' Build synthetic omics datasets for workflow examples and vignettes
#'
#' Creates synthetic datasets for either untargeted LC-MS metabolomics (matched
#' case-control study) or Olink-style targeted proteomics (case-cohort study) to
#' demonstrate both the modular workflow and the semi-automated [process_data()]
#' pipeline in \pkg{OmicsProcessing}. This is the generator used in
#' `data-raw/example_data.R` to build [example_metabolomics] and
#' [example_proteomics].
#'
#' @param type Character string specifying which synthetic dataset to generate:
#'   * `"metabolomics"` (default): Untargeted LC-MS metabolomics matched
#'     case-control dataset (216 rows: 36 pooled `"sQC"` samples and 180 study
#'     samples in 90 matched pairs across 3 batches and 6 plates; 30 raw
#'     positive-intensity `"mass@rt"` features with run-order drift, batch/plate
#'     shifts, MAR/MNAR missingness, 3 non-QC sample outliers, and co-eluting RT
#'     clusters).
#'   * `"proteomics"`: Olink-style targeted proteomics case-cohort dataset (210
#'     study samples across 3 batches and 6 plates, comprising a 140-sample
#'     subcohort plus 70 additional incident cases; 40 UniProt protein features
#'     on positive NPX scale with below-LOD missingness, plate/batch shifts,
#'     biological covariate effects, and 3 sample outliers).
#' @param seed Integer random seed (default `42`).
#'
#' @return A `data.frame` with default (`NULL`) row names containing sample
#'   metadata followed by numeric feature columns.
#'
#' @examples
#' metab <- build_omics_synthetic("metabolomics", seed = 42)
#' dim(metab)
#'
#' prot <- build_omics_synthetic("proteomics", seed = 42)
#' dim(prot)
#'
#' @seealso [example_metabolomics], [example_proteomics]
#' @export
build_omics_synthetic <- function(type = c("metabolomics", "proteomics"), seed = 42) {
  type <- match.arg(type)
  if (identical(type, "metabolomics")) {
    build_metabolomics_synthetic(seed = seed)
  } else {
    build_proteomics_synthetic(seed = seed)
  }
}

#' @noRd
build_metabolomics_synthetic <- function(seed = 42) {
  set.seed(seed)

  # 3 batches x 2 plates per batch = 6 plates, 36 samples per plate (216 total)
  n_batches <- 3L
  n_plates_per_batch <- 2L
  n_per_plate <- 36L
  n <- n_batches * n_plates_per_batch * n_per_plate

  plate_num <- rep(seq_len(n_batches * n_plates_per_batch), each = n_per_plate)
  batch_num <- (plate_num - 1L) %/% n_plates_per_batch + 1L
  run_order <- seq_len(n)

  # 6 pooled QC samples evenly spaced across each plate (36 QCs, 180 study samples)
  qc_pos_in_plate <- c(1L, 8L, 15L, 22L, 29L, 36L)
  pos_in_plate <- rep(seq_len(n_per_plate), times = n_batches * n_plates_per_batch)
  is_qc <- pos_in_plate %in% qc_pos_in_plate

  n_qc <- sum(is_qc)
  n_study <- sum(!is_qc)
  non_qc_idx <- which(!is_qc)
  n_pairs <- n_study / 2L

  sample_type <- ifelse(is_qc, "sQC", "Sample")
  sample_id <- character(n)
  sample_id[is_qc] <- sprintf("sQC_%02d", seq_len(n_qc))
  sample_id[!is_qc] <- sprintf("Sample_%03d", seq_len(n_study))

  # Matched 1:1 case-control design (90 pairs matched on center, sex, and ~age)
  pair_ids <- sprintf("Pair_%02d", seq_len(n_pairs))
  pair_center <- sample(c("C1", "C2", "C3"), size = n_pairs, replace = TRUE)
  pair_sex <- sample(c("F", "M"), size = n_pairs, replace = TRUE)
  pair_age <- stats::rnorm(n_pairs, mean = 55, sd = 10)

  study_pair_tbl <- data.frame(
    case_control_match = rep(pair_ids, each = 2L),
    case_status = rep(c(0L, 1L), times = n_pairs),
    center = rep(pair_center, each = 2L),
    sex = rep(pair_sex, each = 2L),
    age = round(rep(pair_age, each = 2L) + stats::rnorm(n_study, mean = 0, sd = 1.0), 1),
    stringsAsFactors = FALSE
  )
  study_pair_tbl <- study_pair_tbl[sample.int(n_study), , drop = FALSE]

  case_status <- rep(NA_integer_, n)
  case_status[non_qc_idx] <- study_pair_tbl$case_status

  case_control_match <- rep(NA_character_, n)
  case_control_match[non_qc_idx] <- study_pair_tbl$case_control_match

  center <- rep(NA_character_, n)
  center[non_qc_idx] <- study_pair_tbl$center
  center <- factor(center, levels = c("C1", "C2", "C3"))

  sex <- rep(NA_character_, n)
  sex[non_qc_idx] <- study_pair_tbl$sex
  sex <- factor(sex, levels = c("F", "M"))

  age <- rep(NA_real_, n)
  age[non_qc_idx] <- study_pair_tbl$age

  # 30 LC-MS style features named "mass@rt":
  # - Group A (cols 1-3, RT ~1.20): 3 correlated features
  # - Group B (cols 4-7, RT ~2.50): 3 correlated + 1 independent co-eluting feature
  # - Group C (cols 8-9, RT ~3.80): 2 correlated features
  # - Group D (cols 10-11, RT ~5.10): 2 correlated features
  # - Group E (cols 12-13, RT ~6.40): 2 uncorrelated co-eluting features
  # - Group F (cols 14-16, RT ~7.70): 3 correlated features
  # - Singletons (cols 17-26): 10 well-separated features (4 correlated, 6 independent)
  # - High-missingness (cols 27-30): 4 features with >55% NA
  feat_names <- c(
    "145.061@1.20", "146.064@1.22", "167.043@1.24",
    "203.053@2.50", "204.056@2.52", "225.035@2.53", "312.118@2.55",
    "181.071@3.80", "203.054@3.83",
    "265.112@5.10", "287.094@5.13",
    "194.080@6.40", "242.139@6.43",
    "329.152@7.70", "330.155@7.72", "351.134@7.74",
    "118.086@0.85", "132.102@1.95", "156.077@3.15", "175.119@4.45",
    "219.104@5.75", "256.170@7.05", "284.201@8.35", "305.162@9.10",
    "341.108@9.85", "388.245@10.60",
    "412.188@11.30", "455.221@12.05", "498.254@12.80", "521.301@13.55"
  )
  p <- length(feat_names)

  # Latent biological variation (study samples only; pooled QCs have constant biology)
  bio_A <- stats::rnorm(n, 0, 0.58); bio_A[is_qc] <- 0
  bio_B <- stats::rnorm(n, 0, 0.58); bio_B[is_qc] <- 0
  bio_C <- stats::rnorm(n, 0, 0.58); bio_C[is_qc] <- 0
  bio_D <- stats::rnorm(n, 0, 0.58); bio_D[is_qc] <- 0
  bio_F <- stats::rnorm(n, 0, 0.58); bio_F[is_qc] <- 0
  bio_S <- stats::rnorm(n, 0, 0.58); bio_S[is_qc] <- 0

  age_z <- ifelse(is_qc, 0, (age - 55) / 10)
  sex_num <- ifelse(is_qc, 0, ifelse(sex == "M", 0.5, -0.5))
  case_num <- ifelse(is_qc, 0, ifelse(case_status == 1L, 0.5, -0.5))

  bio_mat <- matrix(0, nrow = n, ncol = p)
  for (j in 1:3)   bio_mat[, j] <- bio_A + ifelse(is_qc, 0, stats::rnorm(n, 0, 0.05))
  for (j in 4:6)   bio_mat[, j] <- bio_B + ifelse(is_qc, 0, stats::rnorm(n, 0, 0.05))
  bio_mat[, 7]     <- ifelse(is_qc, 0, stats::rnorm(n, 0, 0.50))
  for (j in 8:9)   bio_mat[, j] <- bio_C + ifelse(is_qc, 0, stats::rnorm(n, 0, 0.05))
  for (j in 10:11) bio_mat[, j] <- bio_D + ifelse(is_qc, 0, stats::rnorm(n, 0, 0.05))
  bio_mat[, 12]    <- ifelse(is_qc, 0, stats::rnorm(n, 0, 0.50))
  bio_mat[, 13]    <- ifelse(is_qc, 0, stats::rnorm(n, 0, 0.50))
  for (j in 14:16) bio_mat[, j] <- bio_F + ifelse(is_qc, 0, stats::rnorm(n, 0, 0.05))
  for (j in 17:20) bio_mat[, j] <- bio_S + ifelse(is_qc, 0, stats::rnorm(n, 0, 0.07))
  for (j in 21:26) bio_mat[, j] <- ifelse(is_qc, 0, stats::rnorm(n, 0, 0.52))
  for (j in 27:30) bio_mat[, j] <- ifelse(is_qc, 0, stats::rnorm(n, 0, 0.50))

  # Share biological covariate slopes within co-eluting adduct/isotope groups
  age_betas <- stats::runif(p, -0.15, 0.15)
  sex_betas <- stats::runif(p, -0.20, 0.20)
  case_betas <- c(rep(0.25, 3), rep(-0.20, 3), rep(0, p - 6))
  for (grp in list(1:3, 4:6, 8:9, 10:11, 14:16)) {
    age_betas[grp] <- age_betas[grp[1L]]
    sex_betas[grp] <- sex_betas[grp[1L]]
  }

  # Shared technical effects (run-order drift + within-plate drift + batch/plate shifts)
  t_scaled <- (run_order - 1) / (n - 1)
  drift1 <- -0.70 * t_scaled + 0.30 * sin(2 * pi * t_scaled * 3)
  drift2 <- 0.40 * (pos_in_plate / n_per_plate) - 0.20
  batch_eff <- c(0, 0.65, -0.35)[batch_num]
  plate_eff <- c(0, 0.35, -0.25, 0.35, -0.20, 0.30)[plate_num]

  base_means <- stats::runif(p, 7.2, 9.8)
  X_log <- matrix(0, nrow = n, ncol = p)
  colnames(X_log) <- feat_names

  for (j in seq_len(p)) {
    w_d1 <- stats::runif(1, 0.75, 1.25)
    w_d2 <- stats::runif(1, 0.65, 1.25)
    w_b  <- stats::runif(1, 0.80, 1.35)
    w_p  <- stats::runif(1, 0.70, 1.30)
    tech_noise <- stats::rnorm(n, 0, ifelse(is_qc, 0.035, 0.05))
    X_log[, j] <- base_means[j] + bio_mat[, j] +
      age_betas[j] * age_z + sex_betas[j] * sex_num + case_betas[j] * case_num +
      w_d1 * drift1 + w_d2 * drift2 + w_b * batch_eff + w_p * plate_eff + tech_noise
  }

  # Inject 3 clear non-QC outlier samples (one per batch)
  outlier_rows <- non_qc_idx[c(22L, 85L, 148L)]
  X_log[outlier_rows[1], ] <- X_log[outlier_rows[1], ] + stats::rnorm(p, 2.8, 0.3)
  X_log[outlier_rows[2], ] <- X_log[outlier_rows[2], ] + rep(c(2.6, -2.6), length.out = p)
  X_log[outlier_rows[3], ] <- X_log[outlier_rows[3], ] + rep(c(-2.8, 2.8), length.out = p)

  # Positive raw intensity scale (inverse of log1p)
  X_raw <- expm1(pmax(X_log, 1.0))

  # Introduce missingness in non-QC rows (QCs remain complete):
  # 1) 4 high-missingness features (>55% NA)
  hi_miss_props <- c(0.58, 0.66, 0.76, 0.86)
  for (k in seq_along(hi_miss_props)) {
    j <- 26L + k
    na_rows <- sample(non_qc_idx, size = round(hi_miss_props[k] * n_study))
    X_raw[na_rows, j] <- NA_real_
  }

  # 2) 5 high-missingness non-QC samples (>55% NA across retained features)
  cand_hi_miss_rows <- setdiff(non_qc_idx, outlier_rows)
  hi_miss_rows <- cand_hi_miss_rows[c(10L, 45L, 80L, 120L, 155L)]
  for (r in hi_miss_rows) {
    na_cols <- sample(seq_len(26L), size = round(stats::runif(1, 0.58, 0.75) * 26L))
    X_raw[r, na_cols] <- NA_real_
  }

  # 3) Moderate missingness in retained features:
  #    - Low MAR missingness (3-7% NA) in correlated features (RF-amenable)
  #    - Low-to-moderate left-censored MNAR (4-24% NA) in independent features (LCMD-amenable)
  keep_non_qc <- setdiff(non_qc_idx, c(hi_miss_rows, outlier_rows))
  mar_low_cols <- c(1:6, 8:11, 14:20)
  mnar_low_cols <- c(7L, 12L, 13L, 21L)
  mnar_mid_cols <- c(22:26)

  for (j in mar_low_cols) {
    na_r <- sample(keep_non_qc, size = round(stats::runif(1, 0.03, 0.07) * length(keep_non_qc)))
    X_raw[na_r, j] <- NA_real_
  }
  for (j in mnar_low_cols) {
    vals <- X_raw[keep_non_qc, j]
    q_cut <- stats::quantile(vals, probs = stats::runif(1, 0.04, 0.07), na.rm = TRUE)
    X_raw[keep_non_qc[vals <= q_cut], j] <- NA_real_
  }
  for (j in mnar_mid_cols) {
    vals <- X_raw[keep_non_qc, j]
    q_cut <- stats::quantile(vals, probs = stats::runif(1, 0.14, 0.24), na.rm = TRUE)
    X_raw[keep_non_qc[vals <= q_cut], j] <- NA_real_
  }

  metab <- data.frame(
    sample_id = sample_id,
    sample_type = sample_type,
    is_qc = is_qc,
    batch = factor(paste0("Batch", batch_num), levels = paste0("Batch", seq_len(n_batches))),
    plate = factor(paste0("Plate", plate_num), levels = paste0("Plate", seq_len(n_batches * n_plates_per_batch))),
    run_order = run_order,
    case_status = case_status,
    case_control_match = case_control_match,
    center = center,
    sex = sex,
    age = age,
    X_raw,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  rownames(metab) <- NULL

  metab
}

#' @noRd
build_proteomics_synthetic <- function(seed = 42) {
  set.seed(seed)

  # Case-cohort study: 210 study samples across 3 batches x 2 plates/batch = 6 plates (35/plate)
  # Random subcohort (n = 140, including 20 incident cases) + 70 additional incident cases = 90 cases, 120 non-cases
  n_batches <- 3L
  n_plates_per_batch <- 2L
  n_per_plate <- 35L
  n <- n_batches * n_plates_per_batch * n_per_plate # 210

  plate_num <- rep(seq_len(n_batches * n_plates_per_batch), each = n_per_plate)
  batch_num <- (plate_num - 1L) %/% n_plates_per_batch + 1L
  sample_id <- sprintf("PROT_%03d", seq_len(n))

  # Case-cohort membership randomly distributed across plates/batches
  design_rows <- data.frame(
    subcohort = c(rep(TRUE, 140L), rep(FALSE, 70L)),
    case_status = c(rep(0L, 120L), rep(1L, 20L), rep(1L, 70L))
  )
  design_rows <- design_rows[sample.int(n), , drop = FALSE]

  center <- factor(sample(c("C1", "C2", "C3"), size = n, replace = TRUE), levels = c("C1", "C2", "C3"))
  sex <- factor(sample(c("F", "M"), size = n, replace = TRUE), levels = c("F", "M"))
  age <- round(stats::rnorm(n, mean = 56, sd = 9.5), 1)

  # 40 realistic human UniProt accessions (inflammatory / cardiometabolic Olink-style panel)
  protein_ids <- c(
    "P01375", "P05231", "P10145", "P01584", "P13500",
    "P02741", "P05362", "P15692", "P01137", "P01579",
    "P05112", "P22301", "P02778", "P09341", "P13232",
    "P19438", "P20333", "P08887", "P01589", "P29459",
    "P14210", "P01133", "P09038", "P16870", "P02763",
    "P00738", "P02751", "P01009", "P02647", "P04114",
    "P05121", "P08253", "P14780", "P01033", "P16035",
    "P10909", "P04275", "P02749", "P00747", "P01024"
  )
  p <- length(protein_ids)

  # Latent biological protein modules + covariates (age, sex, center, case_status)
  bio_factors <- matrix(stats::rnorm(n * 5L, 0, 1), nrow = n, ncol = 5L)
  age_z <- (age - 56) / 9.5
  sex_num <- ifelse(sex == "M", 0.5, -0.5)
  center_eff <- c(-0.20, 0.0, 0.25)[as.integer(center)]
  case_num <- ifelse(design_rows$case_status == 1L, 0.5, -0.5)

  # Plate and batch technical shifts (Olink residual plate/batch effects)
  batch_eff <- c(-0.30, 0.15, 0.45)[batch_num]
  plate_eff <- c(-0.45, 0.30, -0.35, 0.40, -0.25, 0.50)[plate_num]

  base_npx <- stats::runif(p, 4.0, 10.5)
  X_npx <- matrix(0, nrow = n, ncol = p)
  colnames(X_npx) <- protein_ids

  for (j in seq_len(p)) {
    w_bio <- stats::rnorm(5L, 0, 0.22)
    b_age <- stats::runif(1, -0.22, 0.22)
    b_sex <- stats::runif(1, -0.30, 0.30)
    b_ctr <- stats::runif(1, 0.6, 1.3)
    b_case <- if (j <= 8L) stats::runif(1, 0.25, 0.45) else 0
    w_batch <- stats::runif(1, 0.6, 1.3)
    w_plate <- stats::runif(1, 0.7, 1.4)

    X_npx[, j] <- base_npx[j] +
      as.vector(bio_factors %*% w_bio) +
      stats::rnorm(n, 0, 0.55) +
      b_age * age_z + b_sex * sex_num + b_ctr * center_eff + b_case * case_num +
      w_batch * batch_eff + w_plate * plate_eff
  }

  # Keep NPX strictly positive (>= 0.8) before outlier injection
  X_npx <- pmax(X_npx, 0.8)

  # Inject 3 clear sample outliers (shifted across proteins)
  outlier_rows <- c(18L, 95L, 172L)
  X_npx[outlier_rows[1], ] <- X_npx[outlier_rows[1], ] + stats::rnorm(p, 3.8, 0.4)
  X_npx[outlier_rows[2], ] <- X_npx[outlier_rows[2], ] + rep(c(3.5, -2.5), length.out = p)
  X_npx[outlier_rows[3], ] <- pmax(X_npx[outlier_rows[3], ] - stats::rnorm(p, 3.2, 0.4), 0.2)

  # Introduce below-LOD missingness:
  # 1) 4 high-missingness proteins (>55% NA -> dropped at 0.5 threshold)
  hi_miss_props <- c(0.58, 0.68, 0.78, 0.88)
  for (k in seq_along(hi_miss_props)) {
    j <- 36L + k
    q_cut <- stats::quantile(X_npx[, j], probs = hi_miss_props[k], na.rm = TRUE)
    X_npx[X_npx[, j] <= q_cut, j] <- NA_real_
  }

  # 2) 5 high-missingness samples (>55% NA across retained proteins -> dropped at 0.5 threshold)
  cand_rows <- setdiff(seq_len(n), outlier_rows)
  hi_miss_rows <- cand_rows[c(12L, 54L, 108L, 150L, 192L)]
  for (r in hi_miss_rows) {
    na_cols <- sample(seq_len(36L), size = round(stats::runif(1, 0.58, 0.75) * 36L))
    X_npx[r, na_cols] <- NA_real_
  }

  # 3) Remaining 36 proteins (left-censored below-LOD missingness in non-failed samples):
  #    - cols 1-5: 0% NA (complete proteins)
  #    - cols 6-30: 2%-7% NA (retained even at strict 10% threshold -> 30 proteins kept!)
  #    - cols 31-36: 14%-32% NA (dropped at 10% threshold, retained at 50% threshold)
  keep_rows <- setdiff(seq_len(n), c(hi_miss_rows, outlier_rows))
  for (j in 6:30) {
    vals <- X_npx[keep_rows, j]
    q_cut <- stats::quantile(vals, probs = stats::runif(1, 0.02, 0.07), na.rm = TRUE)
    X_npx[keep_rows[vals <= q_cut], j] <- NA_real_
  }
  for (j in 31:36) {
    vals <- X_npx[keep_rows, j]
    q_cut <- stats::quantile(vals, probs = stats::runif(1, 0.14, 0.32), na.rm = TRUE)
    X_npx[keep_rows[vals <= q_cut], j] <- NA_real_
  }

  prot <- data.frame(
    sample_id = sample_id,
    subcohort = design_rows$subcohort,
    case_status = design_rows$case_status,
    batch = factor(paste0("Batch", batch_num), levels = paste0("Batch", seq_len(n_batches))),
    plate = factor(paste0("Plate", plate_num), levels = paste0("Plate", seq_len(n_batches * n_plates_per_batch))),
    center = center,
    sex = sex,
    age = age,
    X_npx,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  rownames(prot) <- NULL

  prot
}
