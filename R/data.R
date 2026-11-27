#' Synthetic untargeted LC-MS metabolomics dataset (matched case-control)
#'
#' A synthetic untargeted LC-MS metabolomics dataset structured as a 1:1 matched
#' case-control study with interspersed pooled QC samples. It is designed to
#' demonstrate the modular processing workflow (missingness filtering, PCA + LOF
#' outlier removal, `log1p()` transformation, hybrid RF + LCMD imputation, SERRF
#' normalisation, run-order distribution plotting, and retention-time feature
#' clustering) as well as matched case-control handling in [process_data()].
#'
#' Reproducible generation code is implemented in [build_omics_synthetic()] and
#' invoked in `data-raw/example_data.R`.
#'
#' @format A data frame with 216 rows and 41 variables:
#' \describe{
#'   \item{sample_id}{Character sample identifier (`"sQC_01"`--`"sQC_36"` for QC samples, `"Sample_001"`--`"Sample_180"` for study samples).}
#'   \item{sample_type}{Character sample type (`"sQC"` for pooled QC samples, `"Sample"` for study samples).}
#'   \item{is_qc}{Logical indicator for QC samples.}
#'   \item{batch}{Factor with 3 levels (`"Batch1"`--`"Batch3"`).}
#'   \item{plate}{Factor with 6 levels (`"Plate1"`--`"Plate6"`, two plates per batch).}
#'   \item{run_order}{Integer acquisition/injection order (`1` to `216`).}
#'   \item{case_status}{Integer case indicator (`1` = case, `0` = control) for study samples; `NA` for QC samples.}
#'   \item{case_control_match}{Character matched pair identifier (`"Pair_01"`--`"Pair_90"`, one case and one control per pair) for study samples; `NA` for QC samples.}
#'   \item{center}{Factor study centre (`"C1"`--`"C3"`) for study samples; `NA` for QC samples.}
#'   \item{sex}{Factor (`"F"`, `"M"`) for study samples; `NA` for QC samples.}
#'   \item{age}{Numeric age in years for study samples; `NA` for QC samples.}
#'   \item{mass@rt columns}{30 numeric raw-intensity feature columns named in `"mass@rt"` format, including high-missingness features (>55% NA), moderate MAR/MNAR missingness, run-order drift, batch/plate shifts, and co-eluting RT clusters.}
#' }
#' @seealso [build_omics_synthetic()], [example_proteomics]
#' @usage data(example_metabolomics)
"example_metabolomics"

#' Synthetic Olink-style proteomics dataset (case-cohort)
#'
#' A synthetic targeted proteomics dataset modelled on an Olink case-cohort
#' study (random subcohort plus additional incident cases). It is designed to
#' demonstrate the semi-automated [process_data()] pipeline, including feature
#' and sample missingness filtering, limit-of-detection (LOD) imputation,
#' rank-based inverse normal transformation, PCA + LOF outlier exclusion, and
#' linear mixed-model plate/batch correction without matched case-control pair
#' dropping.
#'
#' Reproducible generation code is implemented in [build_omics_synthetic()] and
#' invoked in `data-raw/example_data.R`.
#'
#' @format A data frame with 210 rows and 48 variables:
#' \describe{
#'   \item{sample_id}{Character sample identifier (`"PROT_001"`--`"PROT_210"`).}
#'   \item{subcohort}{Logical indicator for membership in the random subcohort (`140` `TRUE`, `70` `FALSE`).}
#'   \item{case_status}{Integer incident case indicator (`90` cases total: `20` inside the subcohort and `70` outside; `120` non-cases).}
#'   \item{batch}{Factor with 3 levels (`"Batch1"`--`"Batch3"`).}
#'   \item{plate}{Factor with 6 levels (`"Plate1"`--`"Plate6"`, two plates per batch).}
#'   \item{center}{Factor study centre (`"C1"`--`"C3"`).}
#'   \item{sex}{Factor (`"F"`, `"M"`).}
#'   \item{age}{Numeric age in years.}
#'   \item{UniProt columns}{40 numeric Olink NPX-scale protein columns named by UniProt accession (e.g., `P01375`, `P05231`), including high-missingness proteins (>55% NA), left-censored below-LOD missingness, plate/batch shifts, and 3 injected sample outliers.}
#' }
#' @seealso [build_omics_synthetic()], [example_metabolomics]
#' @usage data(example_proteomics)
"example_proteomics"
