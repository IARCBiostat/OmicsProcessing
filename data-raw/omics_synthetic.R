# Regenerate the packaged omics_synthetic dataset.

source("inst/scripts/omics_synthetic.R")

generated <- build_omics_synthetic(seed = 1)

omics_synthetic <- generated$omics_synthetic

save(
  omics_synthetic,
  file = "data/omics_synthetic.rda",
  compress = "bzip2"
)
