# Regenerate the packaged example_metabolomics and example_proteomics datasets.

source("R/build_omics_synthetic.R")

example_metabolomics <- build_omics_synthetic(type = "metabolomics", seed = 42)
example_proteomics <- build_omics_synthetic(type = "proteomics", seed = 42)

save(
  example_metabolomics,
  file = "data/example_metabolomics.rda",
  compress = "bzip2"
)

save(
  example_proteomics,
  file = "data/example_proteomics.rda",
  compress = "bzip2"
)
