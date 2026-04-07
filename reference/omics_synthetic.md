# Synthetic omics dataset for plotting and normalisation examples

A synthetic omics dataset with run order, batch, plate, QC flags, and
ten feature columns. It is intended for examples and vignettes
demonstrating plotting and normalisation workflows. Reproducible
data-generation code is available in `data-raw/omics_synthetic.R` in the
source repository and in the installed package via
`system.file("scripts", "omics_synthetic.R", package = "OmicsProcessing")`.

## Usage

``` r
data(omics_synthetic)
```

## Format

A data frame with 2000 rows and 14 variables:

- plate_id:

  Plate identifier with 4 levels.

- F1-F10:

  Numeric feature intensity columns.

- run_ord:

  Integer run order.

- batch_id:

  Batch identifier.

- is_qc:

  Logical indicator for QC samples.
