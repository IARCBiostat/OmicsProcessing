# Build synthetic omics data for plotting and normalisation examples

Create a synthetic omics data set with feature intensities, run-order
drift, batch structure, plate structure, and QC sample labels. This is
the generator used for the packaged `omics_synthetic` example data.

## Usage

``` r
build_omics_synthetic(
  seed = 1,
  n = 2000,
  n_batch = 2,
  n_plate_per_batch = 2,
  n_features = 10,
  qc_frac = 0.05
)
```

## Arguments

- seed:

  Integer random seed.

- n:

  Number of rows (samples).

- n_batch:

  Number of batches.

- n_plate_per_batch:

  Number of plates per batch.

- n_features:

  Number of feature columns to generate.

- qc_frac:

  Fraction of rows labelled as QC.

## Value

A list with three elements:

- omics_synthetic:

  A synthetic omics data frame.

- jump_info:

  A data frame describing the simulated step changes per feature.

- parameters:

  A list of generation parameters and sampled values.

## Examples

``` r
generated <- build_omics_synthetic(seed = 1)
str(generated$omics_synthetic)
#> 'data.frame':    2000 obs. of  14 variables:
#>  $ plate_id: Factor w/ 4 levels "1","2","3","4": 1 1 1 1 1 1 1 1 1 1 ...
#>  $ F1      : num  263 254 240 259 267 ...
#>  $ F2      : num  315 326 321 318 309 ...
#>  $ F3      : num  414 443 447 431 435 ...
#>  $ F4      : num  640 626 638 643 645 ...
#>  $ F5      : num  218 229 218 239 217 ...
#>  $ F6      : num  653 631 619 637 630 ...
#>  $ F7      : num  660 685 655 655 669 ...
#>  $ F8      : num  472 521 487 547 460 ...
#>  $ F9      : num  491 465 452 471 485 ...
#>  $ F10     : num  149.2 155.2 83.3 141.8 148.1 ...
#>  $ run_ord : int  1 2 3 4 5 6 7 8 9 10 ...
#>  $ batch_id: num  1 1 1 1 1 1 1 1 1 1 ...
#>  $ is_qc   : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
head(generated$jump_info)
#>   feature jump_id jump_point jump_size
#> 1      F1       1        421  23.49952
#> 2      F2       1        789  14.35912
#> 3      F2       2       1455  21.30239
#> 4      F3       1       1312  49.26956
#> 5      F3       2       1378  44.52803
#> 6      F4       1        639  36.56033
```
