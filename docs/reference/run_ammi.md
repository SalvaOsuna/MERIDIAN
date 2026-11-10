# Run AMMI Analysis natively (no metan dependency)

Run AMMI Analysis natively (no metan dependency)

## Usage

``` r
run_ammi(
  data,
  gen,
  env,
  rep = NULL,
  trait,
  n_axis = 2,
  block_col = NULL,
  design_type = "RCBD",
  augmented_checks = NULL
)
```

## Arguments

- data:

  Data frame in long format

- gen:

  Genotype column name

- env:

  Environment column name

- rep:

  Replicate column name, or NULL when unavailable

- trait:

  Phenotypic trait column name

- n_axis:

  Number of IPCA axes to retain (default = 2)

- block_col:

  Optional block column nested in replicate and environment

- design_type:

  Experimental design label

- augmented_checks:

  Replicated check genotypes for augmented designs

## Value

List of results for plotting and reporting
