# Detect the experimental design from the data structure

Detect the experimental design from the data structure

## Usage

``` r
detect_design(df, gen_col, env_col, rep_col, block_col = NULL)
```

## Arguments

- df:

  Data frame

- gen_col:

  Name of genotype column

- env_col:

  Name of environment column

- rep_col:

  Name of replicate column

- block_col:

  Name of block column (can be NULL)

## Value

List with design_type and description
