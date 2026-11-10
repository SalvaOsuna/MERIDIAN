# AMMI analysis using metan::performs_ammi()

AMMI analysis using metan::performs_ammi()

## Usage

``` r
run_ammi_metan(df, gen_col, env_col, rep_col, trait)
```

## Arguments

- df:

  Data frame

- gen_col:

  Genotype column name

- env_col:

  Environment column name

- rep_col:

  Replicate column name

- trait:

  Trait column name

## Value

List with AMMI model, ANOVA table, and IPCA scores
