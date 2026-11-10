# Prepare data for GGE while preserving all observed genotypes/environments Missing GxE cells are imputed with additive expectations to avoid dropping levels.

Prepare data for GGE while preserving all observed
genotypes/environments Missing GxE cells are imputed with additive
expectations to avoid dropping levels.

## Usage

``` r
prepare_gge_data(df, gen_col, env_col, trait)
```

## Arguments

- df:

  Data frame

- gen_col:

  Genotype column name

- env_col:

  Environment column name

- trait:

  Trait column name

## Value

List with imputed GGE input data and imputed-cell count
