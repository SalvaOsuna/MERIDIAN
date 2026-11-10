# Run Mega-Environment Analysis (Which-won-where) Natively computes winning genotypes per environment and groups them.

Run Mega-Environment Analysis (Which-won-where) Natively computes
winning genotypes per environment and groups them.

## Usage

``` r
run_mega_envs(df, gen_col, env_col, trait, direction = "higher")
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

- direction:

  Selection objective: higher or lower

## Value

List with env_strat dataframe
