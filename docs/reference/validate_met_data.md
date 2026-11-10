# Validate MET dataset structure and content

Validate MET dataset structure and content

## Usage

``` r
validate_met_data(df, gen_col, env_col, rep_col, resp_vars, block_col = NULL)
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

- resp_vars:

  Character vector of response variable names

- block_col:

  Optional block column identifying units within replicate

## Value

List of validation messages: errors (fatal), warnings (informational)
