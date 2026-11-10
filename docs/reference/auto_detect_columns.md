# Match column names to expected MET roles using fuzzy matching

Match column names to expected MET roles using fuzzy matching

## Usage

``` r
auto_detect_columns(col_names)
```

## Arguments

- col_names:

  Character vector of column names from uploaded data

## Value

Named list: genotype, environment, rep, block (NULL if not found)
