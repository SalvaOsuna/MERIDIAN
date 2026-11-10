# Compute descriptive statistics grouped by a factor

Compute descriptive statistics grouped by a factor

## Usage

``` r
descriptive_summary(df, group_col, traits)
```

## Arguments

- df:

  Data frame

- group_col:

  Column name to group by

- traits:

  Character vector of trait column names

## Value

Tibble with Mean, SD, Min, Max, CV%, N per group x trait
