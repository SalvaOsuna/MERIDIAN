# Parse dates robustly from various formats

Parse dates robustly from various formats

## Usage

``` r
parse_date_robustly(date_val)
```

## Arguments

- date_val:

  Vector or single value of dates in char, factor, numeric, or date
  format

## Value

Date vector

## Examples

``` r
parse_date_robustly(c("2026-05-21", "21/05/2026", "46163"))
#> [1] "2026-05-21" "2026-05-21" "2026-05-21"
```
