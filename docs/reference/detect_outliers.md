# Detect outliers in a trait by group

Detect outliers in a trait by group

## Usage

``` r
detect_outliers(df, trait, group_col, method = "iqr", threshold = NULL)
```

## Arguments

- df:

  Data frame

- trait:

  Trait column name

- group_col:

  Column to group by (e.g., environment)

- method:

  "iqr" or "zscore"

- threshold:

  Multiplier (1.5 for IQR, 3 for z-score)

## Value

Data frame with an 'is_outlier' logical column added
