# Calculate summary agroclimatic indices from daily weather series

Calculate summary agroclimatic indices from daily weather series

## Usage

``` r
calculate_enviromic_indices(daily_weather, dtf_mean = NULL, dtm_mean = NULL)
```

## Arguments

- daily_weather:

  Data frame containing daily variables

- dtf_mean:

  Mean days to flowering (optional)

- dtm_mean:

  Mean days to maturity (optional)

## Value

A single-row data frame with computed indices

## Details

Growing degree days use a base temperature of 10 degrees Celsius.
Reproductive heat-stress days count maximum temperatures above 30
degrees Celsius. These fixed thresholds are not crop-specific
calibrations.

## Examples

``` r
daily <- data.frame(
  T2M_MAX = c(25, 27, 24), T2M_MIN = c(15, 17, 14),
  PRECTOTCORR = c(2, 5, 0), RH2M = c(60, 70, 50),
  ALLSKY_SFC_SW_DWN = c(18, 20, 15)
)
calculate_enviromic_indices(daily)
#>   MeanTemp_C TotalRainfall_mm GDD MeanRH_pct TotalSolar_MJ
#> 1      20.33                7  31         60            53
```
