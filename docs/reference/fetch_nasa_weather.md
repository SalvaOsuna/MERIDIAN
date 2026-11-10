# Fetch daily weather parameters from NASA POWER API

Fetch daily weather parameters from NASA POWER API

## Usage

``` r
fetch_nasa_weather(lat, lon, start_date, end_date)
```

## Arguments

- lat:

  Numeric latitude

- lon:

  Numeric longitude

- start_date:

  R Date object

- end_date:

  R Date object

## Value

A data frame with daily weather variables, or NULL for missing
coordinates or dates. Raises an error when the weather service is
unavailable.

## Examples

``` r
if (FALSE) { # \dontrun{
weather <- fetch_nasa_weather(
  52.13, -106.63, as.Date("2025-05-10"), as.Date("2025-05-15")
)
} # }
```
