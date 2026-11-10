# Process environmental covariates table and calculate derived indices

Process environmental covariates table and calculate derived indices

## Usage

``` r
process_environmental_covariates(
  env_data,
  dtf_means = NULL,
  dtm_means = NULL,
  session = NULL
)
```

## Arguments

- env_data:

  Input data frame with Environment, Latitude, Longitude, PlantingDate,
  HarvestDate

- dtf_means:

  Optional named numeric vector of mean days to flowering per
  environment

- dtm_means:

  Optional named numeric vector of mean days to maturity per environment

- session:

  Optional Shiny session object

## Value

Processed data frame with computed environmental indices

## Examples

``` r
if (FALSE) { # \dontrun{
environments <- data.frame(
  Environment = "E1", Latitude = 52.13, Longitude = -106.63,
  PlantingDate = "2025-05-10", HarvestDate = "2025-09-15"
)
process_environmental_covariates(environments)
} # }
```
