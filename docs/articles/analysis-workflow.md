# Analysis workflow and interpretation

MERIDIAN connects seven modules. The main analytical workflow is
interactive; the examples here use exported helpers and the bundled
simulated data.

## 1. Map data and experimental design

In **Overview**, map the identifiers and numeric traits. Specify RCBD,
incomplete-block or augmented design as appropriate. Blocks are nested
within replicate and environment. Select replicated check genotypes for
augmented designs. A design label alone cannot resolve missing
replication or confounded effects; inspect the validation messages.

## 2. Inspect measurements

Use **Traits** for distributions, G×E means, missingness, environment
correlations and outliers. Check whether absent measurements affect a
few plots or entire genotype–environment cells. Each trait may have
different coverage.

## 3. Fit ANOVA and mixed models

In **ANOVA**, choose a trait and selection objective, then run the
analysis.

- The fixed analysis uses adjusted marginal tests for genotype and
  environment and a partial G×E test. Partial sums of squares are not
  additive variance percentages.
- Variance components use REML. Cullis generalized heritability is
  derived from pairwise prediction-error variances.
- BLUEs treat genotype as fixed and include design and G×E terms.
- BLUPs treat genotype and G×E as random, with environment fixed.
  Predictions average environments equally. Prediction intervals include
  fixed-effect estimation uncertainty, conditional on the estimated
  variance components.

Review singularity, replication and estimability diagnostics alongside
the estimates. A ranking does not by itself establish a significant
difference.

## 4. Study stability

**Stability** includes AMMI, GGE, Eberhart–Russell, Wricke and Shukla
outputs. Read the processing tables before comparing results. AMMI/GGE
can complete missing cell means using an estimable additive model in a
connected network; these predictions remain distinct from observed
means. Classical stability indices use a disclosed complete-cell subset.

AMMI ANOVA uses observed plot data and the design; it does not treat
imputed means as extra observations. Without plot-error degrees of
freedom, interpret the decomposition descriptively.

## 5. Explore adaptation and environmental covariates

**Adaptation** combines observed-winner grouping, reaction norms,
environmental PCA, correlations and climatic-driver plots. Winner groups
display ties and coverage; they are exploratory groupings rather than
validated mega-environments.

You can supply precomputed numeric covariates. Environmental data with
latitude, longitude and planting/harvest dates can request daily NASA
POWER weather. If harvest dates are missing, the workflow may estimate a
crop-cycle duration and reports a warning; check those dates against the
actual trial.

### Example: calculate weather summaries offline

The following small series is simulated to demonstrate the helper. It is
not a weather record or a fitted crop model.

``` r

daily <- data.frame(
  T2M_MAX = c(25, 27, 24, 28, 31, 29),
  T2M_MIN = c(15, 17, 14, 18, 21, 19),
  PRECTOTCORR = c(2, 5, 0, 10, 1, 0),
  RH2M = c(60, 70, 50, 65, 80, 75),
  ALLSKY_SFC_SW_DWN = c(18, 20, 15, 22, 19, 21)
)
MERIDIAN::calculate_enviromic_indices(daily, dtf_mean = 2, dtm_mean = 4)
#>   MeanTemp_C TotalRainfall_mm GDD MeanRH_pct TotalSolar_MJ GDD_Veg
#> 1      22.33               18  74      66.67           115      22
#>   TotalRainfall_Veg MeanTemp_Veg GDD_Rep TotalRainfall_Rep MeanTemp_Rep
#> 1                 7           21      22                10           21
#>   HeatStressDays_Rep GDD_Late TotalRainfall_Late MeanTemp_Late
#> 1                  0       30                  1            25
```

This helper currently calculates growing degree days with a base
temperature of 10 °C and reproductive heat-stress days above 30 °C.
These fixed thresholds are not automatically calibrated to the chosen
crop. Stage boundaries use supplied mean days to flowering and maturity;
check that they match your biological question.

Dates can also be standardized before preparing environmental data:

``` r

MERIDIAN::parse_date_robustly(c("2026-05-21", "21/05/2026", "46163"))
#> [1] "2026-05-21" "2026-05-21" "2026-05-21"
```

Use ISO dates (`YYYY-MM-DD`) to avoid ambiguity between day/month and
month/day formats.

## 6. Inspect spatial patterns

In **Field Map**, select an environment and trait and specify field
row/column coordinates. Fit a SpATS model and inspect its diagnostics,
spatial trend, variance components and adjusted genotype means. Fixed
genotype effects cannot be separated from spatial trends when layout
positions perfectly confound them; this applies to the bundled example
and is diagnosed by the app.

The across-environment spatial summary averages separate fits
descriptively. Its standard error assumes independence. It is not a
joint spatial MET model or a fitted second-stage MET analysis.

## 7. Register and export results

Use the explicit **Send … to Reports** controls to retain outputs.
Registered plots and tables are snapshots with metadata and dataset
identity. The **Reports** module lets you inspect them, change export
styling, build composed figures and generate HTML/PDF reports. Downloads
apply current export settings even if a preview has not been refreshed.

## Interpretation and limits

- Identifiers must be complete and experimental units unique. Missing
  trait values are handled per analysis; non-finite observed
  measurements are invalid.
- Mixed models assume independent G×E effects and homogeneous plot-error
  variance. Factor-analytic covariance structures are not implemented.
- Environmental associations are exploratory. Correlation with weather
  does not establish a causal driver or validate a selection rule.
- Stage-specific weather indices use fixed temperature thresholds, and
  any estimated harvest dates require validation against trial records.
- Observe the reported imputation, coverage and exclusions when
  interpreting stability and adaptation results.
- Spatial means across environments are descriptive summaries of
  separate fits.

For a reproducible report, retain the original input, column mapping,
design, selection direction, model settings and registered outputs.
