# MERIDIAN ![MERIDIAN logo](reference/figures/meridian-mark.png)

**MERIDIAN** is an R Shiny application for analysing multi-environment
crop trials. It brings data validation, exploratory plots, design-aware
models, genotype stability, environmental covariates and report export
into one interface.

The name stands for **M**ulti-**E**nvironment **R**esearch
**I**ntegration: **D**ata **I**ntelligence and **A**gronomic
**N**etworks.

## Installation

Install from GitHub and launch the application:

``` r

install.packages("pak")
pak::pak("SalvaOsuna/MERIDIAN")

MERIDIAN::run_app()
```

R 4.3.0 or later is required. Windows users need Rtools matching their R
release; macOS users need Apple’s Command Line Tools. Linux users need a
compiler and development libraries. See the [installation
guide](https://salvaosuna.github.io/MERIDIAN/articles/installation.md)
for instructions for each system and optional PDF export requirements.

## Getting started

Choose **Load Example Data** in the welcome dialog to explore a
simulated trial with 20 genotypes, five environments, three replicates
and four traits. The example includes environmental covariates and works
without internet access.

You can also inspect those data directly in R:

``` r

trial <- read.csv(system.file(
  "extdata", "example_phenotypic.csv", package = "MERIDIAN"
))

dim(trial)
#> [1] 300  10

aggregate(GrainYield ~ Environment, data = trial, FUN = mean)
#>   Environment GrainYield
#> 1        ENV1   4.821864
#> 2        ENV2   4.041333
#> 3        ENV3   4.520847
#> 4        ENV4   5.225833
#> 5        ENV5   3.888833
```

These are raw descriptive means. Adjusted genotype means and formal
inference require the experimental-design settings in the app. The
[getting-started
guide](https://salvaosuna.github.io/MERIDIAN/articles/MERIDIAN.md)
explains data preparation and your first analysis.

## Analysis workflow

| Module | Main tasks |
|----|----|
| Overview | Import CSV or Excel; map genotype, environment, replicate, block and trait columns; validate records. |
| Traits | Inspect distributions, missing values, outliers, G×E means and environment correlations. |
| ANOVA | Fit design-adjusted ANOVA and mixed models; inspect variance components, Cullis generalized heritability, BLUEs and BLUPs. |
| Stability | Explore AMMI, GGE, Eberhart–Russell, Wricke and Shukla statistics. |
| Adaptation | Inspect observed winners, reaction norms, environmental PCA and covariate associations. |
| Field Map | Fit single-environment SpATS models and inspect spatial trends and adjusted means. |
| Reports | Register selected results, compose figures and export tables, plots and HTML/PDF reports. |

Analyses use the mapped design and the selected trait. Changing data or
model settings invalidates affected results. Outputs sent to Reports are
frozen snapshots, so later recalculation cannot silently change an
exported result.

See the [analysis and interpretation
guide](https://salvaosuna.github.io/MERIDIAN/articles/analysis-workflow.md)
for the full workflow, model assumptions and an environmental-index
example.

## Examples in R

The app is the main entry point. A small set of exported helpers can
also be used in scripts. For example, display correlations between
environments:

``` r

MERIDIAN::plot_env_correlation(
  trial,
  gen_col = "Genotype",
  env_col = "Environment",
  trait = "GrainYield",
  method = "pearson"
)
```

The [function
reference](https://salvaosuna.github.io/MERIDIAN/reference/index.md)
documents the launcher, environment-correlation plot, date parsing,
weather retrieval and agroclimatic indices. Weather retrieval requires
internet access; the examples in the guides use bundled or explicitly
simulated data.

## Exporting results

Send a plot or table from an analysis module to **Reports**. From there
you can download CSV/Excel tables, PNG/PDF/SVG/TIFF figures, composed
multi-panel figures or a complete HTML/PDF report. HTML reports need
Pandoc. PDF reports additionally need `pagedown` and Chrome, Chromium or
Edge.

## Statistical scope

MERIDIAN supports RCBD, incomplete-block and augmented designs when the
supplied data identify the required effects. Missing observations are
handled separately for each trait and analysis. The mixed model
currently assumes independent G×E effects and homogeneous residual
variance. Winner groups are exploratory; across-environment averages of
separate spatial fits are descriptive summaries.

Review design diagnostics, missing-cell processing and uncertainty
before using results for selection. Further details are in the [analysis
guide](https://salvaosuna.github.io/MERIDIAN/articles/analysis-workflow.html#interpretation-and-limits).

## Help and development

Report reproducible problems or request improvements in the [GitHub
issue tracker](https://github.com/SalvaOsuna/MERIDIAN/issues). Source
code, tests and development instructions are available in the
[repository](https://github.com/SalvaOsuna/MERIDIAN).
