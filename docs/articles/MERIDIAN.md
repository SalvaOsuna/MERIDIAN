# Getting started with MERIDIAN

MERIDIAN is a Shiny application for analysing crop trials across
environments. You can run the main workflow through the interface after
installing the package. The R examples below read the same bundled data
and are evaluated when this guide is built.

## Launch the app

``` r

MERIDIAN::run_app()
```

Choose **Load Example Data**. The simulated example has 20 genotypes,
five environments and three replicates, with four traits: `GrainYield`,
`PlantHeight`, `DaysToFlowering` and `TKW`. Some measurements are
intentionally missing.

The website documents the package; it does not host a running Shiny
session.
[`run_app()`](https://salvaosuna.github.io/MERIDIAN/reference/run_app.md)
starts the app on your own computer.

## Inspect the example in R

``` r

trial <- read.csv(system.file(
  "extdata", "example_phenotypic.csv", package = "MERIDIAN"
))
dim(trial)
#> [1] 300  10
head(trial)
#>   Genotype Environment Rep Block Row Col GrainYield PlantHeight DaysToFlowering
#> 1      G01        ENV1   1    B1   1   1       5.96        97.2              57
#> 2      G02        ENV1   1    B1   2   1       4.60        90.2              57
#> 3      G03        ENV1   1    B1   3   1       4.33        84.9              61
#> 4      G04        ENV1   1    B1   4   1       4.91        93.9              59
#> 5      G05        ENV1   1    B1   5   1       4.28        80.8              62
#> 6      G06        ENV1   1    B1   6   1       4.53        85.6              61
#>    TKW
#> 1 44.4
#> 2 39.8
#> 3 36.1
#> 4 40.1
#> 5 35.8
#> 6 38.8
```

``` r

traits <- c("GrainYield", "PlantHeight", "DaysToFlowering", "TKW")
data.frame(
  Trait = traits,
  Observed = vapply(trial[traits], function(x) sum(!is.na(x)), integer(1)),
  Missing = vapply(trial[traits], function(x) sum(is.na(x)), integer(1)),
  row.names = NULL
)
#>             Trait Observed Missing
#> 1      GrainYield      298       2
#> 2     PlantHeight      298       2
#> 3 DaysToFlowering      299       1
#> 4             TKW      299       1
```

## Prepare your own data

Use long format: each row represents a plot or experimental unit, and
each numeric trait has its own column. At minimum, supply genotype,
environment and replicate identifiers and one trait. Include block
identifiers for incomplete blocks, and row/column coordinates for
spatial analysis.

| Column in the example | Role                                            |
|-----------------------|-------------------------------------------------|
| `Genotype`            | Crop genotype or cultivar identifier            |
| `Environment`         | Site, year or site-year trial identifier        |
| `Rep`                 | Replicate within an environment                 |
| `Block`               | Optional block within replicate and environment |
| `Row`, `Col`          | Field coordinates for spatial analysis          |
| `GrainYield`, etc.    | Numeric response traits                         |

In **Overview**, upload CSV or Excel and check the detected column
mappings. Choose the experimental design, or verify the automatic
choice. Augmented designs require the replicated check genotypes to be
specified.

Resolve missing identifiers, infinite values and duplicate experimental
units before fitting models. Missing trait values may remain; their
treatment is reported by each analysis. Recognized spreadsheet error
tokens in otherwise numeric columns are imported as missing values with
a validation notice.

## Explore the trait

In **Traits**, select the response and inspect distributions by
environment, G×E means, correlations and outliers. Here is a descriptive
view of the example:

``` r

ggplot2::ggplot(trial, ggplot2::aes(Environment, GrainYield)) +
  ggplot2::geom_boxplot(fill = "#dceadf", colour = "#276749", na.rm = TRUE) +
  ggplot2::labs(x = "Environment", y = "GrainYield (example scale)") +
  ggplot2::theme_bw(base_size = 11)
```

![Boxplots of simulated grain yield across the five example
environments.](MERIDIAN_files/figure-html/trait-distribution-1.png)

These are raw measurements, not adjusted genotype predictions. Outlier
flags are prompts for inspection. If you exclude flagged values in the
app, exclusion affects only the chosen trait and can be reversed in the
session.

## Run the first analysis

1.  Open **ANOVA** and select `GrainYield`.
2.  Set the selection objective, such as **Higher is better**.
3.  Click **Run Analysis**.
4.  Inspect the adjusted ANOVA, variance components, BLUEs and BLUPs.
5.  Use **Send table to Reports** or **Send this plot to Reports** for
    outputs you want to retain.

Changing a trait, dataset or model setting requires rerunning the
affected analysis. The [analysis
workflow](https://salvaosuna.github.io/MERIDIAN/articles/analysis-workflow.md)
explains the remaining modules and interpretation limits.

## Explore environment correlations in a script

``` r

MERIDIAN::plot_env_correlation(
  trial, gen_col = "Genotype", env_col = "Environment",
  trait = "GrainYield", method = "pearson"
)
```

This helper correlates genotype means between environments using the
available pairs. It does not estimate a genetic covariance model.

## Export a report

Open **Reports** to inspect registered plots and tables. Export
individual outputs or combine compatible plots in the figure composer.
Include the selected items in an HTML/PDF report. See the [installation
guide](https://salvaosuna.github.io/MERIDIAN/articles/installation.html#reports-and-optional-tools)
for Pandoc and browser requirements.
