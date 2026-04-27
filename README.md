# MERIDIAN <img src="inst/app/www/logo.png" align="right" height="140"/>

`MERIDIAN` (**M**ulti-**E**nvironment **R**esearch **I**ntegration: **D**ata **I**ntelligence & **A**gronomic **N**etworks) is a comprehensive **R Shiny** application for Multi-Environment Trial (MET) phenotypic data analysis. It is packaged with the `golem` framework so it can be installed, documented, tested, and launched like a regular R package.

## Features

### Module 1 - Data Upload & Validation

- Upload CSV or Excel files
- Dynamic column mapping with auto-detection
- Manual experimental design selection or auto-detection
- Augmented design support with user-selected control checks
- Experimental design detection (RCBD, Alpha-Lattice, Augmented)
- Data preview with interactive tables
- Missing value and duplicate detection
- Descriptive statistics by environment and genotype

### Module 2 - Exploratory Data Analysis

- Interactive boxplots by environment or genotype
- GxE means heatmap with hierarchical clustering
- Environment correlation matrix (Pearson, Spearman, Kendall)
- Outlier detection (IQR / Z-score) with visualization
- Downloadable summary tables (CSV / Excel)
- Send selected plots and tables to the Reports module

### Module 3 - ANOVA & Variance Components

- Two-way ANOVA for genotype, environment, and GxE interactions
- Variance components estimation using linear mixed models (`lme4`)
- Broad-sense heritability estimation
- BLUEs and BLUPs tables and plots
- Report export for ANOVA tables, variance partition plots, BLUEs, and BLUPs

### Module 4 - Stability Analysis

- AMMI analysis and biplots (AMMI1, AMMI2)
- GGE biplots
- Eberhart-Russell regression
- Wricke ecovalence, Shukla variance, and combined rankings
- Interactive stability ranking plots and tables
- Report registry support for AMMI, GGE, stability ranking plots, and stability tables

### Module 5 - Adaptation & Enviromics

- Native mega-environment clustering
- Reaction norm plots
- Environmental typology via PCA
- Covariate-phenotype correlation heatmaps
- Interactive geospatial trial maps
- Optimized phenotypic adaptation rendering for large METs
- Send selected adaptation and reaction norm plots to Reports

### Module 6 - Spatial Trends

- Fit SpATS spatial models for single-environment spatial trend analysis
- ggplot-based spatial trend maps derived from SpATS model outputs
- Customizable plot outputs with annotations, missing data display, and raw/percentage trend scales
- Dynamic P-spline segments and user-defined fixed/random terms
- Variance components with generalized heritability and variance partitioning
- Extract and download adjusted phenotypic means (BLUEs/BLUPs)
- Batch adjusted means across environments
- Send spatial trend maps, variance partition plots, and adjusted means tables to Reports

### Module 7 - Reports & Export

- Central user-driven Report Registry for plots and tables explicitly sent from analysis modules
- Lightweight report items with metadata and reproducible builder functions instead of duplicated datasets or rendered widgets
- Registered plot and table libraries with preview, removal, and export controls
- Plot export studio with typography, label, theme, resolution, and white-background download controls
- Patchwork figure composer with custom layouts, row/column controls, guide and axis collection, panel tags, global annotations, shared themes, and ZIP panel export
- Stable preview workflow where figures update only when the user refreshes the preview
- Automated HTML/PDF report generation from selected registered plots, tables, and composed figures

## Report Registry Workflow

MERIDIAN uses a session-level Report Registry to connect the analysis modules with publication-style exports.

1. Run an analysis and inspect a plot or table.
2. Click **Send this plot to Reports** or **Send this table to Reports**.
3. Open **Reports & Export**.
4. Preview, customize, remove, or export registered items.
5. Combine two or more registered ggplot-compatible plots in the Patchwork composer.
6. Export individual plots, composed figures, tables, or a full report.

The registry stores lightweight report items with module, trait, item type, metadata, timestamp, dataset signature, and a zero-argument builder function. It avoids storing large duplicated datasets and does not pass plotly/htmlwidget objects into the figure composer.

## Installation

### Prerequisites

- R >= 4.3.0
- RStudio recommended
- Rtools recommended on Windows for C++ acceleration

### Setup From GitHub

```r
install.packages("pak")
pak::pak("SalvaOsuna/MERIDIAN")

MERIDIAN::run_app()
```

### Setup For Local Development

```r
# From the cloned repository
install.packages(c("devtools", "usethis"))
devtools::install_deps(dependencies = TRUE)
devtools::document()
devtools::load_all()

run_app()
```

Dependencies are declared in `DESCRIPTION`. When adding a runtime dependency, use `usethis::use_package("package_name")`; for development-only tooling, use `usethis::use_package("package_name", type = "Suggests")`.

## Input Data Format

The app expects data in **long format** with at minimum:

| Column | Description |
|---|---|
| `gen` / `Genotype` | Genotype identifier |
| `env` / `Environment` | Environment identifier |
| `rep` / `Rep` | Replicate number |
| `block` optional | Block within replicate |
| `row` / `Row` optional | Row coordinate for spatial analysis |
| `col` / `Col` optional | Column coordinate for spatial analysis |
| numeric columns | Phenotypic traits such as yield or height |

### Example

| Genotype | Environment | Rep | Row | Col | GrainYield | PlantHeight |
|---|---|---|---|---|---|---|
| G01 | ENV1 | 1 | 1 | 1 | 3.52 | 78.2 |
| G01 | ENV1 | 2 | 2 | 3 | 3.71 | 80.1 |
| G02 | ENV1 | 1 | 1 | 2 | 4.10 | 65.4 |

An example dataset is included in `inst/extdata` and can be loaded directly from the app.

## Performance

MERIDIAN includes optional C++ acceleration through `Rcpp` for large MET datasets. The C++ kernels live in `src/` and are linked through `Rcpp` attributes during package documentation/build. Current kernels support balanced subset pruning, GxE means, additive GxE imputation, grouped summaries, grouped outlier detection, spatial cell aggregation, AMMI metrics, Finlay-Wilkinson regression, and Eberhart-Russell diagnostics.

If C++ compilation is unavailable, the app warns the user and falls back to R implementations so the workflow remains usable.

## Deployment

### Local

```r
MERIDIAN::run_app()
```

### shinyapps.io

```r
rsconnect::deployApp(appDir = system.file("app", package = "MERIDIAN"))
```

## Notes

- If the app logo does not appear, hard refresh the browser (`Ctrl+F5`) to clear cached static assets.
- The app logo and stylesheet are bundled under `inst/app/www` and served via `/assets/`.
- To test locally, use `devtools::load_all(); run_app()` or install the package and run `MERIDIAN::run_app()`. You no longer launch the app by clicking **Run App** on a root-level `app.R`.
- Downloaded figures use a white background for PNG, PDF, SVG, and TIFF output.
- The Report Registry is cleared when a new dataset is loaded, preventing stale report items from being exported with mismatched data.

## Tech Stack

- **UI framework:** bslib / Bootstrap 5
- **Visualization:** ggplot2, plotly, heatmaply, patchwork
- **Data handling:** dplyr, tidyr, readr, readxl
- **Statistical analysis:** metan, lme4, emmeans, SpATS
- **Reports and export:** rmarkdown, knitr, kableExtra, openxlsx, svglite
- **Performance integrations:** Rcpp

## Author

**SalvaOsuna**

## License

MIT License - see [LICENSE.md](LICENSE.md) for details.
