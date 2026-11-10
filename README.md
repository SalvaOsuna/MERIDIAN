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

- Design-adjusted marginal ANOVA for genotype and environment, with a partial GxE test
- Variance components estimation using linear mixed models (`lme4`)
- Cullis generalized heritability from pairwise prediction-error variances
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

- Exploratory grouping of environments by the observed winning genotype, including ties and coverage
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
- Frozen results with dataset identity, analysis metadata and registration time
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

The registry stores immutable snapshots of the selected tables and static plots. Recalculating an analysis cannot silently change a saved item. Changes to data values, column mappings, design or environmental covariates invalidate live analyses and clear the registry. Changes to trait or model settings require rerunning the affected analysis. Downloads always apply the current export settings, even when the preview has not been refreshed.

## Installation

### Prerequisites

- R >= 4.3.0
- RStudio recommended
- Rtools is required to install this package from source on Windows
- Pandoc for HTML reports (the RStudio-bundled copy is detected automatically)
- Optional `pagedown` and Chrome/Edge for PDF reports; no LaTeX installation is needed

#### Linux system libraries

Installing from GitHub compiles MERIDIAN and some of its dependencies. On Ubuntu
or Debian, install the build tools and development libraries in a terminal first:

```sh
sudo apt-get update
sudo apt-get install -y build-essential r-base-dev cmake libuv1-dev \
  libcurl4-openssl-dev libssl-dev libxml2-dev libfontconfig-dev libfreetype-dev \
  libharfbuzz-dev libfribidi-dev libpng-dev libjpeg-dev libtiff-dev libwebp-dev libx11-dev
```

Pandoc is also needed for HTML reports; use the copy bundled with RStudio or
install it with `sudo apt-get install pandoc`. For the requirements specific to
your platform and current dependency versions, run:

```r
pak::pkg_sysreqs("SalvaOsuna/MERIDIAN")
```

If installation stops with `Configuration failed because libuv was not found`
while building `fs`, install `libuv1-dev` and retry the installation. A failed
dependency installation prevents `MERIDIAN::run_app()` from being available.

### Setup From GitHub

```r
install.packages("pak")
pak::pak("SalvaOsuna/MERIDIAN")

MERIDIAN::run_app()
```

To update an existing installation, restart R in RStudio, then run:

```r
pak::pkg_install("SalvaOsuna/MERIDIAN", upgrade = TRUE)
MERIDIAN::run_app()
```

The bundled example works without internet access. Uploaded environmental data
with coordinates and planting/harvest dates can retrieve daily weather from
NASA POWER; if retrieval fails, the original uploaded covariates remain available.
The interface uses a system font fallback when Google Fonts cannot be reached.

### Setup For Local Development

```r
# From the cloned repository
install.packages(c("devtools", "usethis"))
devtools::install_deps(dependencies = TRUE)
devtools::document()
source("run_app_dev.R")
```

If the folder was copied from another operating system, clear old compiled files
before loading the development version:

```r
pkgbuild::clean_dll(".")
source("run_app_dev.R")
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

MERIDIAN includes C++ kernels for GxE aggregation, grouped summaries, outlier detection, spatial aggregation and stability metrics. Package installation requires compilation; the statistical helpers also have tested R implementations when native symbols are unavailable. Missing-cell imputation and complete-cell subset selection use the reviewed R algorithms.

Mixed models use lme4's fitting engine; the package's custom C++ kernels do not replace REML optimization. Large fixed-effect ANOVAs use equivalent fits within each environment, with a general-model fallback when cell means are not estimable. Recent analyses are cached within each session (two per module); changing the selection direction only reorders predictions.

On a simulated 100-genotype × 4-environment × 20-replicate dataset with nine traits and 1% missing values, one-trait ANOVA fell from 9.38 to 0.68 seconds, and G×E aggregation plus plot-error estimation from 1.55 to 0.25 seconds on the development machine. These are indicative single-run timings, not guaranteed runtimes. See `docs/reviews/performance_2026-09-08.md` for scope and validation.

## Statistical scope

- Identifiers must be complete, and each genotype/environment/replicate/block record must identify a unique experimental unit. Traits must be numeric with finite observed values. Missing trait values are excluded separately for each analysis.
- Block identifiers are nested within replicate and environment. Augmented designs require replicated check genotypes. Models need identifiable design contrasts and enough replication to separate GxE from plot error.
- ANOVA treats genotype, environment and design effects as fixed. Its marginal/partial sums of squares are not additive percentages. Non-estimable contrasts are reported explicitly.
- BLUPs treat genotype and GxE as random and environment as fixed. Predictions average environments equally; approximate prediction intervals include fixed-effect estimation uncertainty. BLUEs include random environment, GxE and design terms. The models assume independent GxE effects and homogeneous plot-error variance.
- AMMI/GGE retain all observed genotype and environment levels. Missing cell means use least-squares additive predictions only when the network is connected; exported processing tables identify imputations. AMMI ANOVA uses observed plot data, never imputed means. Without plot-error degrees of freedom, AMMI is descriptive.
- Classical stability indices use a disclosed complete-cell subset, retaining unequal replication. Eberhart-Russell deviation variance subtracts the plot-error contribution. The combined table uses all available AMMI axes; the AMMI tab uses the number of axes selected by the user.
- Observed winners are exploratory, with a higher/lower selection objective; they do not establish repeatable mega-environments.
- Across-environment spatial averages are descriptive averages of separate fits, with an SE under independence. They are not a two-stage MET model. Fixed genotype effects cannot be separated from spatial trends in layouts with perfectly confounded genotype positions, including the bundled example; the app diagnoses this case.
- Excluding flagged outliers replaces values with NA only for the chosen trait. Original values can be restored in the same session.

## Validation

From the project root:

```r
source("tools/validate_app.R")
source("tools/check_package.R")
```

The tests cover statistical invariance, native/R agreement, missing cells, invalid data, Shiny state, spatial predictions, Excel round trips, image file signatures and report content. Browser checks are in `tools/browser_validation.cjs` and `tools/browser_tabs.cjs`; start `tools/launch_validation_app.R`, then run these with Node and Playwright. Outputs are written to the ignored `validation-output/` directory.

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
- **Reports and export:** rmarkdown, knitr, openxlsx, svglite, pagedown
- **Performance integrations:** Rcpp

## Author

**SalvaOsuna**

## License

MIT License.
