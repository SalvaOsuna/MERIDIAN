# MERIDIAN 🌾

![MERIDIAN Logo](www/logo.png)

**M**ulti-**E**nvironment **R**esearch **I**ntegration: **D**ata **I**ntelligence & **A**gronomic **N**etworks

A comprehensive **R Shiny** application for Multi-Environment Trial (MET) phenotypic data analysis. Built for plant breeders, geneticists, and agronomists to streamline the statistical analysis workflow from raw data to publication-ready results.

## Features

### Module 1 — Data Upload & Validation
- Upload CSV or Excel files
- Dynamic column mapping with auto-detection
- Manual experimental design selection (or auto-detection)
- Augmented design support with user-selected control checks
- Experimental design detection (RCBD, Alpha-Lattice, Augmented)
- Data preview with interactive tables
- Missing value and duplicate detection
- Descriptive statistics by environment and genotype

### Module 2 — Exploratory Data Analysis
- Interactive boxplots by environment or genotype (plotly)
- G×E means heatmap with hierarchical clustering (heatmaply)
- Environment correlation matrix (Pearson, Spearman, Kendall)
- Outlier detection (IQR / Z-score) with visualization
- Downloadable summary tables (CSV / Excel)

### Module 3 — ANOVA & Variance Components
- Two-way ANOVA (Genotype, Environment, and G×E interactions)
- Variance components estimation using linear mixed models (`lme4`)
- Broad-sense heritability estimation
- Mean performance bar plots with standard errors

### Module 4 — Stability Analysis
- Parametric stability indices (Shukla's variance, Wricke's ecovalence, superiority measure)
- Non-parametric stability indices (Fox, Thennarasu, Huehn)
- AMMI (Additive Main effects and Multiplicative Interaction) analysis and biplots (AMMI1, AMMI2)
- GGE (Genotype main effect plus G×E interaction) biplots
- Interactive stability ranking plots and tables
- Fast native stability kernels for large datasets (C++-accelerated GxE means, AMMI metrics, and Finlay-Wilkinson regression)

### Module 5 — Adaptation & Enviromics
- Native Mega-Environment clustering (Which-won-where)
- Reaction Norms (Finlay-Wilkinson) joint regression plots
- Environmental Typology via Principal Component Analysis (PCA)
- Covariate-Phenotype Correlation heatmaps
- Interactive Geo-Spatial Trial Maps showing site productivity
- Optimized phenotypic adaptation rendering (native plotly traces for large METs)

### Module 6 — Spatial Trends
- Fit SpATS spatial models to visualize within-environment spatial trends
- Customizable plot outputs (annotations, missing data, raw/percentage scales)
- Dynamic P-spline segments and user-defined Fixed/Random terms
- Variance Components tab with Generalized Heritability and interactive variance partitioning
- Extract and download adjusted phenotypic means (BLUPs/BLUEs)
- Perform single environment spatial analysis or batch process across all environments
- Auto-detects row and column coordinates

### Module 7 — Reports & Export
- Unified table export center with CSV and Excel downloads (metadata cover sheet + formatting)
- Plot export studio with typography, label, theme, and resolution controls
- Patchwork figure composer with panel labels, global annotations, and ZIP panel export
- Automated HTML/PDF report generation via parameterized R Markdown template

## Installation

### Prerequisites
- R ≥ 4.3.0
- RStudio (recommended)

### Setup

```r
# Clone the repository
# git clone https://github.com/SalvaOsuna/MERIDIAN.git

# Install required packages
install.packages(c(
  "shiny", "bslib", "shinyWidgets", "DT", "shinycssloaders",
  "thematic", "readr", "readxl", "dplyr", "tidyr",
  "ggplot2", "plotly", "heatmaply", "scales", "openxlsx",
  "metan", "lme4", "emmeans", "SpATS", "Rcpp"
))

# Run the app
shiny::runApp()
```

### Using renv (recommended)
```r
# Restore the package environment
renv::restore()

# Run the app
shiny::runApp()
```

## Input Data Format

The app expects data in **long format** with at minimum:

| Column | Description |
|--------|-------------|
| `gen` / `Genotype` | Genotype identifier |
| `env` / `Environment` | Environment identifier |
| `rep` / `Rep` | Replicate number |
| `block` (optional) | Block within replicate |
| `row` / `Row` (optional) | Row coordinate for spatial analysis |
| `col` / `Col` (optional) | Column coordinate for spatial analysis |
| *numeric columns* | Phenotypic traits (yield, height, etc.) |

### Example

| Genotype | Environment | Rep | Row | Col | GrainYield | PlantHeight |
|----------|-------------|-----|-----|-----|------------|-------------|
| G01 | ENV1 | 1 | 1 | 1 | 3.52 | 78.2 |
| G01 | ENV1 | 2 | 2 | 3 | 3.71 | 80.1 |
| G02 | ENV1 | 1 | 1 | 2 | 4.10 | 65.4 |

An example dataset is included and can be loaded directly from the app.

## Deployment

### Local
```r
shiny::runApp()
```

### shinyapps.io
```r
rsconnect::deployApp()
```

## Notes

- If the app logo does not appear, hard refresh the browser (`Ctrl+F5`) to clear cached static assets.
- The app now serves the logo via `/assets/logo.png` (mapped from `www/`), which is robust for local and deployed runs.

## Tech Stack

- **UI Framework:** [bslib](https://rstudio.github.io/bslib/) (Bootstrap 5)
- **Visualization:** ggplot2, plotly, heatmaply
- **Data handling:** dplyr, tidyr, readr, readxl
- **Statistical analysis:** metan, lme4, emmeans, SpATS
- **Performance integrations:** Rcpp (C++ algorithms for handling large G×E datasets)

## Author

**SalvaOsuna**

## License

MIT License — see [LICENSE](LICENSE) for details.
