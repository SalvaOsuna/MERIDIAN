# MERIDIAN 0.1.1 fixes and validation

Date: September 7, 2026. Review applied directly to the package and Shiny app.

## Changes

| Area | Fix and evidence |
|---|---|
| Compilation | Removed `safeBackup` files and obsolete C++ exports. Regenerated Rcpp bindings and documentation. Corrected licensing and export dependencies. |
| Experimental design | Shared model preparation; blocks nested within replicate and environment. Tested reused and unique block and replicate labels. Validated checks for augmented designs. |
| ANOVA | Estimable marginal contrasts for G and E, with a partial G×E test. Adjusted sums of squares are not presented as additive percentages. Tested imbalance without a true genotypic effect. |
| BLUEs | Included G×E and design factors; supported confidence intervals with finite and asymptotic degrees of freedom. |
| BLUPs and heritability | Predictions centered on the adjusted environmental mean with equal environment weights. Uncertainty from mixed-model equations and Cullis generalized H². Invariance to reference environment and replicate coding. |
| Stability | Least-squares additive imputation in connected networks. Eberhart–Russell corrected for experimental error. Sequential removal of incomplete levels while preserving unequal replication and checks. R/C++ equivalence tests. |
| AMMI | Decomposition of cell means; ANOVA on observed plots and experimental design. Handles zero interaction, missing cells and unavailable error degrees of freedom. |
| Adaptation | Higher/lower selection direction; visible ties and genotype coverage. Exploratory grouping by winner, without claiming validated mega-environments. |
| Covariates | Excluded numeric identifiers and constant variables from PCA. Unique environment identity; correlations with pairwise sample sizes. Exported scores, loadings, correlations and adaptation tables. |
| Shiny state | Signature includes values, roles, design and covariates. Invalidated analyses when specifications change. Report items are immutable. Tested changes to trait, environment and datasets of the same size. |
| Spatial analysis | Labels tied to the fitted result. Tested fixed genotypes with an identifiable design. Diagnosed confounding between genotype and spatial trend. Across-environment means labeled descriptive, with SE under independence. |
| Outliers | Exclusion now replaces only flagged values of the selected trait with NA. Original values can be restored. |
| Tables | CSV and Excel, including zero-row and one-row workbooks. Uploaded files read using their original filename. Small p values preserved in scientific notation. |
| Plots | PNG, SVG, TIFF and PDF; style controls do not mutate registered originals. Downloads use current settings even without refreshing the preview. |
| Composed figures | Corrected layout operators, panel labels and ZIP filenames. Reports regenerate compositions from the selected panels. |
| Reports | Include all selected tables and plots, including spatial maps and composed figures. Methods derived from existing results. Detect RStudio's Pandoc outside RStudio; self-contained HTML and PDF through Edge/Chrome. |

## Results for the four example traits

| Trait | Observed plots | Genotypes | Environments | Cullis H² | Mean BLUP |
|---|---:|---:|---:|---:|---:|
| GrainYield | 298 | 20 | 5 | 0.896067 | 4.495981 |
| PlantHeight | 298 | 20 | 5 | 0.949200 | 87.765246 |
| DaysToFlowering | 299 | 20 | 5 | 0.911343 | 59.841906 |
| TKW | 299 | 20 | 5 | 0.939208 | 38.630690 |

All four completed ANOVA, mixed models, BLUEs, BLUPs, AMMI, GGE and
Eberhart–Russell. No G×E cell in the example required imputation, although some
plot measurements are missing. Synthetic tests cover imputation and disconnected
networks.

## App and artifact validation

Edge browser checks covered six exploratory tabs, four ANOVA tabs, eight
stability tabs and two adaptation tabs. They checked individual spatial fits,
the five-environment summary and changes in result identity. Server tests cover
fixed and random spatial genotype effects and results registered before changing
the selected environment.

Verified downloads include CSV, Excel, PNG, SVG, TIFF, PDF, an SVG composition, a
six-panel ZIP and HTML/PDF reports. PNG/TIFF exports requested at 7 × 5 inches and
600 dpi contain 4200 × 3000 pixels. The spatial summary contains 20 genotypes and
five environments per genotype. The test PDF includes every selected output
family, environmental tables, spatial maps, the composed figure and methods; its
text and rendered pages were inspected.

Evidence is saved in `validation-output/`, excluded from Git and the package.
`artifact-verification.json` records content checks;
`example-analysis-summary.csv` contains numerical results. `latest-check.txt`
identifies the latest full installation check directory.

## Statistical scope

The mixed model assumes independent G×E and homogeneous residual variance. It
does not implement factor-analytic covariance structures or a joint spatial MET
model. Across-environment spatial means are descriptive summaries of independent
fits. In the example, repeated genotype positions confound fixed genotype effects
with spatial trends; the app diagnoses this, and fitting random genotypes works.
These limits are documented in the README and relevant app messages.

Installed dependencies may emit version, zero variance-component or deprecated
interface warnings. These do not necessarily indicate calculation failure; zero
components are identified in the ANOVA panel. The final package check is retained
with the evidence to distinguish such warnings from installation or test errors.

## Reproduce

```r
source("tools/validate_app.R")
source("tools/validate_examples.R")
source("tools/check_package.R")
source("run_app_dev.R")
```

Browser tests are in `tools/browser_validation.cjs` and `tools/browser_tabs.cjs`,
using Node, Playwright and a local instance started by
`tools/launch_validation_app.R`. Downloaded-artifact checks are in
`tools/verify_artifacts.py`.

Reference documentation: [metan: Eberhart–Russell regression](https://tiagoolivoto.github.io/metan/reference/ge_reg.html),
[emmeans: supported models and inference](https://rvlenth.github.io/emmeans/articles/models.html)
and [SpATS: heritability](https://search.r-project.org/CRAN/refmans/SpATS/html/getHeritability.html).
