# Larger MET datasets: performance verification

The benchmark matches the reported dimensions: 100 genotypes, four environments,
20 replicates per environment (8,000 rows), and nine numeric traits. One percent
of each trait is missing. Data are simulated with seed 712; these are not the
user's actual observations. Timings below are single runs on the development
machine, excluding package loading and browser rendering.

| Stage, one trait | Before (s) | After (s) |
|---|---:|---:|
| Design-adjusted ANOVA | 9.38 | 0.68 |
| REML fit for BLUPs | 0.49 | 0.51 |
| BLUP prediction uncertainty | 0.03 | 0.03 |
| REML fit for BLUEs | 2.58 | 2.85 |
| BLUE intervals | 0.22 | 0.25 |
| G×E aggregation and plot-error estimation | 1.55 | 0.25 |

The first five stages total 12.70 seconds before and 4.32 seconds after. The
improvement comes from the ANOVA algorithm; the mixed-model fitting algorithm
and inference settings were preserved. Ordinary timing variability explains
small differences in unchanged stages.

## What C++ accelerates

The compiled G×E and stability kernels were loaded and active in both benchmark
versions. Other native kernels serve grouped summaries, outlier detection and
spatial aggregation. They do not replace the optimizer used for mixed models.
REML fits use lme4, which itself uses compiled numerical routines. SpATS has its
own fitting procedure. Thus, having the native MERIDIAN kernels loaded does not
make every operation equally fast.

## Changes

The general ANOVA path now reuses the fitted model's pivoted QR decomposition
instead of decomposing the full observation-by-coefficient matrix again by SVD.
For more than 1,000 observed plots, the full fixed model is decomposed into
independent fits by environment. Genotype/environment tests combine the same
estimable marginal means and covariance information. Interaction still uses the
appropriate reduced additive model. Missing or non-estimable cells fall back to
the general path. Plot-error estimation also uses fits by environment.

Each Shiny module retains at most two recent successful analysis results within
its session. A changed dataset or model specification still invalidates live
results; clicking Run on an exact recent specification reuses its fit. Changing
the higher/lower selection objective only reorders predictions and ranks.

## Correctness and scope

The original and optimized ANOVA tables agree to a relative tolerance of 1e-8 on
the 8,000-row benchmark. BLUE tables and BLUP covariance results also agree.
Regression tests additionally compare the optimized and full fixed model for
an incomplete-block design with missing observations, and check residual mean
squares, degrees of freedom, cache reuse and result invalidation.

All nine traits completed ANOVA, BLUE and BLUP calculation with 100 genotype
estimates each and finite uncertainty values (5.28–6.64 seconds per trait in a
separate validation run). Two actual browser runs took 7.403–7.741 seconds for
the first trait, 6.740–6.769 for the second, 0.602–0.633 for a repeated analysis,
and 0.466–0.496 for a return to a cached trait. Reversing selection direction
took 0.832–0.835 seconds.
Browser timings include rendering and explicit settling waits; other validation
processes were running concurrently. CSV, Excel and PNG downloads succeeded for
the 8,000-row dataset.

The existing emmeans defaults use asymptotic intervals above 3,000 observations;
this behavior was not introduced as a performance shortcut in this change.
Spatial simulations at 2,000 plots per site took 4.59 seconds with random
genotypes and 3.53 seconds with fixed genotypes, using a 40 × 50 layout and
20 × 20 spline segments. Different layouts and convergence behavior can take
longer. These separate fits do not constitute a joint spatial MET model.

## Reproduction and evidence

- `tools/benchmark_models.R user-blocked user`: timing stages and saving estimates.
- `tools/validate_large_traits.R`: checking fits and uncertainty for all nine traits.
- `tools/benchmark_spatial.R`: timing separate spatial fits.
- `tools/browser_performance.cjs`: actual upload, model fits, cached repeat,
  selection direction, second trait, return to cached trait, CSV/Excel/PNG downloads.
- `tools/validate_app.R` and `tools/check_package.R`: regression and package checks.

Machine-specific evidence is in the ignored `validation-output/performance/`
directory: before/after timing CSVs and result RDS files, all-nine-traits CSV,
browser timing JSON, spatial timing CSV, and downloaded large-dataset artifacts.
`validation-output/latest-check.txt` identifies the latest package-check directory.

Final validation: R CMD check finished with Status: OK (zero errors, warnings or
notes). The browser example workflow passed ANOVA, stability, adaptation,
individual/batch spatial analysis, result invalidation, and CSV/XLSX/PNG/SVG/TIFF/
PDF/ZIP/HTML exports. Artifact inspection confirmed the 12-page PDF contains all
selected sections, six SVG panels in the ZIP, and 4,200 × 3,000 pixel PNG/TIFF
images. The large-dataset ANOVA chart was also visually inspected after removing
an unnecessary text-only legend.
