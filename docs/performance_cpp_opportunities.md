# MERIDIAN C++ Performance Opportunities

This audit focuses on code paths that can become expensive with large MET datasets. The app already uses `src/fast_pruning.cpp` for balanced subset pruning, GxE means, AMMI metrics, and Finlay-Wilkinson regressions. The next useful C++ targets are the repeated dense summaries and matrix-building steps that happen before model fitting or plotting.

## Implemented in `src/fast_pruning.cpp`

- `cpp_impute_additive_gxe()` accelerates additive missing-cell imputation for sparse G x E matrices.
- `cpp_er_statistics()` computes Eberhart-Russell slope, intercept, S2di, and R2 in one C++ pass.
- `cpp_group_numeric_summary()` accelerates grouped N, mean, SD, min, max, and CV summaries.
- `cpp_detect_outliers()` accelerates grouped IQR and z-score outlier detection.
- `cpp_spatial_cell_summary()` accelerates row-column cell aggregation for SpATS-derived ggplot maps.

All integrations keep R fallbacks so MERIDIAN remains usable if local Rtools/Rcpp compilation is unavailable.

## Highest Impact

1. `R/stability.R`: GxE matrix construction and missing-cell imputation

   `prepare_gge_data()` and `build_ge_matrices()` already benefit from `cpp_ge_means()`, but the additive missing-cell imputation loop in `prepare_gge_data()` still runs in R. For sparse large G x E matrices, moving the missing index fill into C++ would avoid repeated R indexing and speed up GGE preprocessing.

2. `R/mod_spatial.R`: spatial plot grid aggregation

   `build_spats_plot_grid()` groups row-column cells and averages raw, fitted, residual, and genotype-predicted values before making the ggplot maps. Large high-density spatial trials can make this aggregation visible in the UI. A C++ helper that accepts numeric row/column vectors plus value columns and returns a compact cell-summary table would improve rendering responsiveness.

3. `R/utils.R` and `R/data_processing.R`: validation summaries and duplicate checks

   `validate_met_data()`, `check_missing_values()`, duplicate genotype-environment-rep checks, and descriptive summaries repeatedly count distinct levels and missing values across large tables. These are not model bottlenecks, but they run often while users map columns. A small C++ counting/summarising backend would make upload validation feel snappier.

## Medium Impact

4. `R/utils.R`: `descriptive_summary()`

   This loops over traits and uses grouped dplyr summaries. For many traits and many environments, a C++ grouped summary could calculate N, mean, SD, min, max, and CV in one pass per grouping variable.

5. `R/mod_eda.R`: outlier detection

   `detect_outliers()` is grouped and trait-specific. The IQR and z-score methods are good candidates for C++ if users screen many traits or environments interactively.

6. `R/stability.R`: Eberhart-Russell residual summaries

   `compute_er_table_fast()` still builds fitted matrices and residual sums in R after C++ regression. A C++ extension could return slope, intercept, SSE, S2di, and R2 together, reducing intermediate matrix allocation.

## Lower Impact Or Not Recommended

7. Model fitting in `R/models.R` and `R/mod_spatial.R`

   The expensive work is delegated to `lme4`, `emmeans`, and `SpATS`. Rewriting those solvers in project-level C++ is not practical. The better optimization path is preprocessing, caching, and avoiding repeated fits.

8. Plot composition and report registry

   Patchwork, ggplot, and export work is mostly graphics-device bound. C++ would not help much. The right optimization is keeping registry items lightweight and using explicit preview refreshes, which the app now does.

## Suggested Next C++ Milestone

Add a new `src/fast_group_summary.cpp` with:

- grouped missing counts by column
- grouped numeric summaries by one factor
- row-column cell aggregation for spatial maps
- optional sparse GxE imputation helper

This would complement the existing `src/fast_pruning.cpp` without changing statistical results or replacing trusted model-fitting packages.
