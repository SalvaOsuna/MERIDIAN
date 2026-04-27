#' MERIDIAN
#'
#' @keywords internal
#' @importFrom grDevices chull
#' @importFrom graphics Axis plot.new text
#' @importFrom grid arrow unit
#' @importFrom dplyr %>% all_of arrange bind_rows everything filter group_by left_join mutate n rename select slice_min starts_with summarise
#' @importFrom ggplot2 aes element_text geom_col geom_hline geom_line geom_point geom_segment geom_text geom_tile geom_vline ggplot labs scale_color_gradient scale_color_manual scale_fill_gradient2 scale_shape_manual scale_y_continuous theme theme_bw
#' @importFrom Rcpp sourceCpp
#' @importFrom rlang .data
#' @importFrom stats anova complete.cases cor lm median prcomp predict quantile reorder sd
#' @importFrom shiny isolate need observe observeEvent removeModal req showModal validate
#' @importFrom tibble column_to_rownames
#' @importFrom tidyr pivot_wider
#' @importFrom utils getFromNamespace modifyList write.csv
#' @useDynLib MERIDIAN, .registration = TRUE
"_PACKAGE"

# Resolve files bundled in inst/ both after installation and under devtools::load_all().
app_sys <- function(...) {
  system.file(..., package = "MERIDIAN")
}

utils::globalVariables(c(
  "Across_Env_Mean", "Adjusted_Mean", "BLUE", "BLUP", "CI_lower", "CI_upper",
  "Column", "Component", "Correlation", "Cumulative", "EI", "ENV", "Env1",
  "Env2", "Environment", "Fitted", "GEN", "Genotype", "IPCA1", "IPCA2",
  "Latitude", "Longitude", "MEAN", "MEGA_ENV", "N_Missing", "PC1", "PC2",
  "Pct", "Pct_SS", "Percent", "REP", "Raw", "Residual", "Row", "SE",
  "Source", "TRAIT", "Value", "Variance", "WAAS", "Weight", "avg_rep",
  "env_effect", "env_mean", "fitted_additive", "gen_effect", "gen_mean",
  "hover_text", "intercept", "ipca", "is_outlier", "label", "mean_val",
  "n_rep", "point_size", "slope", "type", "value", "x", "x0", "x1", "y",
  "trait_value", "count"
))
