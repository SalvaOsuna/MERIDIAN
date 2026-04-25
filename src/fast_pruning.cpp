// =============================================================================
// MERIDIAN — Fast Computational Kernels (C++)
// Rcpp functions for performance-critical MET analysis
// =============================================================================

#include <Rcpp.h>
#include <vector>
#include <unordered_map>
#include <cmath>
#include <string>
#include <algorithm>
#include <limits>

using namespace Rcpp;

inline bool is_missing_double(double x) {
    return NumericVector::is_na(x) || std::isnan(x);
}

inline double safe_mean(double sum, int n) {
    return n > 0 ? sum / (double)n : NA_REAL;
}

double quantile_type7(std::vector<double> vals, double prob) {
    vals.erase(
        std::remove_if(vals.begin(), vals.end(), is_missing_double),
        vals.end()
    );
    int n = vals.size();
    if (n == 0) return NA_REAL;
    std::sort(vals.begin(), vals.end());
    if (n == 1) return vals[0];
    double h = 1.0 + (n - 1.0) * prob;
    int hf = (int)std::floor(h);
    double frac = h - hf;
    double lower = vals[hf - 1];
    double upper = vals[std::min(hf, n - 1)];
    return lower + frac * (upper - lower);
}

// =============================================================================
// 1. Fast Balanced Subset Pruning
// =============================================================================

// [[Rcpp::export]]
LogicalVector cpp_find_balanced_subset(IntegerVector gen, IntegerVector env, int n_gen_levels, int n_env_levels) {
    int n = gen.length();
    LogicalVector keep_row(n, true);
    
    std::vector<bool> keep_gen(n_gen_levels + 1, true);
    std::vector<bool> keep_env(n_env_levels + 1, true);
    
    bool changed = true;
    while (changed) {
        changed = false;
        
        std::vector<int> gen_env_counts(n_gen_levels + 1, 0);
        std::vector<int> env_gen_counts(n_env_levels + 1, 0);
        std::vector<std::vector<bool>> matrix(n_gen_levels + 1, std::vector<bool>(n_env_levels + 1, false));
        
        int current_active_envs = 0;
        for (int e = 1; e <= n_env_levels; ++e) {
            if (keep_env[e]) current_active_envs++;
        }
        
        int current_active_gens = 0;
        for (int g = 1; g <= n_gen_levels; ++g) {
            if (keep_gen[g]) current_active_gens++;
        }
        
        // Populate presence/absence matrix
        for (int i = 0; i < n; ++i) {
            if (keep_row[i]) {
                int g = gen[i];
                int e = env[i];
                if (g != NA_INTEGER && e != NA_INTEGER) {
                    if (!matrix[g][e]) {
                        matrix[g][e] = true;
                        gen_env_counts[g]++;
                        env_gen_counts[e]++;
                    }
                } else {
                    keep_row[i] = false;
                }
            }
        }
        
        // Check genotypes
        for (int g = 1; g <= n_gen_levels; ++g) {
            if (keep_gen[g]) {
                if (gen_env_counts[g] < current_active_envs) {
                    keep_gen[g] = false;
                    changed = true;
                }
            }
        }
        
        // Check environments
        for (int e = 1; e <= n_env_levels; ++e) {
            if (keep_env[e]) {
                if (env_gen_counts[e] < current_active_gens) {
                    keep_env[e] = false;
                    changed = true;
                }
            }
        }
        
        // Update row keeping vector if anything changed
        if (changed) {
            for (int i = 0; i < n; ++i) {
                if (keep_row[i]) {
                    int g = gen[i];
                    int e = env[i];
                    if (g == NA_INTEGER || e == NA_INTEGER || !keep_gen[g] || !keep_env[e]) {
                        keep_row[i] = false;
                    }
                }
            }
        }
    }
    
    return keep_row;
}


// =============================================================================
// 2. Fast GxE Cell Means Computation
// =============================================================================

// [[Rcpp::export]]
NumericMatrix cpp_ge_means(IntegerVector gen, IntegerVector env, NumericVector trait,
                           int n_gen, int n_env) {
    // Compute sum and count for each G×E cell, return matrix of means
    int n = gen.length();
    NumericMatrix sums(n_gen, n_env);
    IntegerMatrix counts(n_gen, n_env);
    
    for (int i = 0; i < n; ++i) {
        if (!NumericVector::is_na(trait[i]) && gen[i] != NA_INTEGER && env[i] != NA_INTEGER) {
            int g = gen[i] - 1;  // 0-indexed
            int e = env[i] - 1;
            sums(g, e) += trait[i];
            counts(g, e) += 1;
        }
    }
    
    NumericMatrix means(n_gen, n_env);
    for (int g = 0; g < n_gen; ++g) {
        for (int e = 0; e < n_env; ++e) {
            if (counts(g, e) > 0) {
                means(g, e) = sums(g, e) / counts(g, e);
            } else {
                means(g, e) = NA_REAL;
            }
        }
    }
    return means;
}


// =============================================================================
// 3. Fast AMMI Stability Metrics (ASV, WAAS, SIPC) 
// =============================================================================

// [[Rcpp::export]]
DataFrame cpp_ammi_stability(NumericMatrix gen_ipca_scores,
                              NumericVector axis_ss,
                              NumericVector axis_pct,
                              int n_axis) {
    int n_gen = gen_ipca_scores.nrow();
    
    NumericVector asv(n_gen, NA_REAL);
    NumericVector waas(n_gen, 0.0);
    NumericVector sipc(n_gen, 0.0);
    
    // ASV requires at least 2 axes
    double ss_ratio = 0.0;
    if (n_axis >= 2) {
        ss_ratio = axis_ss[0] / axis_ss[1];
    }
    
    double weight_sum = 0.0;
    for (int k = 0; k < n_axis; ++k) {
        weight_sum += axis_pct[k];
    }
    
    for (int i = 0; i < n_gen; ++i) {
        // ASV
        if (n_axis >= 2) {
            double term1 = ss_ratio * gen_ipca_scores(i, 0);
            double term2 = gen_ipca_scores(i, 1);
            asv[i] = std::sqrt(term1 * term1 + term2 * term2);
        }
        
        // WAAS and SIPC
        double waas_sum = 0.0;
        double sipc_sum = 0.0;
        for (int k = 0; k < n_axis; ++k) {
            double abs_score = std::abs(gen_ipca_scores(i, k));
            waas_sum += abs_score * axis_pct[k];
            sipc_sum += abs_score;
        }
        waas[i] = waas_sum / weight_sum;
        sipc[i] = sipc_sum;
    }
    
    return DataFrame::create(
        Named("ASV") = asv,
        Named("WAAS") = waas,
        Named("SIPC") = sipc
    );
}


// =============================================================================
// 4. Fast Finlay-Wilkinson Regressions
// =============================================================================

// [[Rcpp::export]]
DataFrame cpp_fw_regression(NumericVector mean_vals, NumericVector ei_vals,
                             IntegerVector gen_ids, int n_gen) {
    // Compute OLS regression per genotype: mean_val ~ EI
    int n = mean_vals.length();
    
    NumericVector slopes(n_gen, 0.0);
    NumericVector intercepts(n_gen, 0.0);
    
    // Accumulate sufficient statistics per genotype
    std::vector<double> sum_x(n_gen, 0.0);
    std::vector<double> sum_y(n_gen, 0.0);
    std::vector<double> sum_xy(n_gen, 0.0);
    std::vector<double> sum_x2(n_gen, 0.0);
    std::vector<int> counts(n_gen, 0);
    
    for (int i = 0; i < n; ++i) {
        if (!NumericVector::is_na(mean_vals[i]) && !NumericVector::is_na(ei_vals[i]) &&
            gen_ids[i] != NA_INTEGER) {
            int g = gen_ids[i] - 1;  // 0-indexed
            double x = ei_vals[i];
            double y = mean_vals[i];
            sum_x[g] += x;
            sum_y[g] += y;
            sum_xy[g] += x * y;
            sum_x2[g] += x * x;
            counts[g]++;
        }
    }
    
    for (int g = 0; g < n_gen; ++g) {
        if (counts[g] >= 2) {
            double n_g = (double)counts[g];
            double denom = sum_x2[g] - (sum_x[g] * sum_x[g]) / n_g;
            if (std::abs(denom) > 1e-12) {
                slopes[g] = (sum_xy[g] - (sum_x[g] * sum_y[g]) / n_g) / denom;
            }
            intercepts[g] = (sum_y[g] - slopes[g] * sum_x[g]) / n_g;
        }
    }
    
    return DataFrame::create(
        Named("slope") = slopes,
        Named("intercept") = intercepts
    );
}


// =============================================================================
// 5. Fast Additive Imputation for GxE Matrices
// =============================================================================

// [[Rcpp::export]]
List cpp_impute_additive_gxe(NumericMatrix ge_means) {
    int n_gen = ge_means.nrow();
    int n_env = ge_means.ncol();
    NumericMatrix out = clone(ge_means);

    NumericVector row_sum(n_gen, 0.0), col_sum(n_env, 0.0);
    IntegerVector row_n(n_gen, 0), col_n(n_env, 0);
    double total_sum = 0.0;
    int total_n = 0;

    for (int i = 0; i < n_gen; ++i) {
        for (int j = 0; j < n_env; ++j) {
            double v = ge_means(i, j);
            if (!is_missing_double(v)) {
                row_sum[i] += v;
                col_sum[j] += v;
                row_n[i]++;
                col_n[j]++;
                total_sum += v;
                total_n++;
            }
        }
    }

    double grand_mean = safe_mean(total_sum, total_n);
    NumericVector row_mean(n_gen), col_mean(n_env);
    for (int i = 0; i < n_gen; ++i) row_mean[i] = row_n[i] > 0 ? row_sum[i] / row_n[i] : grand_mean;
    for (int j = 0; j < n_env; ++j) col_mean[j] = col_n[j] > 0 ? col_sum[j] / col_n[j] : grand_mean;

    int n_imputed = 0;
    for (int i = 0; i < n_gen; ++i) {
        for (int j = 0; j < n_env; ++j) {
            if (is_missing_double(out(i, j))) {
                out(i, j) = row_mean[i] + col_mean[j] - grand_mean;
                n_imputed++;
            }
        }
    }

    return List::create(
        Named("matrix") = out,
        Named("n_imputed") = n_imputed
    );
}


// =============================================================================
// 6. Fast Eberhart-Russell Regression Diagnostics
// =============================================================================

// [[Rcpp::export]]
DataFrame cpp_er_statistics(NumericMatrix ge_means, NumericVector env_index) {
    int n_gen = ge_means.nrow();
    int n_env = ge_means.ncol();

    NumericVector slope(n_gen, NA_REAL);
    NumericVector intercept(n_gen, NA_REAL);
    NumericVector s2di(n_gen, NA_REAL);
    NumericVector r2(n_gen, NA_REAL);

    for (int i = 0; i < n_gen; ++i) {
        double sx = 0.0, sy = 0.0, sxy = 0.0, sx2 = 0.0;
        int n = 0;
        for (int j = 0; j < n_env; ++j) {
            double x = env_index[j];
            double y = ge_means(i, j);
            if (!is_missing_double(x) && !is_missing_double(y)) {
                sx += x;
                sy += y;
                sxy += x * y;
                sx2 += x * x;
                n++;
            }
        }
        if (n == 0) continue;

        double denom = sx2 - (sx * sx) / (double)n;
        double b = 0.0;
        if (std::abs(denom) > 1e-12) {
            b = (sxy - (sx * sy) / (double)n) / denom;
        }
        double a = (sy - b * sx) / (double)n;

        double sse = 0.0, sst = 0.0;
        for (int j = 0; j < n_env; ++j) {
            double x = env_index[j];
            double y = ge_means(i, j);
            if (!is_missing_double(x) && !is_missing_double(y)) {
                double yhat = a + b * x;
                double centered = y - a;
                sse += (y - yhat) * (y - yhat);
                sst += centered * centered;
            }
        }

        slope[i] = b;
        intercept[i] = a;
        if (n > 2) s2di[i] = sse / (double)(n - 2);
        if (sst > 1e-12) r2[i] = std::max(0.0, 1.0 - sse / sst);
    }

    return DataFrame::create(
        Named("slope") = slope,
        Named("intercept") = intercept,
        Named("S2di") = s2di,
        Named("R2") = r2
    );
}


// =============================================================================
// 7. Fast Grouped Numeric Summaries
// =============================================================================

// [[Rcpp::export]]
DataFrame cpp_group_numeric_summary(IntegerVector group, NumericVector value, int n_group) {
    NumericVector sum(n_group, 0.0), sumsq(n_group, 0.0), minv(n_group), maxv(n_group);
    IntegerVector count(n_group, 0);
    for (int g = 0; g < n_group; ++g) {
        minv[g] = R_PosInf;
        maxv[g] = R_NegInf;
    }

    int n = value.length();
    for (int i = 0; i < n; ++i) {
        int g = group[i];
        double v = value[i];
        if (g == NA_INTEGER || g < 1 || g > n_group || is_missing_double(v)) continue;
        int idx = g - 1;
        sum[idx] += v;
        sumsq[idx] += v * v;
        count[idx]++;
        if (v < minv[idx]) minv[idx] = v;
        if (v > maxv[idx]) maxv[idx] = v;
    }

    NumericVector mean(n_group, NA_REAL), sd(n_group, NA_REAL), cv(n_group, NA_REAL);
    for (int g = 0; g < n_group; ++g) {
        if (count[g] > 0) {
            mean[g] = sum[g] / count[g];
            minv[g] = minv[g];
            maxv[g] = maxv[g];
            if (count[g] > 1) {
                double var = (sumsq[g] - (sum[g] * sum[g]) / count[g]) / (count[g] - 1);
                sd[g] = std::sqrt(std::max(0.0, var));
            }
            if (!is_missing_double(mean[g]) && std::abs(mean[g]) > 1e-12 && !is_missing_double(sd[g])) {
                cv[g] = 100.0 * sd[g] / mean[g];
            }
        } else {
            minv[g] = NA_REAL;
            maxv[g] = NA_REAL;
        }
    }

    return DataFrame::create(
        Named("GroupId") = seq(1, n_group),
        Named("N") = count,
        Named("Mean") = mean,
        Named("SD") = sd,
        Named("Min") = minv,
        Named("Max") = maxv,
        Named("CV_pct") = cv
    );
}


// =============================================================================
// 8. Fast Grouped Outlier Detection
// =============================================================================

// [[Rcpp::export]]
LogicalVector cpp_detect_outliers(IntegerVector group, NumericVector value,
                                  int n_group, std::string method, double threshold) {
    int n = value.length();
    std::vector<std::vector<double>> vals(n_group);
    for (int i = 0; i < n; ++i) {
        int g = group[i];
        double v = value[i];
        if (g != NA_INTEGER && g >= 1 && g <= n_group && !is_missing_double(v)) {
            vals[g - 1].push_back(v);
        }
    }

    NumericVector lower(n_group, NA_REAL), upper(n_group, NA_REAL), mean(n_group, NA_REAL), sd(n_group, NA_REAL);
    for (int g = 0; g < n_group; ++g) {
        int m = vals[g].size();
        if (m == 0) continue;
        if (method == "iqr") {
            double q1 = quantile_type7(vals[g], 0.25);
            double q3 = quantile_type7(vals[g], 0.75);
            double iqr = q3 - q1;
            lower[g] = q1 - threshold * iqr;
            upper[g] = q3 + threshold * iqr;
        } else {
            double s = 0.0, ss = 0.0;
            for (double v : vals[g]) {
                s += v;
                ss += v * v;
            }
            mean[g] = s / m;
            if (m > 1) {
                double var = (ss - (s * s) / m) / (m - 1);
                sd[g] = std::sqrt(std::max(0.0, var));
            }
        }
    }

    LogicalVector out(n);
    for (int i = 0; i < n; ++i) {
        int g = group[i];
        double v = value[i];
        if (g == NA_INTEGER || g < 1 || g > n_group || is_missing_double(v)) {
            out[i] = NA_LOGICAL;
            continue;
        }
        int idx = g - 1;
        if (method == "iqr") {
            out[i] = (!is_missing_double(lower[idx]) && (v < lower[idx] || v > upper[idx]));
        } else {
            if (is_missing_double(sd[idx]) || sd[idx] <= 1e-12) {
                out[i] = false;
            } else {
                out[i] = std::abs((v - mean[idx]) / sd[idx]) > threshold;
            }
        }
    }
    return out;
}


// =============================================================================
// 9. Fast Spatial Row-Column Cell Aggregation
// =============================================================================

// [[Rcpp::export]]
DataFrame cpp_spatial_cell_summary(IntegerVector cell_id,
                                   NumericVector column,
                                   NumericVector row,
                                   NumericVector raw,
                                   NumericVector fitted,
                                   NumericVector residual,
                                   NumericVector genotype,
                                   NumericVector weight,
                                   int n_cell) {
    NumericVector col_first(n_cell, NA_REAL), row_first(n_cell, NA_REAL);
    NumericVector sum_raw(n_cell, 0.0), sum_fit(n_cell, 0.0), sum_res(n_cell, 0.0), sum_gen(n_cell, 0.0), sum_w(n_cell, 0.0);
    IntegerVector n_raw(n_cell, 0), n_fit(n_cell, 0), n_res(n_cell, 0), n_gen(n_cell, 0), n_w(n_cell, 0);

    int n = cell_id.length();
    for (int i = 0; i < n; ++i) {
        int id = cell_id[i];
        if (id == NA_INTEGER || id < 1 || id > n_cell) continue;
        int idx = id - 1;
        if (is_missing_double(col_first[idx])) col_first[idx] = column[i];
        if (is_missing_double(row_first[idx])) row_first[idx] = row[i];

        if (!is_missing_double(raw[i])) { sum_raw[idx] += raw[i]; n_raw[idx]++; }
        if (!is_missing_double(fitted[i])) { sum_fit[idx] += fitted[i]; n_fit[idx]++; }
        if (!is_missing_double(residual[i])) { sum_res[idx] += residual[i]; n_res[idx]++; }
        if (!is_missing_double(genotype[i])) { sum_gen[idx] += genotype[i]; n_gen[idx]++; }
        if (!is_missing_double(weight[i])) { sum_w[idx] += weight[i]; n_w[idx]++; }
    }

    NumericVector mean_raw(n_cell), mean_fit(n_cell), mean_res(n_cell), mean_gen(n_cell), mean_w(n_cell);
    for (int i = 0; i < n_cell; ++i) {
        mean_raw[i] = safe_mean(sum_raw[i], n_raw[i]);
        mean_fit[i] = safe_mean(sum_fit[i], n_fit[i]);
        mean_res[i] = safe_mean(sum_res[i], n_res[i]);
        mean_gen[i] = safe_mean(sum_gen[i], n_gen[i]);
        mean_w[i] = safe_mean(sum_w[i], n_w[i]);
    }

    return DataFrame::create(
        Named("CellId") = seq(1, n_cell),
        Named("Column") = col_first,
        Named("Row") = row_first,
        Named("Raw") = mean_raw,
        Named("Fitted") = mean_fit,
        Named("Residual") = mean_res,
        Named("Genotype") = mean_gen,
        Named("Weight") = mean_w
    );
}
