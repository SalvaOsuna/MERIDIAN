// =============================================================================
// MERIDIAN — Fast Computational Kernels (C++)
// Rcpp functions for performance-critical MET analysis
// =============================================================================

#include <Rcpp.h>
#include <vector>
#include <unordered_map>
#include <cmath>
#include <string>

using namespace Rcpp;

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
