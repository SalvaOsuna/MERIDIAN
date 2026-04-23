#include <Rcpp.h>
#include <vector>

using namespace Rcpp;

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
                // Handle NAs which are represented as NA_INTEGER in Rcpp
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
