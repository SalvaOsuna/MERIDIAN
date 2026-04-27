# =============================================================================
# Generate example_phenotypic.csv
# Synthetic MET dataset: 20 genotypes × 5 environments × 3 reps (RCBD)
# Run this script once to create the example dataset
# =============================================================================

set.seed(42)

genotypes    <- paste0("G", sprintf("%02d", 1:20))
environments <- paste0("ENV", 1:5)
reps         <- 1:3

# Base genotype effects (some high, some low yielding)
gen_effects <- c(
  G01 = 0.8, G02 = 0.5, G03 = -0.2, G04 = 0.3, G05 = -0.5,
  G06 = 0.1, G07 = -0.3, G08 = 0.6, G09 = -0.1, G10 = 0.4,
  G11 = -0.4, G12 = 0.2, G13 = 0.7, G14 = -0.6, G15 = 0.9,
  G16 = -0.2, G17 = 0.3, G18 = -0.1, G19 = 0.5, G20 = -0.3
)

# Environment effects
env_effects <- c(ENV1 = 0.5, ENV2 = -0.3, ENV3 = 0.2, ENV4 = 0.8, ENV5 = -0.5)

# G×E interaction matrix (crossover pattern: G01 best in ENV1-2, G15 best in ENV4-5)
ge_interaction <- matrix(rnorm(20 * 5, mean = 0, sd = 0.3), nrow = 20, ncol = 5)
rownames(ge_interaction) <- genotypes
colnames(ge_interaction) <- environments

# Create crossover G×E
ge_interaction["G01", c("ENV1", "ENV2")] <- c(0.5, 0.4)
ge_interaction["G01", c("ENV4", "ENV5")] <- c(-0.4, -0.3)
ge_interaction["G15", c("ENV1", "ENV2")] <- c(-0.3, -0.2)
ge_interaction["G15", c("ENV4", "ENV5")] <- c(0.6, 0.5)
ge_interaction["G13", c("ENV3")]         <- 0.5
ge_interaction["G08", c("ENV2", "ENV3")] <- c(0.4, 0.3)

# Build dataset
data_rows <- list()
row_idx <- 1

for (env in environments) {
  for (gen in genotypes) {
    for (rep in reps) {
      grand_mean_gy  <- 3.8
      grand_mean_ph  <- 85
      grand_mean_dtf <- 60
      grand_mean_tkw <- 38

      # GrainYield (t/ha)
      gy <- grand_mean_gy + gen_effects[gen] + env_effects[env] +
        ge_interaction[gen, env] + rnorm(1, 0, 0.25)
      gy <- max(gy, 0.5)  # floor

      # PlantHeight (cm) — correlated with yield environment
      ph <- grand_mean_ph + gen_effects[gen] * 8 + env_effects[env] * 5 +
        rnorm(1, 0, 3)

      # DaysToFlowering — less GxE, more genotype-driven
      dtf <- grand_mean_dtf + gen_effects[gen] * (-3) + env_effects[env] * 2 +
        rnorm(1, 0, 1.5)

      # TKW (thousand kernel weight, g)
      tkw <- grand_mean_tkw + gen_effects[gen] * 3 + env_effects[env] * 2 +
        ge_interaction[gen, env] * 1.5 + rnorm(1, 0, 1.2)

      data_rows[[row_idx]] <- data.frame(
        Genotype        = gen,
        Environment     = env,
        Rep             = rep,
        Block           = paste0("B", rep),
        GrainYield      = gy,
        PlantHeight     = ph,
        DaysToFlowering = dtf,
        TKW             = tkw,
        stringsAsFactors = FALSE
      )
      row_idx <- row_idx + 1
    }
  }
}

df <- do.call(rbind, data_rows)

# Assign spatial coordinates (Row and Col) and add a spatial gradient
# For each environment, assign a 10 rows x 6 cols grid.
# Rep 1: Cols 1-2; Rep 2: Cols 3-4; Rep 3: Cols 5-6
df <- do.call(rbind, lapply(split(df, df$Environment), function(env_df) {
  env_df <- env_df[order(env_df$Rep), ]
  
  # For each rep (20 plots), assign 10 rows x 2 cols
  coords <- do.call(rbind, lapply(1:3, function(r) {
    data.frame(
      Row = rep(1:10, times = 2),
      Col = rep(c(r*2 - 1, r*2), each = 10)
    )
  }))
  
  # Randomize plots within each rep slightly to mix genotypes, but we keep the Rep structure
  env_df$Row <- coords$Row
  env_df$Col <- coords$Col
  
  # Add spatial gradient: row trend + column trend
  spatial_trend <- 0.1 * env_df$Row + 0.15 * env_df$Col + 0.05 * env_df$Row * env_df$Col
  env_df$GrainYield <- env_df$GrainYield + spatial_trend * 0.2
  env_df$PlantHeight <- env_df$PlantHeight + spatial_trend * 0.5
  
  # Round values after adding spatial trend
  env_df$GrainYield <- round(env_df$GrainYield, 2)
  env_df$PlantHeight <- round(env_df$PlantHeight, 1)
  env_df$DaysToFlowering <- round(env_df$DaysToFlowering, 0)
  env_df$TKW <- round(env_df$TKW, 1)
  
  env_df
}))

# Reorder columns
df <- df[, c("Genotype", "Environment", "Rep", "Block", "Row", "Col", "GrainYield", "PlantHeight", "DaysToFlowering", "TKW")]

# Introduce ~2% missing values (6 cells across traits)
set.seed(123)
miss_indices <- sample(1:nrow(df), 6)
df$GrainYield[miss_indices[1:2]]      <- NA
df$PlantHeight[miss_indices[3:4]]     <- NA
df$DaysToFlowering[miss_indices[5]]   <- NA
df$TKW[miss_indices[6]]              <- NA

# Write CSV
write.csv(df, file.path("data", "example_phenotypic.csv"), row.names = FALSE)
cat("Generated example_phenotypic.csv:", nrow(df), "rows\n")
