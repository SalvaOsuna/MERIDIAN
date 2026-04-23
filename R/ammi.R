#' Run AMMI Analysis natively (no metan dependency)
#'
#' @param data     data.frame en formato largo
#' @param gen      string: nombre de la columna de genotipo
#' @param env      string: nombre de la columna de ambiente
#' @param rep      string: nombre de la columna de repetición (o NULL si no hay)
#' @param trait    string: nombre de la columna del rasgo fenotípico
#' @param n_axis   integer: número de ejes IPCA a retener (default = 2)
#'
#' @return lista con todos los objetos necesarios para plotear y reportar

run_ammi <- function(data, gen, env, rep = NULL, trait, n_axis = 2) {
  
  # ── 0. Preparar y validar datos ──────────────────────────────────────────
  df <- data %>%
    rename(
      GEN   = all_of(gen),
      ENV   = all_of(env),
      TRAIT = all_of(trait)
    ) %>%
    filter(!is.na(TRAIT)) %>%
    mutate(
      GEN = as.factor(GEN),
      ENV = as.factor(ENV)
    )
  
  if (!is.null(rep)) {
    df <- df %>% rename(REP = all_of(rep)) %>% mutate(REP = as.factor(REP))
  }
  
  n_gen <- nlevels(df$GEN)
  n_env <- nlevels(df$ENV)
  
  # Limitar ejes al máximo posible
  n_axis <- min(n_axis, min(n_gen, n_env) - 1)
  
  # ── 1. Medias por celda GEN×ENV ───────────────────────────────────────────
  # Si hay repeticiones, promediar primero para obtener una observación por celda
  cell_means <- df %>%
    group_by(GEN, ENV) %>%
    summarise(mean_val = mean(TRAIT, na.rm = TRUE), .groups = "drop")
  
  # ── 2. ANOVA aditivo para obtener efectos principales ────────────────────
  # Medias marginales
  grand_mean <- mean(cell_means$mean_val, na.rm = TRUE)
  
  gen_means <- cell_means %>%
    group_by(GEN) %>%
    summarise(gen_mean = mean(mean_val, na.rm = TRUE)) %>%
    mutate(gen_effect = gen_mean - grand_mean)  # α_i
  
  env_means <- cell_means %>%
    group_by(ENV) %>%
    summarise(env_mean = mean(mean_val, na.rm = TRUE)) %>%
    mutate(env_effect = env_mean - grand_mean)  # β_j
  
  # ── 3. Matriz de interacción G×E ─────────────────────────────────────────
  # e_ij = Y_ij - μ - α_i - β_j
  # donde Y_ij = media de celda, μ = gran media, α_i = efecto genotipo, β_j = efecto ambiente
  
  interaction_long <- cell_means %>%
    left_join(gen_means %>% select(GEN, gen_effect), by = "GEN") %>%
    left_join(env_means %>% select(ENV, env_effect), by = "ENV") %>%
    mutate(
      fitted_additive = grand_mean + gen_effect + env_effect,
      interaction     = mean_val - fitted_additive
    )
  
  # Convertir a matriz (filas = genotipos, columnas = ambientes)
  interaction_matrix <- interaction_long %>%
    select(GEN, ENV, interaction) %>%
    pivot_wider(names_from = ENV, values_from = interaction) %>%
    column_to_rownames("GEN") %>%
    as.matrix()
  
  # ── 4. SVD de la matriz de interacción ───────────────────────────────────
  # interaction_matrix = U %*% diag(D) %*% t(V)
  # U = eigenvectores de genotipos (left singular vectors)
  # V = eigenvectores de ambientes (right singular vectors)
  # D = valores singulares (singular values)
  
  svd_result <- svd(interaction_matrix)
  
  singular_values <- svd_result$d
  U <- svd_result$u  # dimensiones: n_gen × n_axis
  V <- svd_result$v  # dimensiones: n_env × n_axis
  
  rownames(U) <- rownames(interaction_matrix)
  rownames(V) <- colnames(interaction_matrix)
  
  # ── 5. Scores de genotipos y ambientes ───────────────────────────────────
  # Convención "simétrica": multiplicar por raíz del valor singular
  # Esto da la misma escala a genotipos y ambientes en el biplot
  
  gen_scores <- as.data.frame(U[, 1:n_axis, drop = FALSE] %*% 
                                diag(sqrt(singular_values[1:n_axis]), 
                                     nrow = n_axis))
  colnames(gen_scores) <- paste0("IPCA", 1:n_axis)
  gen_scores$GEN <- rownames(U)
  
  env_scores <- as.data.frame(V[, 1:n_axis, drop = FALSE] %*% 
                                diag(sqrt(singular_values[1:n_axis]), 
                                     nrow = n_axis))
  colnames(env_scores) <- paste0("IPCA", 1:n_axis)
  env_scores$ENV <- rownames(V)
  
  # Unir con medias marginales
  gen_scores <- gen_scores %>%
    left_join(gen_means, by = "GEN")
  
  env_scores <- env_scores %>%
    left_join(env_means, by = "ENV")
  
  # ── 6. Varianza explicada por cada eje ───────────────────────────────────
  total_ss    <- sum(singular_values^2)
  axis_ss     <- singular_values^2
  axis_pct    <- round(100 * axis_ss / total_ss, 2)
  axis_cum    <- cumsum(axis_pct)
  
  variance_explained <- data.frame(
    Axis        = paste0("IPCA", seq_along(singular_values)),
    SS          = axis_ss,
    Percent     = axis_pct,
    Cumulative  = axis_cum
  )
  
  # ── 7. Métricas de estabilidad ────────────────────────────────────────────
  
  # ASV — AMMI Stability Value (Purchase et al. 2000)
  # ASV = sqrt[ (SS_IPCA1/SS_IPCA2 * IPCA1)^2 + IPCA2^2 ]
  # Requiere al menos 2 ejes
  
  stability <- gen_scores %>% select(GEN, gen_mean, starts_with("IPCA"))
  
  if (n_axis >= 2) {
    ss_ratio <- axis_ss[1] / axis_ss[2]
    stability <- stability %>%
      mutate(
        ASV = sqrt((ss_ratio * IPCA1)^2 + IPCA2^2)
      )
  }
  
  # WAAS — Weighted Average of Absolute Scores (Olivoto et al. 2019)
  # WAAS = sum(|IPCA_k| * SS_k%) / sum(SS_k%)  para k ejes retenidos
  ipca_cols   <- paste0("IPCA", 1:n_axis)
  weights     <- axis_pct[1:n_axis]
  
  stability <- stability %>%
    rowwise() %>%
    mutate(
      WAAS = sum(abs(c_across(all_of(ipca_cols))) * weights) / sum(weights)
    ) %>%
    ungroup()
  
  # SIPC — Sums of IPCA scores (Sneller et al.)
  stability <- stability %>%
    rowwise() %>%
    mutate(
      SIPC = sum(abs(c_across(all_of(ipca_cols))))
    ) %>%
    ungroup() %>%
    arrange(WAAS)  # ordenar por estabilidad (menor WAAS = más estable)
  
  # ── 8. ANOVA tabla formal ─────────────────────────────────────────────────
  # Para el ANOVA completo (con reps), usar lm si hay repeticiones
  
  if (!is.null(rep)) {
    formula_full <- as.formula("TRAIT ~ GEN + ENV + GEN:ENV")
    model_full   <- lm(formula_full, data = df)
    anova_table  <- as.data.frame(anova(model_full))
    anova_table$Source <- rownames(anova_table)
    anova_table  <- anova_table %>% select(Source, everything())
    rownames(anova_table) <- NULL
  } else {
    anova_table <- NULL  # sin reps no hay estimación del error
  }
  
  # ── 9. Output final ───────────────────────────────────────────────────────
  list(
    # Datos base
    grand_mean         = grand_mean,
    gen_means          = gen_means,       # data.frame: GEN, gen_mean, gen_effect
    env_means          = env_means,       # data.frame: ENV, env_mean, env_effect
    cell_means         = cell_means,      # data.frame: GEN, ENV, mean_val
    interaction_long   = interaction_long,# data.frame: GEN, ENV, interaction, fitted_additive
    interaction_matrix = interaction_matrix, # matrix: G×E
    
    # SVD
    singular_values    = singular_values,
    U                  = U,
    V                  = V,
    
    # Scores listos para ggplot
    gen_scores         = gen_scores,      # data.frame: GEN, IPCA1, IPCA2..., gen_mean, gen_effect
    env_scores         = env_scores,      # data.frame: ENV, IPCA1, IPCA2..., env_mean, env_effect
    
    # Varianza
    variance_explained = variance_explained, # data.frame: Axis, SS, Percent, Cumulative
    
    # Estabilidad
    stability          = stability,       # data.frame: GEN, gen_mean, IPCA1..., ASV, WAAS, SIPC
    
    # ANOVA
    anova_table        = anova_table,
    
    # Parámetros usados
    params = list(
      gen = gen, env = env, rep = rep, trait = trait,
      n_axis = n_axis, n_gen = n_gen, n_env = n_env
    )
  )
}