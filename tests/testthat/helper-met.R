example_bundle <- function() {
  d <- read.csv(app_sys("extdata/example_phenotypic.csv"))
  list(data = d, gen_col = "Genotype", env_col = "Environment", rep_col = "Rep", block_col = "Block",
       row_col = "Row", col_col = "Col", traits = c("GrainYield", "PlantHeight"),
       design = list(design = "RCBD"), augmented_checks = character(),
       env_data = read.csv(app_sys("extdata/example_envdata.csv")))
}

balanced_fixture <- function() {
  set.seed(612)
  d <- expand.grid(gen = sprintf("G%02d", 1:8), env = paste0("E", 1:4), rep = 1:3)
  d$y <- 30 + rnorm(8, 0, 2)[d$gen] + c(-12, -5, 3, 20)[d$env] +
    rnorm(32, 0, 2)[as.integer(interaction(d$gen, d$env))] +
    rnorm(12, 0, 1)[as.integer(interaction(d$env, d$rep))] + rnorm(nrow(d), 0, 2)
  d
}
