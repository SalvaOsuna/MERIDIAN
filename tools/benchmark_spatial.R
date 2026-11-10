Sys.setenv(LC_ALL = "C", LANG = "C")
.libPaths(c(normalizePath(".local-library", mustWork = FALSE), .libPaths()))
pkgload::load_all(".", quiet = TRUE)
set.seed(204)
d <- expand.grid(Genotype = factor(sprintf("G%03d", 1:100)), Environment = factor(paste0("E", 1:4)), Rep = factor(1:20))
d$Yield <- 50 + rnorm(100, 0, 4)[d$Genotype] + rnorm(4, 0, 8)[d$Environment] + rnorm(nrow(d), 0, 3)
d$Row <- d$Col <- NA_integer_
for (environment in levels(d$Environment)) {
  rows <- which(d$Environment == environment)
  positions <- sample(seq_along(rows))
  d$Row[rows] <- (positions-1L) %% 40L + 1L
  d$Col[rows] <- (positions-1L) %/% 40L + 1L
}
db <- list(data=d,gen_col="Genotype",env_col="Environment",rep_col="Rep",block_col=NULL,
  row_col="Row",col_col="Col",traits="Yield",design=list(design="RCBD"),env_data=NULL)
shiny::testServer(mod_spatial_server, args=list(data_result=list(data_bundle=shiny::reactive(db))), {
  summary <- do.call(rbind,lapply(c(TRUE,FALSE),function(random) {
    elapsed <- system.time(invisible(capture.output(model <- fit_spats(d[d$Environment=="E1",],
      "Yield","Row","Col","Genotype", "Rep", character(),20,20,random))))["elapsed"]
    stopifnot(inherits(model,"SpATS"))
    data.frame(Rows=2000,Genotypes=100,Genotype_random=random,Seconds=unname(elapsed))
  }))
  write.csv(summary,"validation-output/performance/spatial-timings.csv",row.names=FALSE)
  print(summary)
})
