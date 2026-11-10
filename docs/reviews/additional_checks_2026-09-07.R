options(width = 120, warn = 1)
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(tibble))
for (f in c("utils.R", "models.R", "ammi.R")) source(file.path("R", f))
out <- file("docs/reviews/additional_evidence_2026-09-07.txt", "wt")
sink(out, split = TRUE)
set.seed(614)
d <- expand.grid(gen = factor(paste0("G", 1:10)), env = factor(paste0("E", 1:5)), rep = factor(1:4))
d$y <- 20 + rnorm(10, 0, 2)[d$gen] + c(-10, -5, 0, 5, 10)[d$env] +
  rnorm(50, 0, 5)[as.integer(interaction(d$gen, d$env))] +
  rnorm(20, 0, 7)[as.integer(interaction(d$env, d$rep))] + rnorm(nrow(d))
cat("### AMMI ANOVA ignores replicates within environment\n")
a <- run_ammi(d, "gen", "env", "rep", "y")
ref <- run_anova_met(d, "gen", "env", "rep", trait = "y")
cat("AMMI sources:", paste(a$anova_table$Source, collapse = ", "), "\n")
cat("AMMI residual MS:", tail(a$anova_table$`Mean Sq`, 1),
    "RCBD residual MS:", tail(ref$anova_table$MeanSq, 1), "\n")

cat("\n### BLUE model lacks GxE; precision of genotype contrasts\n")
ctrl <- lme4::lmerControl(optimizer = "nloptwrap", calc.derivs = FALSE)
current <- lme4::lmer(y ~ gen + (1|env) + (1|env:rep), d, control = ctrl)
with_ge <- lme4::lmer(y ~ gen + (1|env) + (1|env:gen) + (1|env:rep), d, control = ctrl)
c1 <- as.data.frame(emmeans::contrast(emmeans::emmeans(current, "gen", lmer.df = "asymptotic"), "pairwise"))
c2 <- as.data.frame(emmeans::contrast(emmeans::emmeans(with_ge, "gen", lmer.df = "asymptotic"), "pairwise"))
cat("Median contrast SE, implemented formula:", median(c1$SE), "with GxE:", median(c2$SE), "\n")

cat("\n### ANOVA sequential SS absorbs environment imbalance in genotype term\n")
set.seed(315)
u <- d
u$y <- 50 + c(-20, -10, 0, 10, 20)[u$env] + rnorm(nrow(u))
keep <- (as.integer(u$gen) <= 5 & as.integer(u$env) <= 2) |
  (as.integer(u$gen) > 5 & as.integer(u$env) >= 4) | u$rep == "1"
u <- u[keep, ]
f1 <- run_anova_met(u, "gen", "env", "rep", trait = "y")
f2 <- anova(lm(y ~ env + gen + env:rep + gen:env, u))
cat("Same fixed model space, genotype entered first SS:", f1$anova_table$SumSq[1],
    "genotype after environment SS:", f2["gen", "Sum Sq"], "\n")
cat("Implemented genotype P:", f1$anova_table$P_value[1],
    "genotype after environment P:", f2["gen", "Pr(>F)"], "\n")

cat("\n### Report template fails to match actual registry labels\n")
cat("ANOVA table - GrainYield matched by ^ANOVA Table:", grepl("^ANOVA Table", "ANOVA table - GrainYield"), "\n")
cat("Selected custom/composed plots are rendered only when their labels match template patterns.\n")
sink()
close(out)
