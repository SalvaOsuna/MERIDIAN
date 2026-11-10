# Technical and statistical review of MERIDIAN 0.1.0

Date: September 7, 2026. Repository: Shiny_MET. Reference commit:
`0cc39d0c7999be241dbebbeb1b41ee5a711dbca4`, including the local working state
and untracked files. This is a historical review of that version; the findings
below are not a description of the current corrected implementation.

**Assessment:** the package provides a useful structure for exploring
multi-environment trials, but needs corrections before its results support
varietal selection or publishable inference. Reproducible errors affect
estimation, experimental-design specification, incomplete data and the link
between results and their source data. A plausible-looking table or plot does
not reveal these problems.

The review did not modify production code. Added files were audit artifacts.
Pre-existing changes to `.Rbuildignore`, `.gitignore` and backups were preserved;
builds ran in temporary copies.

## Scope and evidence

The review inspected ANOVA, BLUEs/BLUPs, heritability, AMMI, GGE,
Eberhart–Russell, Wricke, Shukla, adaptation, environmental covariates, SpATS,
validation, Shiny state, reports, dependencies, documentation and tests, together
with C++ kernels and backup files.

Controlled examples ran on Windows with R 4.4.1, `lme4` 1.1-38, `emmeans` 2.0.1,
`metan` 1.19.0 and `SpATS` 1.0-19. Formulas and results were compared with the
packages' own documentation and source. This was not an exhaustive literature
review or a multi-platform validation campaign. Synthetic examples demonstrate
specific failures; their numerical differences do not estimate error magnitude
in a user's real trial.

| Check | Result |
|---|---|
| Install local source, including backups | Fails because C++ symbols are defined twice. |
| Build and install a copy without `safeBackup` | Builds and installs from source. |
| Full check of the copy without backups | **0 errors, 3 warnings, 1 note**; installation, loading, compiled code and tests pass. See `clean_00check.log`. |
| Existing `test-app.R` tests | Three blocks, five expectations; all pass during development loading. `testthat.R` also passes in R CMD check after fresh compilation. |
| Additional statistical tests | Reproduce BLUP, H², block, BLUE, AMMI, Eberhart–Russell and imputation errors. |
| Shiny server tests | Reproduce stale results after changing data and incorrect spatial labels. |
| Parsing | All 36 R files, including backups, parse in the local environment. |

Development loading in the Shiny tests deliberately reused the existing DLL;
this does not certify that DLL as a fresh build. Source installation was checked
separately. Statistical tests load canonical files and exercise R paths when no
native library is loaded.

## Priority findings

P1 means correct before using the affected workflow for decisions or
distribution. P2 means correct in the next iteration because functionality,
clarity or reproducibility is affected. Method limitations are distinguished
from implementation errors. File locations refer to the reviewed snapshot.

### 01 — P1. Local backups prevent package compilation

**Location:** `src/RcppExports-salvador-usask-safeBackup-0001.cpp`,
`src/RcppExports.cpp` and `.Rbuildignore`.

Both C++ files declare the same nine exported functions and `R_init_MERIDIAN`.
Installing a copy of the local sources fails at linking with `multiple
definition`. `.Rbuildignore` does not exclude these backups. Backup R files can
also enter the namespace and make behavior depend on load order, including
functions that exist only in a backup. Package directory and build rules are
documented in [Writing R Extensions](https://stat.ethz.ch/R-manual/R-devel/doc/manual/R-exts.html).

**Scope:** confirmed for the local working tree. The backups are untracked; this
failure is not automatically attributed to the Git commit or GitHub installation.
A copy excluding them compiles and installs.

**Correction:** move backups outside package directories, exclude them from
builds and version control, and regenerate Rcpp bindings from one source.
Ignoring `.o` and `.dll` binaries alone is insufficient.

**Evidence:** `install_2026-09-07.txt`; case 14 in `evidence_2026-09-07.txt`.

### 02 — P1. BLUPs are centered on the first environment

**Location:** `R/models.R:287–300`, especially
`grand_mean <- lme4::fixef(model)[1]`.

Environment is fixed. Under R's default contrasts, the intercept represents the
reference environment. Adding it to genotype effects and calling it the grand
mean makes absolute predictions depend on factor coding.

**Reproduction:** changing only the reference level from E1 to E5 shifts BLUPs by
about **36.170 units**, leaving genotype effects unchanged. The observed mean is
30.573, whereas the published BLUP mean with E1 as reference is 13.317. Ranking
is preserved in this example, but absolute values and comparisons with thresholds
or means from other analyses are incorrect.

Intervals use only `ranef(..., condVar = TRUE)`. That conditional variance does
not by itself include all uncertainty in a marginal prediction combining
estimated fixed and random effects. Restrict its interpretation or recalculate
it; see [lme4 ranef documentation](https://lme4.github.io/lme4/reference/ranef.html).

**Correction:** define the prediction target and environment weights, calculate
the corresponding fixed mean and add the genotype effect. Report the random
effect, prediction on the trait scale and appropriate uncertainty separately.
Add a reference-level invariance test.

### 03 — P1. H² depends on replicate numbering

**Location:** `R/models.R:170–178`.

`n_distinct(rep)` is interpreted as replicates per environment. Unique labels
for each environment/replicate combination instead count blocks across the MET.

**Reproduction:** the same five-environment, three-replicate trial gives
**H² = 0.832** with reused labels 1–3 and **H² = 0.858** with unique labels.
Estimated variances are identical; the error is in the subsequent denominator.
The balanced entry-mean formula is also applied without checking trait balance
or estimability. `accuracy = sqrt(H2)` inherits those limitations and is not a
demonstrated individual BLUP reliability.

**Correction:** count replication within environment and check balance before
using the simplified formula. For unbalanced trials, implement a measure based
on prediction errors or contrasts, state its definition and avoid presenting one
approximation as universal.

### 04 — P1. Incomplete blocks are not respected by every model

**Location:** `R/models.R:50–57`, `R/models.R:122–141`,
`R/mod_anova.R:303–319`.

ANOVA uses `env:block`, although block numbering can restart in each replicate,
requiring `env:rep:block`. Mixed-model, BLUE and BLUP functions do not even
receive the block column or selected design.

**Reproduction:** changing only block labels to be unique within environment
changes residual mean square from **40.146 to 3.611** and residual degrees of
freedom from 100 to 80, using the same plots and observations.

Selecting Alpha-Lattice therefore does not ensure design-correct variance,
adjusted means or uncertainty. Augmented-design inference is likewise not solved
by changing only the ANOVA formula.

**Correction:** use a shared design specification for every model, explicitly
nesting blocks within replicate and environment. Test reused and unique labels.

### 05 — P1. Sequential ANOVA can attribute environmental imbalance to genotype

**Location:** `R/models.R:50–71`.

`anova(aov(...))` uses sequential sums of squares. Genotype precedes environment
and design factors, so it can absorb environmental differences associated with
unequal observation coverage. The table does not explain order dependence or
offer contrasts adjusted to the inference target.

**Reproduction:** data generated without a genotype effect give
**P = 1.18 × 10⁻⁴⁸**, SS = 7144.324, when genotype enters first. With the same
model space and environment first, **P = 0.282**, SS = 11.795.

**Correction:** define the hypothesis, fixed/random factors and target population
of environments. Provide adjusted tests or estimable marginal contrasts; a
mechanical “type III” label does not specify contrasts and interactions.
Sequential ANOVA can remain an explicit option for appropriate settings.

### 06 — P1. The BLUE model omits G×E

**Location:** `R/models.R:216–231`.

The formula is `trait ~ gen + (1|env) + (1|env:rep)`. Genotype means over random
environments with genuine interaction require a genotype × environment
component. Replicates within a G×E cell do not replace independent environments
when estimating genotype precision.

**Reproduction:** in a G×E example, median standard error of genotype contrasts
is **1.594** under the implemented formula and **3.419** with `env:gen`.

**Correction:** include G×E and design terms when appropriate, or state and
justify the additive restriction. Treat the fixed/random status of environments
separately from the decision to make genotype fixed.

### 07 — P1. Eberhart–Russell S²di does not subtract experimental error

**Location:** `R/stability.R:257–305`; `src/fast_pruning.cpp:369`.

The implementation calculates `SSE/(E−2)` from cell means without subtracting
experimental error on the means scale, `MSE/r`, for the corresponding balanced
design. Replication information is discarded before calculation. The C++ kernel
has the same omission.

**Reproduction:** G01 returns **10.539**, compared with approximately **9.35** in
`metan::ge_reg`; G02 returns **1.057**, whereas the corrected estimator is about
**−0.129**. Regression coefficients agree. This is a difference in statistical
definition, not acceleration.

**Correction:** retain experimental error and replication, apply the correction
and document negative estimates. Otherwise call the output regression residual
variance of means, rather than Eberhart–Russell S²di. See
[ge_reg documentation and source](https://tiagoolivoto.github.io/metan/reference/ge_reg.html).

### 08 — P1. “Additive” imputation uses marginal means biased by missing cells

**Location:** `R/stability.R:112–137`; `src/fast_pruning.cpp:295–305`.

`genotype_mean + environment_mean − grand_mean`, calculated directly from
observed values, does not fit an additive model to an incomplete table. Marginal
means can represent different sets of environments and genotypes.

**Reproduction:** an exactly additive table with genotype effects 10/20/30 and
environment effects 0/100/200 should impute **210** for A in E3. After removing
that cell, the package imputes **176.25**, introducing artificial non-additivity
before GGE.

**Correction:** fit an estimable additive model, check connectivity and disclose
the method, imputed cells and fraction of predicted information. Extensive
missingness may require mixed models or validated imputation; reporting the
number of cells alone does not validate predictions.

### 09 — P1. AMMI fails when one G×E cell is absent

**Location:** `R/ammi.R:68–82`; `R/mod_stability.R:382–389`.

The widened matrix can contain NA and is passed directly to `svd`. Although the
module prepares balanced data, it calls native AMMI with the original dataset.

**Reproduction:** removing all three G01/E1 replicates produces `infinite or
missing values in 'x'`, even with complete replication in every remaining cell.

**Correction:** define an explicit missing-cell policy: reject with a useful
diagnostic, use a disclosed subset or apply a validated procedure. Indices
calculated on different subsets should not appear comparable without disclosure.

### 10 — P1. AMMI ANOVA ignores replicates within environment

**Location:** `R/ammi.R:166–174`.

Although `rep` is supplied, the formula is `TRAIT ~ GEN + ENV + GEN:ENV`.
Between-replicate variation enters the residual, despite presenting the table as
formal ANOVA of replicated data.

**Reproduction:** with replicate differences, AMMI residual mean square is
**48.849**, versus **0.995** in RCBD ANOVA modeling `env:rep` on the same data.
F statistics and probabilities change. SVD of means and ANOVA inference are
separate issues.

**Correction:** include appropriate design terms and retain plot-error
estimation. Document the scales of interaction and axis sums of squares when
shown beside ANOVA.

### 11 — P1. Balancing discards excessive information without an exclusion audit

**Location:** `R/stability.R:18–71`; `src/fast_pruning.cpp:87–107`.

The algorithm removes incomplete rows and columns simultaneously against the
same previous matrix. It can remove genotypes whose missingness would disappear
after removing only an environment. It first drops cells whose observation count
differs from the mode, potentially discarding augmented-design checks.

**Reproduction:** it retains **72 of 162 records**. Removing only the problematic
environment would retain **144**, including all 12 genotypes. No persistent
trait-specific exclusion table identifies plots, genotypes and environments.

**Correction:** define a defensible subset objective and selection rule, report
every exclusion and check minimum dimensions afterward. For commonly unbalanced
METs, prefer models supporting imbalance instead of destructive default pruning.

### 12 — P1. Results and report items can survive a dataset change

**Location:** `R/report_registry.R:13–25`, `R/app_server.R:69–81`,
`R/mod_anova.R:288`.

The signature includes dimensions, column names and traits, but omits values,
roles, design and covariates. Registry clearing relies on this signature.
`eventReactive` results also remain valid until the calculation button is pressed
again, even after data change.

**Reproduction:** adding 1000 to every trait value leaves the signature
unchanged. In `shiny::testServer`, replacing data whose mean changes from
**4.4986 to 104.4986** retains the previous result, **4.4986**.

Registry builders consult current reactive results instead of immutable analysis
snapshots; recalculating a trait can retroactively change a previously saved item.

**Correction:** version datasets and analysis runs. Invalidate or mark results
and downloads stale when data or specifications change. Store compact immutable
results or references to versioned runs, including design, roles and covariates
in analytical identity.

### 13 — P1. Spatial predictions can acquire the wrong environment label

**Location:** `R/mod_spatial.R:524–536`, `R/mod_spatial.R:990–1002`.

`adjusted_means()` combines a button-fitted stored model with the current
`input$env`. Spatial plot builders can also retrieve a later model with labels
captured earlier.

**Reproduction:** fit ENV1, then select ENV2 without refitting. The **same
predictions** are returned with `Environment = ENV2`. Confirmed with a real SpATS
fit to the example data and a Shiny server test.

**Correction:** store environment, trait, design and dataset version inside the
fit result and derive labels from it. Disable exports or mark results pending
when specifications change.

### 14 — P2. BLUEs fail with asymptotic emmeans intervals

**Location:** `R/models.R:238–243`.

The code assumes interval columns `lower.CL` and `upper.CL`. With infinite
degrees of freedom, `emmeans` returns `asymp.LCL` and `asymp.UCL`, so rounding
NULL fails.

**Reproduction:** `emmeans::emm_options(lmer.df = "asymptotic")` followed by
`compute_blues()` produces `non-numeric argument to mathematical function`.
Asymptotic mode may also follow size limits or missing degrees-of-freedom
dependencies, as described in [emmeans documentation](https://rvlenth.github.io/emmeans/articles/models.html).

**Correction:** normalize interval extraction for both cases, record the
inferential method and test finite and asymptotic paths explicitly.

### 15 — P2. Reports can omit selected items

**Location:** `inst/report_template.Rmd:49–53`,
`inst/report_template.Rmd:91–118`, `R/mod_reports.R:1035–1036`.

The template selects items by label patterns. The registry produces `ANOVA table
- ...`, whereas the template case-sensitively matches `^ANOVA Table`. A composed
figure is supplied as `Composed figure`, but no template block prints it. There
is no general loop ensuring every selected plot, including spatial outputs, is
included.

**Correction:** render selected identifiers using structured section/type
metadata; do not use display labels as control keys. Check correspondence
between requested and rendered items.

**Evidence limit:** producer/consumer inspection and label-matching tests
demonstrate the incompatibility. This review did not visually certify a complete
PDF/HTML report.

### 16 — P2. Methods claim analyses were run without results

**Location:** `R/report.R:5–31`.

`build_methods_text()` receives results but does not use them to select methods.
It always claims ANOVA, REML, BLUEs, BLUPs and stability analyses were performed,
and lists every selected trait although modules calculate one trait at a time.

**Reproduction:** calling it without results returns that full list of completed
analyses, potentially contradicting the report tables.

**Correction:** derive methods from identified successful runs, including
formulas, data used, exclusions, imputation, parameters, versions and actual
traits. Do not claim figure resolution without checking export parameters.

### 17 — P2. Validation accepts missing identifiers and infinite measurements

**Location:** `R/utils.R:252–326`, `R/mod_data_upload.R:439–499` and consumers
such as `R/mod_anova.R:289`.

Validation checks numeric traits, but not finiteness. It accepts NA or empty
genotype, environment and replicate identifiers. Dimensions are counted before
trait filtering and can include NA as another level. Main modules check for
data and a trait, but not `validation$valid`. Duplicates are reported yet can
enter fitting as extra observations.

**Reproduction:** data containing NA in all three keys and an `Inf` trait value
return `valid = TRUE`.

**Correction:** distinguish data integrity, design validity and analysis-specific
requirements. Block missing keys, non-finite values and incompatible mappings;
check experimental-unit duplicates, connectivity and usable dimensions after
filtering. Provide explicit resolution of true duplicates versus subsamples.

## Additional methodological limitations and improvements

These require product or scientific-target decisions; they are not all
demonstrated numerical errors.

| Area | Observation and proposed action |
|---|---|
| Across-environment spatial summaries | `R/mod_spatial.R:641–654` averages independent predictions and their SEs. `Mean_SE` is a mean of standard errors, not the SE of `Across_Env_Mean`. This alone is not two-stage MET analysis. Label it descriptive or model the second stage with precision/covariance and environmental structure. Averaging already-shrunken BLUPs also needs a justified target. |
| G×E covariance | Independent G×E and a single residual variance are initial assumptions, not universal descriptions of trials with heterogeneous precision or environmental correlations. Prioritize diagnostics and structure comparisons. |
| Mega-environments | `run_mega_envs()` groups environments by the highest observed genotype mean and discards incomplete environments. Label it exploratory winner grouping; it does not establish repeatable mega-environments across years or validate GGE sectors. |
| Selection objective | BLUEs/BLUPs are ranked descending and winners use `which.max`. Add trait-specific direction or an explicit objective; disease, lodging and earliness may require other rules. |
| PCA and covariates | `compute_env_pca()` uses all numeric columns, potentially including numeric environment identifiers, without filtering zero variance. Validate covariate joins, unique environment rows, identifiers and constants. |
| Optional-C++ claim | The README calls C++ optional, but `NeedsCompilation: yes`, `useDynLib` and wrappers require it for source installation. `run_finlay_wilkinson()` also calls the kernel without fallback, and `exists()` does not establish native-symbol availability. Align behavior and documentation. Case 12 reproduces failure without loaded kernels. |
| Public interface | `NAMESPACE` exports only `run_app`, consistent with a packaged app. The analyses are not yet a stable public scripting API. Decide whether to distribute an analytical library too. |
| Performance | ANOVA fits the same mixed model separately for variances and BLUPs; stability reconstructs previously calculated matrices and metrics. Reuse run-specific results after fixing data identity. Large-scale timings were not measured. |
| Tests | Existing tests check one exported function, UI construction and three files. They do not test scientific results, design or invariance. No versioned CI configuration was found under `.github`. |

## Recommended correction order

1. **Integrity and distribution:** exclude backups, stabilize installation and
   implement data/run identity to prevent stale results and incorrect labels.
2. **Shared statistical specification:** define fixed/random environments,
   target population, design, blocks and G×E for ANOVA, BLUEs, BLUPs and H².
3. **Explicit imbalance policy:** resolve AMMI/GGE, connectivity and imputation;
   replace silent exclusions with verifiable diagnostics.
4. **Indices and uncertainty:** correct Eberhart–Russell, predictions and
   intervals; compare R/C++ with references and algebraically known cases.
5. **Reports:** export exactly the selected items and derive methods from runs
   that actually occurred.
6. **Ongoing validation:** run statistical and Shiny interaction tests in CI,
   then evaluate scale and extend mixed-model structures.

## Minimum regression coverage

- Prediction and H² invariance to level reordering, replicate renaming and reused
  or unique block identifiers.
- Simulated RCBD and incomplete-block trials with known variances; explicit
  augmented-design and insufficient-replication validation.
- BLUEs/BLUPs and contrasts with strong G×E, imbalance and heterogeneous
  precision; finite and asymptotic intervals.
- AMMI on additive and known-rank matrices; missing cells, empty rows/columns
  and replicate variation.
- Eberhart–Russell under different plot errors with constant means, plus R/C++
  numerical equivalence.
- Data, trait or environment changes after fitting; results and downloads must
  invalidate or preserve their original identity.
- Every selected plot/table appears in reports, and methods describe only
  completed analyses.

## Reproducible files

Files are alongside this report. Run scripts from the repository root. They are
audit tools, not an integrated `testthat` regression suite.

| File | Content |
|---|---|
| `reproduce_review_2026-09-07.R` | Fifteen diagnostic cases and session versions. |
| `evidence_2026-09-07.txt` | Numerical results and observed errors. |
| `additional_checks_2026-09-07.R` | AMMI ANOVA, BLUE precision, sequential SS and report labels. |
| `additional_evidence_2026-09-07.txt` | Additional evidence. |
| `check_package_2026-09-07.R` | Local installation attempt, existing tests and Shiny server tests in a temporary copy. |
| `install_2026-09-07.txt` | Actual duplicate-symbol linker failure. |
| `package_tests_2026-09-07.txt` | UI tests and stale-state/spatial-label reproductions. |
| `check_clean_source_2026-09-07.R` | Build and check source without backups in a temporary copy. |
| `clean_build_2026-09-07.txt`, `clean_00install.out`, `clean_00check.log` | Build, installation and R CMD check logs. |

## Final R CMD check result for the reviewed snapshot

The backup-free copy built, compiled, installed and checked with **0 errors,
3 warnings and 1 note**. Included tests passed. Warnings were:

1. `DESCRIPTION` specifies `MIT + file LICENSE`, but only `LICENSE.md` exists.
2. A multiplication-symbol string in `R/mod_eda.R` did not translate under the
   ASCII locale used for checking. This is observed portability behavior, not a
   universal syntax error.
3. Documentation exists for five functions absent from canonical code:
   `calculate_enviromic_indices`, `fetch_nasa_weather`, `parse_date_robustly`,
   `process_environmental_covariates` and `validate_environmental_data`. A palette
   default also differs between documentation and code. The five environmental
   help files are untracked locally; some functionality exists in backups and
   needs reconciliation before regenerating documentation.

The note reports missing suggested dependencies `kableExtra`, `openxlsx` and
`shinytest2`. No extra packages were installed in the user's library, so Excel
workflows and `shinytest2` browser tests were not certified.

This copy excludes `safeBackup` files and previous binaries. It does not modify
the working tree or certify the complete local distribution. Suggested-package
requirements were relaxed; manuals and vignettes were not built. A valid locale
was set only for checking processes after inherited `C.UTF-8` caused startup
errors on Windows.
