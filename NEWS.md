# MERIDIAN 0.1.2

* Integrate the local statistical and export fixes with NASA POWER weather retrieval, phenology-aware covariates and the latest spatial controls.
* Declare runtime dependencies for clean GitHub installations and document Linux build requirements, including libuv for fs.
* Keep the bundled example usable offline, retain uploaded environmental data when weather retrieval fails, and avoid downloading fonts on the R server during startup.
* Preserve calendar dates in the input time zone and detect RStudio's nested Linux Pandoc installation.
* Exclude local credentials, histories and validation artifacts from source packages.

# MERIDIAN 0.1.1

* Repair the AMMI interaction heatmap and singleton winner tooltips. Recognized spreadsheet error cells in otherwise numeric columns are imported as missing with an explicit validation notice.
* Accelerate large fixed-effect analyses with equivalent environment-wise QR fits; reuse these blocks for plot-error estimation. Cache two recent analyses per module/session and change selection direction without refitting.
* Repair package compilation, dependency declarations and generated documentation.
* Replace order-dependent ANOVA with estimable marginal tests; share design preparation across ANOVA, BLUEs and BLUPs, including blocks within replicate and environment.
* Correct BLUP centering, prediction uncertainty, generalized heritability and asymptotic BLUE intervals. Add higher/lower selection objectives and design diagnostics.
* Correct additive imputation and Eberhart-Russell error adjustment; disclose excluded records and imputed cells. Preserve replicated checks and unequal replication in complete-cell stability subsets.
* Invalidate results on changes to data or model settings; freeze registered outputs. Correct spatial environment labels and descriptive across-environment SEs.
* Repair current-setting downloads, one-row Excel output, composition layouts/tags, panel ZIPs, and complete HTML/PDF reports. Preserve small p values in report tables and detect RStudio's Pandoc outside RStudio.
* Apply graphical controls to saved plots without mutating registered originals; export environmental PCA, correlation and adaptation tables.
* Make outlier exclusion reversible and limited to the chosen trait.
* Add statistical, Shiny, export and native-fallback regression tests, real-browser validation scripts and a package-check workflow.
