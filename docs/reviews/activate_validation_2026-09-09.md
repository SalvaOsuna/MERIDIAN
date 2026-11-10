# ACTIVATE lentil data: bug fixes and validation

Tested `ACTIVATE_lentils_myYraw_rep_gen.csv`: 8,648 rows, 104 genotypes and four
environments. Mapping: genotype `Lentil`, environment `ENV`, replicate `Rep_gen`,
coordinates `Row`/`Col`. The optional block field was left unmapped for the model
checks; these tests verify software behavior under that specification, not the
experimental randomization or the suitability of a particular field model.

The last nine columns were used exactly as requested: `VegP`, `DTM`, `RepP`,
`lodging`, `YLD`, `YLD_kgha`, `PRT`, `CPY_kgha`, `DS`.

## Corrected failures

1. **AMMI interaction heatmap:** its ordering code referred to `gen_effect` and
   `env_effect` in a table that no longer contained them. It now takes ordering
   from the corresponding effect tables and preserves all interaction values.
2. **Winner tooltip:** a group containing just one environment serializes its
   text differently in Plotly. `%{text}` was displayed literally. Tooltips now
   contain explicit environment, winner and mean values, including ties.
3. **Exploratory heatmap export:** registration tried to convert a data frame
   directly to a table, disconnecting the Shiny session. The builder now
   explicitly converts it to a matrix before reshaping, and factor levels preserve
   the requested clustered order in the exported figure.
4. **Spreadsheet errors:** 12 `#VALUE!` cells in `YLD_kgha` and 14 in `CPY_kgha`
   made these otherwise numeric columns import as text. Recognized spreadsheet
   errors now become NA only in otherwise numeric columns. Validation reports
   their counts; original row, column and token are retained as import metadata.
   Arbitrary text is not silently discarded. The source CSV was not modified.
5. **Dense labels:** large static heatmaps and the AMMI interactive heatmap use
   selected genotype axis labels to avoid overlap. Every genotype cell remains
   present; interactive hover data retain genotype identity. Large heatmaps omit
   overlapping cell-value annotations and identify the label thinning in a caption.

## Data coverage and numerical checks

| Trait | Observed | Missing | Genotype predictions |
|---|---:|---:|---:|
| VegP | 8,637 | 11 | 104 |
| DTM | 6,540 | 2,108 | 104 |
| RepP | 6,540 | 2,108 | 104 |
| lodging | 8,637 | 11 | 104 |
| YLD | 8,636 | 12 | 104 |
| YLD_kgha | 8,636 | 12 | 104 |
| PRT | 8,634 | 14 | 104 |
| CPY_kgha | 8,634 | 14 | 104 |
| DS | 4,920 | 3,728 | 104 |

All nine traits completed ANOVA, mixed models, BLUEs, BLUPs, AMMI, GGE, environment
correlations, means heatmaps, interaction heatmaps, observed-winner grouping and
reaction norms. Each trait has finite estimates, SEs and interval bounds for all
104 genotype predictions. Observed winning means agree with an independent
aggregation of the imported data. None of these mixed fits was singular.

`DTM` and `RepP` have only 63 measurements across 42 genotypes in Clavet2025.
`DS` has observations for only 30 genotypes in each 2024 environment. The app
continues to disclose AMMI/GGE imputations and complete-cell subset exclusions;
the fixes do not convert those missing measurements into observed values.

## Browser and export checks

The browser tests used the actual CSV and all nine traits. They checked actual
rendered plot traces and tooltip text, including winner groups of length one.
The means heatmap, observed-winner plot and interaction heatmap were registered
and downloaded in PNG, SVG and PDF. Adaptation tables were downloaded in CSV
and Excel. A focused final run checks label spacing on `DS` and regenerates
these exported files.

Artifact inspection passed: all three PNGs are 4,200 × 3,000 pixels, all three
PDFs are readable one-page figures, all three SVGs are valid SVG documents, and
the Excel workbook is readable. The exported adaptation table contains all four
environments, including tied winners and genotype-coverage counts. Final static
and browser heatmaps were visually inspected with readable axis labels.

Regression tests cover spreadsheet-error handling, preservation of arbitrary
text and the original input file, AMMI effect-table ordering, singleton winner
tooltips, heatmap registration with missing cells, and retention of all 416
cells in a 104-genotype × four-environment heatmap.

## Reproduce

From the project root, pass the dataset path to:

- `tools/validate_activate.R <dataset.csv> final models`
- `tools/verify_activate_results.R <dataset.csv>`
- `tools/browser_activate.cjs <dataset.csv> final` (local validation app on port 3877)
- `tools/validate_app.R` and `tools/check_package.R`

Evidence stays in the ignored `validation-output/activate/` directory: coverage,
model summaries, import-error locations, saved model results, browser logs,
screenshots and actual downloads. No raw dataset is added to package tests.
`validation-output/latest-check.txt` identifies the latest completed package check.

Final result: **R CMD check — Status: OK, zero errors, warnings and notes**.
The final check directory is `validation-output/check-73b077245ea1/MERIDIAN.Rcheck`.
All real-data numerical checks, browser checks and downloaded-artifact checks
described above completed successfully.
