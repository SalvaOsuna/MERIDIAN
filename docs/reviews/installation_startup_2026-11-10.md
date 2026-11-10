# Installation and startup validation — 2026-11-10

## Reproduced failure

On this Ubuntu machine (R 4.5.2), `pak::pkg_install("SalvaOsuna/MERIDIAN")`
stopped while building `fs` because the libuv development files were missing.
The initial dependency plan also identified missing CMake, font development
libraries and X11 headers. RStudio already bundles the Pandoc executable needed
for reports. A failed dependency build leaves MERIDIAN uninstalled, so its
launcher cannot run. Retrying the GitHub installation after supplying the build
dependencies succeeded.

The local checkout also contained a Windows object file and DLL. Loading the
development package on Linux initially failed when the linker reused that
object. The generated artifacts were set aside and the package was recompiled.
They are ignored by Git and excluded from the source archive. The README now
explains both Linux prerequisites and cleaning a checkout copied between systems.

## Repository integration and fixes

The local branch had one unpublished commit, additional working changes, and
eight upstream commits. The integration preserves the local statistical,
state-management, import and export fixes alongside upstream NASA POWER,
phenology and spatial features. Runtime packages are declared in Imports so
normal GitHub installation includes them.

The bundled example retains its precomputed covariates and does not request
weather data. Failed weather retrieval preserves uploaded environmental data.
Google Fonts no longer need downloading by the R server at startup. Tests also
identified and fixed a calendar-date shift from local time to UTC and an error
while searching RStudio directories without Pandoc.

## Validation

- Source archive built and installed successfully as MERIDIAN 0.1.2.
- Archive inspected: no local credentials, histories, private libraries,
  validation output or compiled binaries.
- 47 test blocks, 209 passed expectations, zero failures, zero errors and zero
  skipped tests. The tests emitted 17 plotting warnings about deprecated scale
  arguments, missing observations and Plotly trace styling; these did not fail
  the tests.
- `R CMD check --no-manual`: **Status: OK**, zero errors, warnings and notes.
  Optional packages `ggpubr`, `ragg`, `heatmaply`, `thematic`, `usethis` and
  `devtools` were unavailable; the check used `_R_CHECK_FORCE_SUGGESTS_=false`.
- Installed app verified in a real browser: welcome dialog, example import
  (300 observations, 20 genotypes, five environments), and a completed GrainYield
  ANOVA with 298 observed values and rendered results.
- CI now includes a required-dependencies-only installation and HTTP startup
  check, including the bundled stylesheet and logo.

Local logs and test results remain under the ignored `validation-output/`
directory. The actual NASA weather service was not needed for these offline
startup checks.
