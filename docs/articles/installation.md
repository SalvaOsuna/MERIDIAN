# Installation on Windows, macOS and Linux

MERIDIAN requires **R 4.3.0 or later**. RStudio is recommended. The
package is installed from GitHub as source code, so its C++ code needs
the appropriate compiler even when dependencies are available as
binaries.

## Windows

1.  Install [R for
    Windows](https://cran.r-project.org/bin/windows/base/).
2.  Install RStudio and [Rtools matching your R
    release](https://cran.r-project.org/bin/windows/Rtools/). Use the
    default Rtools installation location, then restart RStudio.
3.  Run the R installation commands below.

Rtools includes the C/C++ tools needed to compile MERIDIAN. Available
CRAN Windows binaries can be used for dependencies. If R cannot find the
compiler, check the installation with:

``` r

install.packages("pkgbuild")
pkgbuild::has_build_tools(debug = TRUE)
```

## macOS

Install the [CRAN version of R](https://cran.r-project.org/bin/macosx/)
and RStudio for your processor architecture: Apple Silicon or Intel.
Install Apple’s Command Line Tools from Terminal:

``` sh
xcode-select --install
```

Then run the R installation commands below. Use available CRAN binaries
for dependencies. If a dependency needs compilation with Fortran, use
the compiler matching the installed R build from the [official R for
macOS tools page](https://mac.r-project.org/tools/). Avoid mixing Intel
and Apple Silicon libraries in one R installation.

## Linux

On Ubuntu or Debian, run these commands in a terminal before installing
the R package:

``` sh
sudo apt-get update
sudo apt-get install -y build-essential r-base-dev cmake libuv1-dev \
  libcurl4-openssl-dev libssl-dev libxml2-dev libfontconfig-dev libfreetype-dev \
  libharfbuzz-dev libfribidi-dev libpng-dev libjpeg-dev libtiff-dev libwebp-dev libx11-dev
```

For other distributions or current dependency requirements, inspect the
plan in R:

``` r

pak::pkg_sysreqs("SalvaOsuna/MERIDIAN")
```

If `fs` fails with `libuv was not found`, install the libuv development
package (`libuv1-dev` on Ubuntu/Debian) before retrying. A dependency
build failure means MERIDIAN may not yet be installed.

## Install and launch on any supported system

Run in the R console:

``` r

install.packages("pak")
pak::pak("SalvaOsuna/MERIDIAN")
MERIDIAN::run_app()
```

To update, restart R first, then run:

``` r

pak::pkg_install("SalvaOsuna/MERIDIAN", upgrade = TRUE)
MERIDIAN::run_app()
```

The bundled example works offline. A browser can use a system font when
Google Fonts is unavailable. NASA POWER weather retrieval requires
internet access; uploaded environmental covariates remain available if
retrieval fails.

## Reports and optional tools

- **HTML reports:** Pandoc, provided with RStudio or installed
  separately.
- **PDF reports:** Pandoc, the R package `pagedown`, and Chrome,
  Chromium or Edge. A LaTeX installation is not needed for this export
  route.
- **Tables and individual figures:** CSV/Excel and PNG/SVG/TIFF/PDF
  exports are available from the Reports module. Individual PDF figures
  do not need the browser-based report exporter.

``` r

install.packages("pagedown")
pagedown::find_chrome()
```

If Pandoc is not detected, run the app from RStudio or put a standalone
Pandoc installation on the system PATH. RStudio’s direct and
architecture-specific bundled locations are checked by the report
exporter.

## Local development

Install development dependencies and load the source project:

``` r

install.packages("devtools")
devtools::install_deps(dependencies = TRUE)
source("run_app_dev.R")
```

After copying a checkout between operating systems, remove old compiled
files before loading it:

``` r

pkgbuild::clean_dll(".")
source("run_app_dev.R")
```

The automated workflow builds, tests and starts the installed app on
Linux, Windows and macOS. The local checkout can only exercise the
system on which it is running; consult [GitHub
Actions](https://github.com/SalvaOsuna/MERIDIAN/actions) for the latest
results on the other systems.
