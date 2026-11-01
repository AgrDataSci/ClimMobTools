
ClimMobTools
============

<!-- badges: start -->
[![CRAN](https://www.r-pkg.org/badges/version/ClimMobTools)](https://cran.r-project.org/package=ClimMobTools)
[![CRAN checks](https://badges.cranchecks.info/worst/ClimMobTools.svg)](https://cran.r-project.org/web/checks/check_results_ClimMobTools.html)
[![Downloads](https://cranlogs.r-pkg.org/badges/ClimMobTools)](https://cran.r-project.org/package=ClimMobTools)
[![R-CMD-check](https://github.com/AgrDataSci/ClimMobTools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/AgrDataSci/ClimMobTools/actions/workflows/R-CMD-check.yaml)
[![ProjStatus](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/AgrDataSci/ClimMobTools/blob/master/LICENSE.md)
<!-- badges: end -->

*ClimMobTools*: API Client for the 'ClimMob' platform in R <img align="right" src="man/figures/logo.png">
=========================================================================================================

## Overview

**ClimMobTools** is the API client for the 'ClimMob' platform in R. [ClimMob](https://climmob.net/) is an open source software for decentralized large-N trials with the 'tricot' approach. Developed by [van Etten et al. (2019)](https://doi.org/10.1017/S0014479716000739) 'tricot' turns the research paradigm on its head; instead of a few researchers designing complicated trials to compare several technologies in search of the best solutions, it enables many participants to carry out reasonably simple experiments that taken together can offer even more information.

## What it does

* Download project data from 'ClimMob' with `getDataCM()`, list projects with `getProjectsCM()`, check data collection with `getProjectProgress()` and retrieve questionnaires with `getSurvey()`.
* Export tidy tables for analysis with the `export*()` functions.
* Design new trials with `randomise()`, which builds sequentially balanced incomplete blocks so the design stays balanced even when only part of the packages are distributed or returned.
* Displace participants' coordinates with `rmGeoIdentity()` before sharing data.

Keep your API key out of scripts. Store it in the environment variable `CLIMMOB_KEY` (for example in `~/.Renviron`) and read it with `Sys.getenv("CLIMMOB_KEY")`. Rankings are analysed with [gosset](https://agrdatasci.github.io/gosset/) and [PlackettLuce](https://CRAN.R-project.org/package=PlackettLuce).

## Package website

<https://agrdatasci.github.io/ClimMobTools/>

## Installation

The package may be installed from CRAN via

``` r
install.packages("ClimMobTools")
```

The development version can be installed via

``` r
library("remotes")
install_github("agrdatasci/ClimMobTools", build_vignettes = TRUE)

```


## Going further

The full functionality of **ClimMobTools** is illustrated in the package vignette. The vignette can be found on the [package website](https://agrdatasci.github.io/ClimMobTools/) or from within `R` once the package has been installed, e.g. via

``` r
vignette("Overview", package = "ClimMobTools")
```

## Meta

  - Please [report any issues or bugs](https://github.com/agrdatasci/ClimMobTools/issues).

  - License: MIT.

  - Get citation information for *ClimMobTools* in R by typing `citation(package = "ClimMobTools")`.

  - You are welcome to contribute to the *ClimMobTools* project. Please read our [contribution guidelines](CONTRIBUTING.md).

  - Please note that the *ClimMobTools* project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in the *ClimMobTools* project you agree to abide by its terms.
