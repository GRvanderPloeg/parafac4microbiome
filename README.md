
<!-- README.md is generated from README.Rmd. Please edit that file -->

# parafac4microbiome

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/GRvanderPloeg/parafac4microbiome/branch/master/graph/badge.svg)](https://app.codecov.io/gh/GRvanderPloeg/parafac4microbiome?branch=master)
[![R-CMD-check](https://github.com/GRvanderPloeg/parafac4microbiome/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/GRvanderPloeg/parafac4microbiome/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Overview

The parafac4microbiome package enables R users with an easy way to
create Parallel Factor Analysis (PARAFAC) models for longitudinal
microbiome data.

- `processDataCube()` can be used to process the microbiome count data
  appropriately for a multi-way data array.
- `parafac()` allows the user to create a Parallel Factor Analysis model
  of the multi-way data array.
- `assessNumComponents()` helps the user select the appropriate number
  of components for the PARAFAC model.
- `modelStabilityCheck()` performs jack-knifing of samples to inspect
  the stability of the PARAFAC model.
- `plotPARAFACmodel()` helps visually inspect the PARAFAC model.

This package also comes with three example datasets: \* `Fujita2023`: an
in-vitro experiment of ocean inocula on peptide medium, sampled every
day for 110 days (<https://doi.org/10.1186/s40168-023-01474-5>). \*
`Shao2019`: a large cohort dataset of vaginally and caesarean-section
born infants from London
(<https://www.nature.com/articles/s41586-019-1560-1>). \*
`vanderPloeg2024`: a small gingivitis intervention dataset with specific
response groups (manuscript in preparation).

A basic introduction to the package is given in
`vignette("PARAFAC_introduction")` and modelling the example datasets
are elaborated in their respective vignettes
`vignette("Fujita2023_analysis")`, `vignette("Shao2019_analysis")` and
`vignette("vanderPloeg2024_analysis")`.

## Installation

You can install the development version of parafac4microbiome from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("GRvanderPloeg/parafac4microbiome")
```

## Citation

Please use the following citation when using this package:

- van der Ploeg, G. R., et al. (2024). Multi-way modelling of oral
  microbial dynamics and host-microbiome interactions during induced
  gingivitis. \[Manuscript in preparation\].

## Usage

``` r
library(parafac4microbiome)
library(multiway)
#> Loading required package: CMLS
#> Loading required package: quadprog
#> Loading required package: parallel
#> 
#> Attaching package: 'multiway'
#> The following objects are masked from 'package:parafac4microbiome':
#> 
#>     corcondia, parafac
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(ggplot2)
library(ggpubr)
set.seed(0) # for reproducibility

# Process the data cube
processedFujita = processDataCube(Fujita2023, sparsityThreshold=0.99, centerMode=1, scaleMode=2)

# Make a PARAFAC model
model = parafac(processedFujita$data, nfac=3, verbose=FALSE)

# Plot the PARAFAC model using some metadata
plotPARAFACmodel(model, processedFujita,
  colourCols = c("", "Genus", ""),
  legendTitles = c("", "Genus", ""),
  xLabels = c("Replicate", "Feature index", "Time point"),
  legendColNums = c(0,5,0),
  arrangeModes = c(FALSE, TRUE, FALSE),
  continuousModes = c(FALSE,FALSE,TRUE),
  overallTitle = "Fujita PARAFAC model")
```

<img src="man/figures/README-usage-1.png" width="100%" />