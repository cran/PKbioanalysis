
# PKbioanalysis <img src="man/figures/logo.png" align="right" alt="" width="120" />

<!-- badges: start -->
  [![R-CMD-check](https://github.com/OmarAshkar/PKbioanalysis/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/OmarAshkar/PKbioanalysis/actions/workflows/R-CMD-check.yaml)

 <!-- badges: end -->

[![CRAN
version](http://www.r-pkg.org/badges/version/PKbioanalysis)](https://cran.r-project.org/package=PKbioanalysis)
[![CRAN total
downloads](https://cranlogs.r-pkg.org/badges/grand-total/PKbioanalysis)](https://cran.r-project.org/package=PKbioanalysis)
[![CRAN monthly
downloads](https://cranlogs.r-pkg.org/badges/PKbioanalysis)](https://cran.r-project.org/package=PKbioanalysis)

PKbioanalysis is an R package designed to facilitate the integration of pharmacokinetic (PK) and bioanalytical data analysis. The overarching goal of this package is to provide a streamlined and standardized approach to chromatography-based pharmacokinetic studies starting from study design to PK analysis and reporting.

## Installation
You can install the released version of PKbioanalysis from CRAN with:

```R
install.packages("PKbioanalysis")
```
Or the development version from GitHub with:

```R
remotes::install_github("OmarAshkar/PKbioanalysis")
```


## Main Features

- 96-well plate design and visualization 
- Support Pharmacokinetic samples attributes on the plate
- Automatically propagate and design injection sequences
- Export injection sequences to CSV compatible with Masslynx and MassHunter
- Interactive dilution scheme with unit conversion

## Milestones
- Support import peak areas from TargetLynx or other formats
- Support interactive chromatography integration and visualization 
- Linearity 
- Suitability 
- MLE of additive and proportional errors 
- Automatically estimate LOD
- Estimate UAD (dilution threshold) recommendation
- Export to NONMEM-formated dataset
