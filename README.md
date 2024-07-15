<!-- README.md is generated from README.Rmd. Please edit that file -->

# {cases} R package: Stratified Evaluation of Subgroup Classification Accuracy

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![](https://img.shields.io/badge/devel%20version-0.1.8-blue.svg)](https://github.com/maxwestphal/cases)
[![](https://www.r-pkg.org/badges/version/cases?color=orange)](https://cran.r-project.org/package=cases)
[![R build
status](https://github.com/maxwestphal/cases/workflows/R-CMD-check/badge.svg)](https://github.com/maxwestphal/cases/actions)
[![](https://codecov.io/gh/maxwestphal/cases/branch/main/graph/badge.svg)](https://codecov.io/gh/maxwestphal/cases)
[![](https://img.shields.io/badge/paper-SMMR-gold.svg)](https://journals.sagepub.com/doi/full/10.1177/09622802241236933)
[![](https://img.shields.io/badge/preprint-arXiv-gold.svg)](https://arxiv.org/abs/2105.13469)
<!-- badges: end -->

**{cases}** is an R package to simultaneously assess classification
accuracy of multiple classifiers in several subgroups (strata). For
instance, it allows to asses the accuracy of multiple candidate (index)
diagnostic tests which is often measured with

-   sensitivity (accuracy in the diseased subgroup) and
-   specificity (accuracy in the healthy subgroup).

A widespread goal in diagnostic accuracy studies a so-called
**co-primary** analysis of these two endpoints, i.e. to show a
significant benefit (compared to some benchmark) in sensitivity **and**
specificity for **at least one** of the candidate classifiers. The
package implements different methods for multiplicity adjustment for
that purpose (e.g. Bonferroni, maxT, pairs bootstrap). Theoretical
background and an extensive simulation study is explained in the paper
by Westphal & Zapf (2024).

------------------------------------------------------------------------

## Installation

To install the latest stable version from
[CRAN](https://cran.r-project.org/), use the following R command:

    install.packages("cases")

You can install the latest development version from
[GitHub](https://github.com/) with:

    # install.packages("remotes")
    remotes::install_github("maxwestphal/cases",
      ref = "development",
      build_vignettes = TRUE
    )

------------------------------------------------------------------------

## Usage

A vignette which explains the basic functionality of the {cases} package
can be displayed as follows:

    vignette(topic = "package_overview", package = "cases")

The following vignette shows an exemplary usage of the package in the
context of biomarker assessment and prediction model evaluation:

    vignette(topic = "example_wdbc", package = "cases")

------------------------------------------------------------------------

## References

1.  Westphal M, Zapf A. Statistical inference for diagnostic test
    accuracy studies with multiple comparisons. Statistical Methods in
    Medical Research. 2024;0(0).
    [doi:10.1177/09622802241236933](https://journals.sagepub.com/doi/full/10.1177/09622802241236933)
