# grepreaper <img src="man/figures/logo.png" align="right" height="139" />

[![R-CMD-check](https://github.com/akshat09867/grepreaper/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/akshat09867/grepreaper/actions/workflows/R-CMD-check.yaml)
The goal of `grepreaper` is to provide a fast, R-friendly interface to the system 'grep' utility. It allows you to read, filter, and aggregate data from thousands of files simultaneously without loading them all into memory by leveraging command-line tools before data enters the R environment.

## Installation

You can install the development version of grepreaper from [GitHub](https://github.com/akshat09867/grepreaper) with:

```r
# install.packages("devtools")
devtools::install_github("akshat09867/grepreaper")