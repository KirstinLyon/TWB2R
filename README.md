
<!-- README.md is generated from README.Rmd. Please edit that file -->

# TWB2R

<!-- badges: start -->
<!-- badges: end -->

The goal of TWB2R is to …

## Installation

You can install the development version of TWB2R from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("KirstinLyon/TWB2R")
```

## Import Library

``` r
library(TWB2R)
library(tidyverse)
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ dplyr     1.1.4     ✔ readr     2.1.4
#> ✔ forcats   1.0.0     ✔ stringr   1.5.1
#> ✔ ggplot2   3.4.4     ✔ tibble    3.2.1
#> ✔ lubridate 1.9.3     ✔ tidyr     1.3.0
#> ✔ purrr     1.0.2     
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
library(xml2)
```

## Example

Read in a Tableau workbook as XML and view metadata.

``` r
#Read in tableau twb files  (NB:  unpackage a twbx file to get a twb)
twb_file <- read_xml("inst/extdata/Data/tableau_example_workbook.twb")

#Extract the meta-data for fields using TWB2R
all_datasources <- TWB2R::all_datasources(twb_file) #All datasources connected to the workbook
all_windows <- TWB2R::all_windows(twb_file) #all worksheets, dashboards and storyboards in the workbook
Raw <- TWB2R::all_raw_fields(twb_file) #all fields from the datasets
calcs <- TWB2R::all_other_created(twb_file) #all calculations created in the workbook
param <- TWB2R::all_parameters(twb_file) #all parameters created in the workbook
```
