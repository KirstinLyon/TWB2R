
<!-- README.md is generated from README.Rmd. Please edit that file -->

# TWB2R

<!-- badges: start -->
<!-- badges: end -->

The TWB2R package streamlines the extraction of metadata from Tableau
Workbook (TWB) files. Different use cases include:

    Ensuring uniformity in calculations across multiple workbooks
    Effortlessly generating a comprehensive data dictionary
    Creating a user-friendly Table of Contents for workbooks
    Verifying calculations for potential errors
    Examining the datasources employed in a workbook

## Preparing a Datasource

The TWB2R package uses a TWB file. If your workbook is saved in the
Tableau Packaged Workbook (TWBX) format, you can easily retrieve the
required TWB file by right-clicking on the TWBX file and selecting
“unpackage” in your file browser. This process grants you direct access
to the underlying TWB file associated with your workbook. This package
does not need the files associated with the TWBX file, only the TWB
file.

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

A TWB file is an XML file. There is no need to convert from a TWB to an
XML in order to use the XML2 package.

Read in a Tableau workbook as XML and view metadata.

``` r
#Read in tableau twb files  (NB:  unpackage a twbx file to get a twb)
twb_file <- read_xml("inst/extdata/Data/tableau_example_workbook.twb")

#Extract the individual meta-data for fields using TWB2R
all_datasources <- TWB2R::show_all_datasources(twb_file) #All datasources connected to the workbook
all_windows <- TWB2R::show_all_windows(twb_file) #all worksheets, dashboards and storyboards in the workbook
Raw <- TWB2R::show_all_raw_fields(twb_file) #all fields from the datasets
calcs <- TWB2R::show_all_other_created(twb_file) #all calculations created in the workbook
param <- TWB2R::show_all_parameters(twb_file) #all parameters created in the workbook


#or extract all meta-data, and then stipulate which data to use
all_meta_data <- TWB2R::extract_all_metadata(twb_file)
raw <- TWB2R::get_metadata(all_meta_data, "raw")
```
