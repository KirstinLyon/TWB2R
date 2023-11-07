# TWB2R
Package for converting Tableau workbook meta-data. 


Example code:
# install package
install.packages("remotes")
remotes::install_github("kirstinlyon/TWB2R")

# libraries
library(glamr)
library(tidyverse)
library(TWB2R)
library(xml2)


#folder structure
folder_setup(
    folder_list = list("Data", "Images", "Scripts", "AI", "Dataout", "GIS", "Documents",
                       "Graphics", "markdown", "Tableau")
)

#Read in tableau twb files  (NB:  unpackage a twbx file to get a twb)
twb_file <- read_xml("Data/DISA Dashboard.twb")

#Extract the meta-data for fields using TWB2R
all_datasources <- TWB2R::all_datasources(twb_file)
all_windows <- TWB2R::all_windows(twb_file)
Raw <- TWB2R::all_raw_fields(twb_file)
calcs <- TWB2R::all_other_created(twb_file)
param <- TWB2R::all_parameters(twb_file)
