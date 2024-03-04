library(tidyverse)
library(rvest)
library(lubridate)
library(nycflights13)
library(xml2)
library(rvest)
library(readxl)
library(dplyr)
library(httr)
library(devtools)
library(censusapi) 
library(bea.R)

#-------------------- Changes directory to script location ---------------------
setwd(normalizePath(dirname(rstudioapi::getSourceEditorContext()$path),winslash = "\\"))
#-------------------------------------------------------------------------------

# get Small Area Income and Poverty Estimates
# (SAIPE) from the U.S. Census Bureau
# And Other BLS and BEA control variables

 source('scraping_functions.R')
 saipe_scraping()
 
 source('scraping_functions.R')
 get_bls_bea_data()
 