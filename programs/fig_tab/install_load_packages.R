
# List of required libraries
libraries <- c(
  "tidyverse", "fixest", "modelsummary", "AER", "ivreg", "bacondecomp",
  "lmtest", "multiwayvcov", "plm", "lmtest", "sandwich", "latex2exp",
  "haven", "ggplot2", "dplyr", "tidyverse", "gt", "magrittr", "fastDummies",
  "bacondecomp", "did2s", "rvest", "lubridate", "nycflights13", "xml2",
  "dplyr", "kableExtra", "estimatr", "geosphere", "rnaturalearthhires",
  "rmapshaper", "tigris", "sf", "ggplot2", "ggspatial", "rnaturalearth",
  "matrixStats", "readxl", "scales", "urbnmapr", "urbnthemes", "devtools",
  "maps", "tigris", "sf", "magick", "ggpattern", "grid", "Synth",
  "synthdid", "groupdata2", "tikzDevice", "gtrendsR",
  "data.table","fwildclusterboot","educationdata"
)

# Function to check and install packages
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# Loop through the list and install missing packages
invisible(lapply(libraries, install_if_missing))


# Load all the libraries
library(tidyverse)
library(fixest)
library(modelsummary)
library(AER)
library(ivreg)
library(bacondecomp)
library(lmtest)
library(multiwayvcov)
library(plm)
library(lmtest)
library(sandwich)
library(latex2exp)
library(haven)
library(ggplot2)
library(dplyr)
library(gt)
library(magrittr)
library(fastDummies)
library(bacondecomp)
library(did2s)
library(rvest)
library(lubridate)
library(nycflights13)
library(xml2)
library(dplyr)
library(kableExtra)
library(estimatr)
library(geosphere)
library(rnaturalearthhires)
library(rmapshaper)
library(tigris)
library(sf)
library(ggplot2)
library(ggspatial)
library(rnaturalearth)
library(matrixStats)
library(readxl)
library(scales)
library(urbnmapr)
library(urbnthemes)
library(devtools)
library(maps)
library(tigris)
library(fwildclusterboot)
library(magick)
library(ggpattern)
library(grid)
library(Synth)
library(synthdid)
library(groupdata2)
library(tikzDevice)
library(gtrendsR)
library(data.table)
library(educationdata)

## NOTE: you may need to get the most updated bacondecomp from github

