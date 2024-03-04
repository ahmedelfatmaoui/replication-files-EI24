# Overview

The code in this replication package installs all necessary commands and runs all the analyses for the paper "From High School to Higher Education: Is recreational marijuana a consumption amenity for US college students?" by Ahmed El Fatmaoui. All analysis is done in R. A parent file (master.R) can be used to run all files at once, calling other scripts to install and load required packages and create tables and figures. The master.R script code saves tables in the `tables` folder and figures in the `figures` folder. To replicate a single table or figure, run only the source named after that table or figure (e.g., run `source("table_A7.R")` to replicate Table A7). Replicators can expect the code to take about 1 hour to run. Mainly, replication of Figure 6 and Table A9 takes a longer time to run as it computes the distance between each college location and the closest treated state border.

## Data Availability and Provenance Statements

The paper uses mainly IPEDS data (National Center for Education Statistics, 2019), and two other data from Bureau of Economic Analysis and the Bureau of Labor Statistics. Other data used in appendix are from Google Trends and priceofweed.com. See section 3 and appendix B for detailed description of these data.

I certify that the author(s) of the manuscript have legitimate access to and permission to use, redistribute, and publish the data used in this manuscript. All data are publicly available and have been deposited in the ICPSR repository of this paper.

### Dataset List

| Dataset          | Data files         | Description                                                               | Data Location  | Citation Provided |
|------------------|--------------------|---------------------------------------------------------------------------|----------------|-------------------|
| IPEDS—Fall Enrollment       | enroll_main.csv, enroll_all.csv, enroll_vocational.csv | - enroll_main: fall enrollment in academic institutions <br> - enroll_all: Fall enrollment in Academic and vocational institutions <br>- enroll_vocational: Fall enrollment in vocational institutions | data/clean_data (National Center for Education Statistics, 2019) | TRUE              |
| IPEDS—Graduation rates     | grad_rates.csv    | Graduation rates at 100% or 150% of normal time for associate and bachelor degrees | data/clean_data (National Center for Education Statistics, 2019) | TRUE              |
| IPEDS—Completion           | completion.csv    | Number of Bachelor and Associate Degrees                                   | data/clean_data (National Center for Education Statistics, 2019) | TRUE              |
| IPEDS—Tuition revenue and retention rates | welfare.csv | Tuition revenue per undergraduate student and retention rates             | data/clean_data (National Center for Education Statistics, 2019) | TRUE              |
| IPEDS—Admission and test scores | adm.csv           | Admission numbers and SAT and ACT scores for non-open admission policy institutions | data/clean_data (National Center for Education Statistics, 2019) | TRUE              |
| IPEDS—residence             | resid_first_enrol.csv | Enrollments by residence                                                 | data/source_data/ipeds (National Center for Education Statistics, 2019) | TRUE              |

**Table 1: Main datasets Used in Paper**

## Computational Requirements

### Software Requirements

While the code should run in most computers as it is not computationally expensive, the code was run on a MacBook Pro with the following specifications:
- System: macOS Monterey (version 12)
- Processor: 2.3 GHz 8-Core Intel Core i9
- Memory: 16 GB 2667 MHz DDR4

The only software used is R. The code has been run with R version 4.2.2 (2022-10-31). All programs used can be installed by running `install_load_packages.R` which is called in `master.R`. They include the following:
- tidyverse
- fixest
- modelsummary
- bacondecomp
- fwildclusterboot
- haven
- ggplot2
- dplyr
- magrittr
- did2s
- rvest
- lubridate
- kableExtra
- geosphere
- rnaturalearthhires
- rmapshaper
- tigris
- sf
- ggspatial
- rnaturalearth
- matrixStats
- readxl
- scales
- urbnmapr
- urbnthemes
- devtools
- maps
- magick
- ggpattern
- grid
- groupdata2
- tikzDevice
- gtrendsR

### Memory and Runtime Requirements

Running `master.R` takes no more than one hour to run, but replicators can replicate each table and figure separately by running only the source of the table or figure of interest. With the exception of spillover related figures, which require computation of distance between institutions locations and treated states borders, each figure or table should run in no more than 10 minutes.

## Description of Programs and Instructions to Replicators

The repository has four main folders: `programs`, `figures`, `tables`, and `data`. `programs/fig_tab` folder contains all the scripts needed to replicate all the tables and figures. In `programs/fig_tab` folder, `master.R` (orchestrator script) replicates all the tables an figures. The tables are saved in `tables` folder; figures are saved in `figures` folder.

`programs/data_cleaning` contains codes for downloading and saving raw data from the source. For instance, `programs/data_cleaning/ipeds_downloading` contains code that downloads IPEDS surveys from the NCES source1. The raw data is then saved in `data/source_data/ipeds` and the cleaned data which is merged with control variables is then saved in `data/clean_data`.

### IPEDS data sources:

- [IPEDS Data Files](https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx?gotoReportId=7&fromIpeds=true&sid=ac68b949-876c-439b-abf0-e431b89449a2&rtid=1)
- [Institution by Name](https://nces.ed.gov/ipeds/datacenter/InstitutionByName.aspx?sid=ac68b949-876c-439b-abf0-e431b89449a2&rtid=1)

## List of Tables, Figures and Programs

To enhance the ease of replicating and understanding the code, each script in the `programs/fig_tab` directory is named after the tables or figures it generates. The figures and tables are saved with numerical initials that correspond to their respective numbers in the paper. Figures are saved in `figures/main` or `figures/appendix`. Similarly, tables are saved in `tables/main` or `tables/appendix`.

As an illustration, executing `source("figure_2.R")` in `master.R` generates Figure 2, which is then saved as `fig_2_a_main_did.eps` and `fig_2_b_main_did.eps` in the `figures/main` directory. All source code files included in `master.R` follow this convention to create tables and figures.

## References

National Center for Education Statistics (2019), ‘Integrated postsecondary education data system (ipeds)’, [https://nces.ed.gov/statprog/handbook/pdf
