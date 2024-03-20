<a name="br1"></a> 

Overview

The code in this replication package installs all necessary commands and runs all the analyses for the paper " From High

School to Higher Education: Is recreational marijuana a consumption amenity for US college students?" by Ahmed El

Fatmaoui. All analysis is done in R. A parent ﬁle (master.R) can be used to run all ﬁles at once, calling other scripts to

install and load required packages and create tables and ﬁgures. master.R script code saves tables in tables folder and

ﬁgures in ﬁgures folder. To replicate a single table or ﬁgure, run only the source named after that table or ﬁgure (e.g.,

run source("table\_A7.R") to replicate table A7). Replicators can expect the code to take about 1 hour to run. Mainly,

replication of ﬁgure 6 and table A9 takes a longer time to run as it computes the distance between each college location

and the closest treated state border.

Figure 1: Layout of Replication Files

replication-files-rml

data

clean\_data (cleaned/merged data)

source\_data (raw data)

controls

ipeds

others

programs

data\_cleaning (Extracts data from the sources and merges data)

bls\_bea

census\_pop

completion\_cleaning

enrollment\_cleaning

google\_trend

grad\_rates\_panel

ipeds\_downloading

marijuana\_price

fig\_tab (creates tables and figures)

master.R (performs all data cleaning and analysis procedures)

figure\_#.R

table\_#.R

figures

appendix

main

tables

appendix

main

1



<a name="br2"></a> 

Figure [1](#br1)[ ](#br1)illustrates the layout of the replication ﬁles. The data folder encompasses two primary subfolders. One folder

(source\_data) houses the raw data obtained from [National](#br9)[ ](#br9)[Center](#br9)[ ](#br9)[for](#br9)[ ](#br9)[Education](#br9)[ ](#br9)[Statistics](#br9)[ ](#br9)[(2022](#br9)[a](#br9)) or other sources

[(Bureau](#br9)[ ](#br9)[of](#br9)[ ](#br9)[Labor](#br9)[ ](#br9)[Statistics,](#br9)[ ](#br9)[2021,](#br9)[ ](#br9)[Bureau](#br9)[ ](#br9)[of](#br9)[ ](#br9)[Economic](#br9)[ ](#br9)[Analysis,](#br9)[ ](#br9)[2021,](#br9)[ ](#br9)[U.S.](#br9)[ ](#br9)[Census](#br9)[ ](#br9)[Bureau,](#br9)[ ](#br9)[2021).](#br9)[ ](#br9)The other folder

contains the processed data, speciﬁcally the merged IPEDS data.

Similarly, the programs folder, which comprises all the R scripts, includes a subfolder (data\_cleaning) responsible for

downloading and cleaning the data. Another subfolder (ﬁg\_tab) within the programs folder executes all analyses. The

latter saves ﬁgures and tables in their respective folders located within the last two major folders illustrated in Figure [1.](#br1)

Data Availability and Provenance Statements

The paper uses mainly IPEDS data [National](#br9)[ ](#br9)[Center](#br9)[ ](#br9)[for](#br9)[ ](#br9)[Education](#br9)[ ](#br9)[Statistics](#br9)[ ](#br9)[(2022](#br9)[a](#br9)), and two other data from [Bureau](#br9)

[of](#br9)[ ](#br9)[Economic](#br9)[ ](#br9)[Analysis](#br9)[ ](#br9)[(2021)](#br9)<sup>1</sup>, [Bureau](#br9)[ ](#br9)[of](#br9)[ ](#br9)[Labor](#br9)[ ](#br9)[Statistics](#br9)[ ](#br9)[(2021),](#br9)[ ](#br9)and [U.S.](#br9)[ ](#br9)[Census](#br9)[ ](#br9)[Bureau](#br9)[ ](#br9)[(2021).](#br9)[ ](#br9)Other data used

in appendix are from Google Trends and priceofweed.com. See section 3 and appendix B for detailed description of

these data. I certify that the author(s) of the manuscript have legitimate access to and permission to use, redistribute,

and publish the data used in this manuscript. All data are publicly available and have been deposited in the ICPSR

repository of this paper.

Dataset List

Table 1: Table 1: datasets Used in Paper

Dataset

Data ﬁles

Description and processing

Data Location

Citation

Provided

IPEDS: Fall

Enrollment

enroll\_main.csv,

enroll\_all.csv,

Data source [(Link):](https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx?gotoReportId=7&fromIpeds=true&sid=ac68b949-876c-439b-abf0-e431b89449a2&rtid=1)

data/clean\_data

[National](#br9)[ ](#br9)[Center](#br9)

TRUE

\- IPEDs Survey: Fall Enrollment

[for](#br9)

[Educa-](#br9)

enroll\_vocational.csv

\- IPEDs Title: Race/ethnicity, gender, attendance status, and level of

student: Fall (2009-2019).

[tion](#br9)[ ](#br9)[Statistics](#br9)

[(2022](#br9)[a](#br9))

Data Processing

\- data\_cleaning/ipeds\_downloading/IPEDS\_scraping.R downloads

the data.

\- data\_cleaning/enrollment\_cleaning/enroll\_cleaning.R merges the

data together and saves for vocational (enroll\_vocational), academic

(enroll\_main) and all institutions (enroll\_all).

Continued on next page

<sup>1</sup>This data is extracted using the bea.R package in R. Prior to accessing the data, the user must obtain an API key. Further details can be found at

[https://github.com/us-bea/bea.R.](https://github.com/us-bea/bea.R)

2



<a name="br3"></a> 

Table 1 – continued from previous page

Description and processing

Dataset

IPEDS—

Graduation

rates

Data ﬁles

Data Location

Citation

Provided

grad\_rates.csv

Data source [(Link):](https://nces.ed.gov/ipeds/datacenter/MasterVariableList.aspx?cFrom=ADDVARIABLE&sid=ac68b949-876c-439b-abf0-e431b89449a2&rtid=1)

data/clean\_data

[National](#br9)[ ](#br9)[Center](#br9)

TRUE

\- The link will navigate you to the dataset detailing graduation

rates over 4, 5, and 6-year periods. Click ‘Continue’ as required

until the data is successfully downloaded to your local machine

(data/source\_data/ipeds/gradrateraw.csv).

Data Processing

[for](#br9)

[Educa-](#br9)

[tion](#br9)[ ](#br9)[Statistics](#br9)

[(2022](#br9)[b](#br9))

\- data\_cleaning/grad\_rates\_panel/gradratespanel.R merges the data

together.

IPEDS—

completion.csv

Data source [(Link):](https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx?gotoReportId=7&fromIpeds=true&sid=ac68b949-876c-439b-abf0-e431b89449a2&rtid=1)

data/clean\_data

[National](#br9)[ ](#br9)[Center](#br9)

TRUE

Completion

\- IPEDs Survey: Completions

[for](#br9)

[Educa-](#br9)

\- IPEDs Title: Awards/degrees conferred by program (6-digit CIP

code), award level, race/ethnicity, and gender: (2009-2019).

Data Processing

[tion](#br9)[ ](#br9)[Statistics](#br9)

[(2022](#br9)[a](#br9))

\- data\_cleaning/ipeds\_downloading/IPEDS\_scraping.R downloads

the data.

\- data\_cleaning/completion\_cleaning/compl\_cleaning.R merges the

data together.

PEDS— Tu-

ition revenue

and retention

rates

welfare.csv

Data source [(Link):](https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx?gotoReportId=7&fromIpeds=true&sid=ac68b949-876c-439b-abf0-e431b89449a2&rtid=1)

data/clean\_data

[National](#br9)[ ](#br9)[Center](#br9)

TRUE

[for](#br9)

[Educa-](#br9)

\1) Tuition revenue

[tion](#br9)[ ](#br9)[Statistics](#br9)

\- IPEDs Survey: Finance

[(2022](#br9)[a](#br9))

\- IPEDs Title: all of the ﬁnance surveys (2009-2019)

\2) Retention rates

\- IPEDs Survey: Fall Enrollment

\- IPEDs Title: Race/ethnicity, gender, attendance status, and level of

student: Fall (2009-2019)

Data Processing

\- data\_cleaning/ipeds\_downloading/IPEDS\_scraping.R downloads

the data.

\- data\_cleaning/enrollment\_cleaning/welfare\_cleaning.R merges the

data together.

IPEDS—

adm.csv

Data source [(Link):](https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx?gotoReportId=7&fromIpeds=true&sid=ac68b949-876c-439b-abf0-e431b89449a2&rtid=1)

data/clean\_data

[National](#br9)[ ](#br9)[Center](#br9)

TRUE

Admission

[for](#br9)

[Educa-](#br9)

and

test

\- IPEDs Survey: Admissions and Test Scores

[tion](#br9)[ ](#br9)[Statistics](#br9)

scores

\- IPEDs Title: Admission considerations, applications, admissions,

enrollees and test scores, fall (2009-2019)

[(2022](#br9)[a](#br9))

Data Processing

\- data\_cleaning/ipeds\_downloading/IPEDS\_scraping.R downloads

the data.

Continued on next page

3



<a name="br4"></a> 

Table 1 – continued from previous page

Dataset

IPEDS—

Fall

Data ﬁles

Description and processing

Data Location

Citation

Provided

resid\_ﬁrst\_enrol.csv

Data source [(Link):](https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx?gotoReportId=7&fromIpeds=true&sid=ac68b949-876c-439b-abf0-e431b89449a2&rtid=1)

data/source\_data/ipeds

[National](#br9)[ ](#br9)[Center](#br9)

TRUE

En-

[for](#br9)

[Educa-](#br9)

rollment,

residency

\- IPEDs Survey: Fall Enrollment

[tion](#br9)[ ](#br9)[Statistics](#br9)

\- IPEDs Title: Residence and migration of ﬁrst-time freshman: Fall

(2009-2019)

[(2022](#br9)[a](#br9))

Data Processing

\- data\_cleaning/ipeds\_downloading/IPEDS\_scraping.R downloads

the data.

IPEDS— Fi-

nance

ﬁnance\_all.csv

Data source [(Link):](https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx?gotoReportId=7&fromIpeds=true&sid=ac68b949-876c-439b-abf0-e431b89449a2&rtid=1)

data/source\_data/ipeds

[National](#br9)[ ](#br9)[Center](#br9)

TRUE

ﬁnance\_fasb.csv

ﬁnance\_gasp.csv

ﬁnance\_private.csv

ﬁnance\_public.csv

[for](#br9)

[Educa-](#br9)

\- All Finance surveys except "Response status for all survey compo-

[tion](#br9)[ ](#br9)[Statistics](#br9)

nents" from 2009 to 2019

[(2022](#br9)[a](#br9))

Data Processing

\- data\_cleaning/ipeds\_downloading/IPEDS\_scraping.R downloads

the data.

IPEDS—

Institutional

Characteris-

tics

df\_inst\_char.csv

df\_inst\_char2.csv

df\_inst\_char3.csv

Data source [(Link):](https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx?gotoReportId=7&fromIpeds=true&sid=ac68b949-876c-439b-abf0-e431b89449a2&rtid=1)

data/source\_data/ipeds

[National](#br9)[ ](#br9)[Center](#br9)

TRUE

[for](#br9)

[Educa-](#br9)

\- IPEDs Survey: Institutional Characteristics

\- IPEDS Titles:

[tion](#br9)[ ](#br9)[Statistics](#br9)

[(2022](#br9)[a](#br9))

Directory information (2009-2019)

Educational offerings, organization, services and athletic associa-

tions (2009-2019)

Student charges for academic year programs (2009-2019)

Data Processing

\- data\_cleaning/ipeds\_downloading/IPEDS\_scraping.R downloads

the data.

IPEDS—

12-Month

Enrollment

headcounts.csv

Data source [(Link):](https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx?gotoReportId=7&fromIpeds=true&sid=ac68b949-876c-439b-abf0-e431b89449a2&rtid=1)

data/source\_data/ipeds

[National](#br9)[ ](#br9)[Center](#br9)

TRUE

[for](#br9)

[Educa-](#br9)

\- IPEDs Survey: 12-Month Enrollment

\- IPEDS Titles: 12-month unduplicated headcount by race/ethnicity,

gender and level of student:(2009-2019)

Data Processing

[tion](#br9)[ ](#br9)[Statistics](#br9)

[(2022](#br9)[a](#br9))

\- data\_cleaning/ipeds\_downloading/IPEDS\_scraping.R downloads

the data.

Continued on next page

4



<a name="br5"></a> 

Table 1 – continued from previous page

Dataset

Data ﬁles

Description and processing

Data Location

Citation

Provided

IPEDS—

Fall Enroll-

ment

quality\_measures.csv

Data source [(Link):](https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx?gotoReportId=7&fromIpeds=true&sid=ac68b949-876c-439b-abf0-e431b89449a2&rtid=1)

data/source\_data/ipeds

[National](#br9)[ ](#br9)[Center](#br9)

TRUE

[for](#br9)

[Educa-](#br9)

\- IPEDs Survey: Fall Enrollment

[tion](#br9)[ ](#br9)[Statistics](#br9)

\- IPEDS Titles: Total entering class, retention rates, and student-to-

faculty ratio: (2009-2019)

[(2022](#br9)[a](#br9))

Data Processing

\- data\_cleaning/ipeds\_downloading/IPEDS\_scraping.R downloads

the data.

BEA, BLS,

and Census

Data

bls\_bea\_data.csv cen-

sus\_agesex.csv cen-

sus\_migration.csv

County level data for population, unemployment rate and per capita

income from 2009 to 2019:

data/source\_data/controls [(Bureau](#br9)[ ](#br9)[of](#br9)[ ](#br9)[Eco-](#br9)

[nomic](#br9)[ ](#br9)[Analysis,](#br9)

TRUE

[2021,](#br9)[ ](#br9)[Bureau](#br9)[ ](#br9)[of](#br9)

BLS data source [(Link)](https://web.archive.org/web/20190831005817/https://www.bls.gov/lau)

BEA data source [(Link)](https://github.com/us-bea/bea.R)

Census data source [(Link)](https://www2.census.gov/programs-surveys/popest/datasets/)

[Labor](#br9)[ ](#br9)[Statistics,](#br9)

[2021,](#br9)

[U.S.](#br9)

[Census](#br9)[ ](#br9)[Bureau,](#br9)

[2021)](#br9)

Data Processing

\- data\_cleaning/census\_pop/census\_data.R downloads the census

data.

\- data\_cleaning/bls\_bea/scraping\_functions.R downloads the BLS

and BEA data.

Other Data

WeedPrice.csv

cpi\_data.csv

Google trends for marijuana related keywords, marijuana prices,

and medical marijuana legalization timeline from 2009 to 2019:

data/source\_data/controls [National](#br9)[ ](#br9)[Center](#br9)

TRUE

[for](#br9)

[Educa-](#br9)

cleaned\_price\_df.csv

google\_trend\_df.csv

trends\_raw.csv

medical\_marij

medical.csv

[tion](#br9)[ ](#br9)[Statistics](#br9)

Google Trends [(Link)](https://cran.r-project.org/web/packages/gtrendsR/gtrendsR.pdf)

[(2022](#br9)[a](#br9))

Price of Weed data [(Link)](https://www.priceofweed.com/)

Legalization timeline [(Marijuana](#br9)[ ](#br9)[Policy](#br9)[ ](#br9)[Project,](#br9)[ ](#br9)[2022,](#br9)[ ](#br9)[Carnevale](#br9)

[Associates,](#br9)[ ](#br9)[2022)](#br9)

CPI data [(Link)](https://www.minneapolisfed.org/about-us/monetary-policy/inflation-calculator/consumer-price-index-1913-)

Data Processing

\-

/data\_cleaning/google\_trend/google\_trend\_data.R processes

Google Trends data.

/data\_cleaning/marijuana\_price/marijuana\_price\_scraping.R

\-

processes priceofweed.com data and retrieving the old data using

archive.org/web.

Note: To locate the data on the provided link source, users should visit the National Center for Education Statistics

5



<a name="br6"></a> 

(NCES) [website.](https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx?gotoReportId=7&fromIpeds=true&sid=ac68b949-876c-439b-abf0-e431b89449a2&rtid=1)[ ](https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx?gotoReportId=7&fromIpeds=true&sid=ac68b949-876c-439b-abf0-e431b89449a2&rtid=1)Once there, they can navigate to the section containing data from the Integrated Postsecondary

Education Data System (IPEDS) surveys. These surveys are identiﬁed by two key variables: the IPEDS Survey (Survey

column) and the IPEDS Survey Title (Title column). Users can ﬁnd these identiﬁers listed in Table 1 under the ‘Data

Source’ column for each IPEDS dataset. Note that data\_cleaning/ipeds\_downloading/IPEDS\_scraping.R downloads

the raw IPEDS data from [National](#br9)[ ](#br9)[Center](#br9)[ ](#br9)[for](#br9)[ ](#br9)[Education](#br9)[ ](#br9)[Statistics](#br9)[ ](#br9)[(2022](#br9)[a](#br9)) and saves the data in data/source\_data/ipeds.

In data\_cleaning/ipeds\_downloading/IPEDS\_scraping.R, there are functions called to construct panel data from each

survey. For instance, the fall\_enroll\_race() function is utilized to extract yearly surveys for the ﬁrst dataset listed in the

table, thereby forming fall enrollment panels. All the data is accessed as of June, 2023. The additional raw data, not

listed in the provided table, includes: df\_completion.csv (refer to the Completion Dataset in the table for data sources),

df\_enroll\_fall\_race.csv (refer to the ﬁrst Fall Enrollment Dataset in the table for data sources), df\_adm\_act.csv (refer to

the Admission and test scores Dataset in the table for data sources), grad-rate-raw.csv (refer to the Residence Dataset

in the table for data sources), and resid\_ﬁrst\_enrol.csv (refer to the Residence Dataset in the table for data sources).

Further documentation is provided in the programs/data\_cleaning/ subfolders.

Computational Requirements

Software Requirements

While the code should run in most computers as it is not computationally expensive, the code was run on a MacBook

Pro with the following speciﬁcations:

• System: macOS Monterey (version 12)

• Processor: 2.3 GHz 8-Core Intel Core i9 and

• Memory: 16 GB 2667 MHz DDR4

The only software used is R. The code has been run with R version 4.2.2 (2022-10-31). All programs used can be

installed by running install\_load\_packages.R which is called in master.R. They include the following:

• tidyverse

• haven

• ggplot2

• dplyr

• rvest

• ﬁxest

• lubridate

• modelsummary

• bacondecomp

• fwildclusterboot

• kableExtra

• geosphere

• rnaturalearthhires

• magrittr

• did2s

6



<a name="br7"></a> 

• rmapshaper

• tigris

• readxl

• magick

• scales

• ggpattern

• grid

• sf

• urbnmapr

• urbnthemes

• devtools

• maps

• ggspatial

• rnaturalearth

• matrixStats

• groupdata2

• tikzDevice

• gtrendsR

Several packages (pacbacondecomp, gtrendsR, rnaturalearthhires, urbnthemes, urbnmapr) are loaded from GitHub

using either the devtools or remotes package. The script install\_load\_packages.R is responsible for executing the

package loading process from both CRAN and GitHub. For GitHub packages, users should expect to answer some

prompt questions to load all the required packages.

Memory and Runtime Requirements

Running master.R takes no more than one hour to run, but replicators can replicate each table and ﬁgure separately by

running only the source of the table or ﬁgure of interest. With the exception of spillover related ﬁgures, which require

computation of distance between institutions locations and treated states borders, each ﬁgure or table should run in no

more than 10 minutes.

Description of Programs and Instructions to Replicators

The repository has four main folders: programs, ﬁgures, tables, and data. programs/ﬁg\_tab folder contains all the scripts

needed to replicate all the tables and ﬁgures. in programs/ﬁg\_tab folder, master.R (orchestrator script) replicates all the

tables an ﬁgures. The tables are saved in tables folder; ﬁgures are saved in ﬁgures folder.

programs/data\_cleaning contains codes for downloading and saving raw data from the source. For instance, pro-

grams/data\_cleaning/ipeds\_downloading contains code that downloads IPEDS surveys from the NCES source (see

ReadMe.Docx ﬁle in the same folder). The raw data is then saved in data/source\_data/ipeds and the cleaned data which is

merged with control variables is then saved in data/clean\_data (see scripts in programs/data\_cleaning/enrollment\_cleaning

folder).

Lines 16 to 39 in the master.R ﬁle execute all data cleaning processes in the speciﬁed order. It is important to note

that these processes are commented out, so the user needs to uncomment these lines before executing. Additionally,

7



<a name="br8"></a> 

executing this will update all data ﬁles. It is worth mentioning that all data was last accessed as of December 2023.

As mentioned in footnote 1, Prior to using bea.R package to extract BEA data, the user must obtain an API key (see

[https://github.com/us-bea/bea.R).](https://github.com/us-bea/bea.R)[ ](https://github.com/us-bea/bea.R)Starting from line 46, master.R runs all the analysis to generates tables and ﬁgures on

this paper. Please note that main tables presented in the paper’s main ﬁndings adhere to a 5 percent signiﬁcance level.

Therefore, any indication of signiﬁcance at the 10 percent level, which is denoted by a plus sign in some main tables, is

omitted. For instance, any plus signs in tab\_2\_med\_completion\_Associate’s degree.tex are presented in Table 2 without

the "+" sign.

Any minor discrepancies in the wild bootstrap p-values in the tables are solely attributed to the use of different seeds in

older versions. Changing the seed in data-sources.R will lead to slightly different wild bootstrap p-values. Parallel

computing and random number generation algorithms could also introduce slight variations in the wild bootstrap

p-values. However, it is important to note that the results remain qualitatively the same despite these differences.

List of Tables, Figures and Programs

To facilitate the replication and comprehension of the code, each script in the "programs/ﬁg\_tab" directory is named

after the tables or ﬁgures it generates. Figures and tables are saved with numerical initials corresponding to their

respective numbers in the paper. Figures are stored in either the "ﬁgures/main" or "ﬁgures/appendix" directory, while

tables are saved in the "tables/main" or "tables/appendix" directory.

For example, executing source(ﬁgure\_2.R) in master.R generates Figure 2, which is then saved as ﬁg\_2\_a\_main\_did.eps

and ﬁg\_2\_b\_main\_did.eps in the ﬁgures/main directory. All source code ﬁles included in master.R adhere to this

convention for creating tables and ﬁgures.

8



<a name="br9"></a> 

References

Bureau of Economic Analysis (2021), ‘County level per capita income’, [https://www.bea.gov/data.](https://www.bea.gov/data)[ ](https://www.bea.gov/data)Accessed:

2023-03-19.

Bureau of Labor Statistics (2021), ‘Labor force data by county’, [https://www.bls.gov/lau/tables.htm#cntyaa.](https://www.bls.gov/lau/tables.htm#cntyaa)[ ](https://www.bls.gov/lau/tables.htm#cntyaa)Accessed:

2023-03-19.

Carnevale Associates (2022), ‘Status of state Marijuana Legalization - Carnevale Info Brief’, [https://www.carnevaleass](https://www.carnevaleassociates.com/our-work/status-of-state-marijuana-legalization.html)

[ociates.com/our-work/status-of-state-marijuana-legalization.html.](https://www.carnevaleassociates.com/our-work/status-of-state-marijuana-legalization.html)[ ](https://www.carnevaleassociates.com/our-work/status-of-state-marijuana-legalization.html)Online; accessed March 20, 2022.

Marijuana Policy Project (2022), ‘State policy’, [https://www.mpp.org/states/.](https://www.mpp.org/states/)[ ](https://www.mpp.org/states/)Online; accessed March 20, 2022.

National Center for Education Statistics (2022a), ‘Integrated postsecondary education data system (ipeds)’, [https:](https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx?gotoReportId=7&fromIpeds=true&sid=ac68b949-876c-439b-abf0-e431b89449a2&rtid=1)

[//nces.ed.gov/ipeds/datacenter/DataFiles.aspx?gotoReportId=7&fromIpeds=true&sid=ac68b949-876c-439b-abf](https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx?gotoReportId=7&fromIpeds=true&sid=ac68b949-876c-439b-abf0-e431b89449a2&rtid=1)

[0-e431b89449a2&rtid=1.](https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx?gotoReportId=7&fromIpeds=true&sid=ac68b949-876c-439b-abf0-e431b89449a2&rtid=1)[ ](https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx?gotoReportId=7&fromIpeds=true&sid=ac68b949-876c-439b-abf0-e431b89449a2&rtid=1)Accessed: 2022-4-9.

National Center for Education Statistics (2022b), ‘Integrated postsecondary education data system (ipeds)’, [https:](https://nces.ed.gov/ipeds/datacenter/MasterVariableList.aspx?cFrom=ADDVARIABLE&sid=ac68b949-876c-439b-abf0-e431b89449a2&rtid=1)

[//nces.ed.gov/ipeds/datacenter/MasterVariableList.aspx?cFrom=ADDVARIABLE&sid=ac68b949-876c-439b-abf](https://nces.ed.gov/ipeds/datacenter/MasterVariableList.aspx?cFrom=ADDVARIABLE&sid=ac68b949-876c-439b-abf0-e431b89449a2&rtid=1)

[0-e431b89449a2&rtid=1.](https://nces.ed.gov/ipeds/datacenter/MasterVariableList.aspx?cFrom=ADDVARIABLE&sid=ac68b949-876c-439b-abf0-e431b89449a2&rtid=1)[ ](https://nces.ed.gov/ipeds/datacenter/MasterVariableList.aspx?cFrom=ADDVARIABLE&sid=ac68b949-876c-439b-abf0-e431b89449a2&rtid=1)Accessed: 2022-4-9.

U.S. Census Bureau (2021), ‘County level population by age groups’, [https://www2.census.gov/programs-surveys/pop](https://www2.census.gov/programs-surveys/popest/datasets/)

[est/datasets/.](https://www2.census.gov/programs-surveys/popest/datasets/)[ ](https://www2.census.gov/programs-surveys/popest/datasets/)Accessed: 2023-03-19.

9


