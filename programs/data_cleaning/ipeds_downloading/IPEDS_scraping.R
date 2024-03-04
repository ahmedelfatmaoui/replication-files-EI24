library(rvest)
library(tidyverse)
library(plyr)

setwd("~/Desktop/mr_paper/programs/ipeds_downloading")
source('IPEDS_Scraping_Functions.R')
#-------------------------------------------------------------------------
## 1 ## Race/ethnicity, gender, attendance status, and level of student
#-------------------------------------------------------------------------

institution_characteristics() 

institution_characteristics2()
  
# Student charges for academic year programs                                        
institution_characteristics3()

###################
## Educational offerings, organization, applications, 
## admissions, enroll., test scores, services and athletic associations
###############################################################################
setwd("~/Desktop/mr_paper/programs/ipeds_downloading")
source('IPEDS_Scraping_Functions.R')

 admission() ## not used due to missing obs. when left joined to enroll. or completion 

 
 #-------------------------------------------------------------------------
 ## 12-month unduplicated headcount
 #-------------------------------------------------------------------------

 setwd("~/Desktop/mr_paper/programs/ipeds_downloading")
   source('IPEDS_Scraping_Functions.R')
   enrol_headcount()
 
#-------------------------------------------------------------------------
## 1 ## Race/ethnicity, gender, attendance status, and level of student
#-------------------------------------------------------------------------
   setwd("~/Desktop/mr_paper/programs/ipeds_downloading")
 source('IPEDS_Scraping_Functions.R')
 
 fall_enroll_race()

 
 #-------------------------------------------------------------------------
 ## finance data
 #-------------------------------------------------------------------------
 
 setwd("~/Desktop/mr_paper/programs/ipeds_downloading")
 source('IPEDS_Scraping_Functions.R')
 finance_public_gasp()
 finance_public_fasb()
 finance_private()
 
 fasb <- read_csv("~/Desktop/marijuana_enrollment/data/source_data/ipeds/finance_fasb.csv")
 gasp <- read_csv("~/Desktop/marijuana_enrollment/data/source_data/ipeds/finance_gasp.csv")
 private <- read_csv("~/Desktop/marijuana_enrollment/data/source_data/ipeds/finance_private.csv")
 finance_concat(fasb,gasp)
 finance_concat_all(fasb,gasp,private)
 
 
 
 #-------------------------------------------------------------------------
 ## Residence and migration of first-time freshman
 #-------------------------------------------------------------------------------------
 
 setwd("~/Desktop/mr_paper/programs/ipeds_downloading")
 source('IPEDS_Scraping_Functions.R')
 residence_first_enrol()
 
 
 
 #-------------------------------------------------------------------------
 ## Major field of study, race/ethnicity, gender, attendance status, and level of student: Fall 
 #-------------------------------------------------------------------------------------
 
 ## enrollment by major is available on IPEDS starting from 2010
 setwd("~/Desktop/mr_paper/programs/ipeds_downloading")
 source('IPEDS_Scraping_Functions.R')
 enroll_majors()
 
 #-------------------------------------------------------------------------
 ## Total entering class, retention rates, and student-to-faculty ratio
 #-------------------------------------------------------------------------
 setwd("~/Desktop/mr_paper/programs/ipeds_downloading")
 source('IPEDS_Scraping_Functions.R')
 quality_measures()
 
 
 #-------------------------------------------------------------------------
 ##  Awards/degrees conferred by program (6-digit CIP code), award level, race/ethnicity
 #-------------------------------------------------------------------------
 setwd("~/Desktop/mr_paper/programs/ipeds_downloading")
 source('IPEDS_Scraping_Functions.R')
 degree_completion()
 
 
 
 