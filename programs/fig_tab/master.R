# Setting directory to script location 
setwd(normalizePath(dirname(rstudioapi::getSourceEditorContext()$path),winslash = "\\"))


#----------------------------------------------------------
#-------------------------- TABLES ------------------------
#----------------------------------------------------------

# Summary Statistics (tables 1 & A1)
source("table_1_A1.R")

# RML effect on Log Number of Undergraduate Degrees (tables 2 & A4--panel a)
source("table_2_A4_panel_a.R")

# RML effect on Undergraduate Graduation Rates (tables 2 & A4--panels b and c)
source("table_2_A4_panel_b_c.R")

# RML effect on tuition and retention rate (tables 3 & A5)
source("table_3_A5.R")

# RML effect on Admission and Test Scores (tables 4 and A6)
source("table_4_A6.R")

# RML effect on in-State Enrollment (table A7)
source("table_A7.R")

# RML effect on Enrollment (tables A2 & A3)
source("table_A2_A3.R")

# Heterogeneous effect by the allowed cultivation (table A8)
source("table_A8.R")

# see figure_6_table_A9.R bellow for Table A9


#----------------------------------------------------------
#-------------------------- FIGURES -----------------------
#----------------------------------------------------------

# Figure 1: Treated States Legalization Timeline, Trends, and Control Groups 
# (Figure 1, A1, A2, A7, A8, A9)
source("figure_1_A1_A2_A7_A8_A9.R")


# RML Effect on First-Time Enrollment (Figure 2)
source("figure_2.R")

# main event study (Figure 3)
source("figure_3.R")

# Effects on out-of-state enrollments (Figure 4 and A10)
source("figure_4_A10.R")

# Heterogeneous Effects of Recreational Marijuana Legalization (Figure 5)
source("figure_5.R")

# Sensitivity to proximity spillover (figure 6 and table A9)
# Takes long to run due to the computing of distance matrix
# (i.e., each college and the closest treatment state border)
source("figure_6_table_A9.R")

# Other event study graphs (Figure A3, A4, A5)
source("figure_A3_A4_A5.R")

# Goodman-Bacon Decomposition (Figure A6)
source("figure_A6.R")


