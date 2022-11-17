################################################################################
################## 0. LOAD PACKAGES AND SET UP ################################# 
################################################################################
pacman::p_load(
  rio,        # importing data  
  here,       # relative file pathways
  readxl,     # read excel
  writexl,    # write excel
  openxlsx,   # Write to Excel
  strex,      #String manipulation
  janitor,    # data cleaning and tables
  lubridate,  # working with dates
  epikit,     # age_categories() function
  tidyverse,  # data management and visualization,
  #gtsummary,  # summary statistics and tests,
  rstatix,    # summary statistics and statistical tests
  scales,     # easily convert proportions to percents  
  labelled,   # Variable and values labelling
  flextable,  # Format tables
  sqldf,      # Use sqlite queries
  RODBC,       # Odbc database connection,
  DiagrammeR,  # Create flowcharts and diagrams
  rsvg,       # Create images
  DiagrammeRsvg # Export diagrams
)

# use here from the here package
here <- here::here
# use clean_names from the janitor package
clean_names <- janitor::clean_names

excelDBFile <- "EXPORT DONNEES SYNTHESE DITONDO.xlsx"
data_folder <- "C:\\Users/Gauthier Abdala/MSF/SAMU M&E and OR - DRC_Kinshasa - Documents/DRC_Kinshasa/3. OR/SORT IT/"

###############################################################################
#################   I. Importing & Exploring Data         #####################
###################   Version of Tier.net : 1.13.4      #######################
###############################################################################

mydata       <- readxl::read_excel(paste(data_folder,excelDBFile,sep=""),sheet=1)

mydata_new <- mydata %>%
  separate(
    CRAG,
    into = c("cragresult","cragres"),
    sep = "(?<=[A-Za-z])(?=[0-9])"
  )

