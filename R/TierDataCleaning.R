## Import Tier Data from Excel

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
  #skimr,      # Exploring data
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

# load functions used from R scripts
source_path <- here("R", "RScripts.R")
source(source_path,local = knitr::knit_global())

data_folder <- "data/"
image_folder <- "images/"
output_folder <- "output/"
excelDBFile <- "TierXmlExcel.xlsx"

###############################################################################
#################   I. Importing & Exploring Data         #####################
###################   Version of Tier.net : 1.13.4      #######################
###############################################################################


############ !!!!  NOTE : Excel template  when importing data !!!! #############
################################################################################

dem_raw       <- readxl::read_excel(here(data_folder,excelDBFile),sheet="DEM")
pat_raw       <- readxl::read_xlsx(here(data_folder,excelDBFile),sheet = "PAT")
## If your dabase visit table is beyond 400000 rows, you can filter TierXmlFile
## excel file and remove visits that are more than 5 years from current reporting period 
vis_raw       <- readxl::read_xlsx(here(data_folder,excelDBFile),sheet = "VIS")
art_raw       <- readxl::read_xlsx(here(data_folder,excelDBFile),sheet = "ART")
tb_raw        <- readxl::read_xlsx(here(data_folder,excelDBFile),sheet = "TB")
lab_raw       <- readxl::read_xlsx(here(data_folder,excelDBFile),sheet = "LAB")
transfers_raw <- readxl::read_xlsx(here(data_folder,excelDBFile),sheet = "TRANSFERS")
vis_tb_raw    <- readxl::read_xlsx(here(data_folder,excelDBFile),sheet = "VIS_TB")
infant_raw    <- readxl::read_xlsx(here(data_folder,excelDBFile),sheet = "INFANT")
vis_infant_raw<- readxl::read_xlsx(here(data_folder,excelDBFile),sheet = "VIS_INFANT")
pregnancy_raw <- readxl::read_xlsx(here(data_folder,excelDBFile),sheet = "PREGNANCY")
vis_preg_raw  <- readxl::read_xlsx(here(data_folder,excelDBFile),sheet = "VIS_PREG")


object.size(vis_raw)

################################################################################
##########################     II. Cleaning data    ############################
################################################################################

############# 1. Cleaning column names    #################
###########################################################

dem       <- dem_raw %>% janitor::clean_names()
pat       <- pat_raw %>% janitor::clean_names()
vis       <- vis_raw %>% janitor::clean_names()
art       <- art_raw %>% janitor::clean_names()
tb        <- tb_raw  %>% janitor::clean_names()
lab       <- lab_raw %>% janitor::clean_names()
transfers <- transfers_raw  %>% janitor::clean_names()
vis_tb    <- vis_tb_raw     %>% janitor::clean_names()
infant    <- infant_raw     %>% janitor::clean_names()
vis_infant<- vis_infant_raw %>% janitor::clean_names()
pregnancy <- pregnancy_raw  %>% janitor::clean_names()
vis_preg  <- vis_preg_raw   %>% janitor::clean_names()

#tapply(pat_raw$PATIENT,pat_raw$FACILITY, function(x) length(unique(x)))
# pat_raw %>% filter(OUTCOME==20)%>% tabyl(FACILITY)

####### 2. Select,add and/or re order/rename columns #######
############################################################
vis <- vis %>%
  select(patient, visit_dmy, tb_status, who_stage, next_visit_dmy) %>%
  #Convert all columns to character class
  mutate(across(.cols = everything(), .fns = as.character)) %>%
  #Trim all variable contents
  mutate(across(where(is.character), str_trim))

pat <- pat %>%
  select(patient, cohort, facility, birth_dmy,gender, frsvis_dmy,hivp_dmy, haart,
         haart_dmy, fhv_stage_who, tb_fhv, method_into_art, transfer_in_dmy, outcome,
         outcome_dmy) %>%
  #Convert all columns to character class
  mutate(across(.cols = everything(), .fns = as.character)) %>%
  #Trim all variable contents
  mutate(across(where(is.character), str_trim))

lab <- lab %>%
  select(patient, lab_dmy, lab_id, lab_v, lab_t, episode_type, episode_id) %>%
  #Convert all columns to character class
  mutate(across(.cols = everything(), .fns = as.character)) %>%
  #Trim all variable contents
  mutate(across(where(is.character), str_trim))

dem <- dem %>%
  select(patient, folder_number) %>%
  #Convert all columns to character class
  mutate(across(.cols = everything(), .fns = as.character)) %>%
  #Trim all variable contents
  mutate(across(where(is.character), str_trim))

art <- art %>%
  select(-c(art_rs,info_source)) %>%
  #Convert all columns to character class
  mutate(across(.cols = everything(), .fns = as.character)) %>%
  #Trim all variable contents
  mutate(across(where(is.character), str_trim))



tb <- tb %>%
  select(patient, reg_dmy, tb_start_dmy, tb_end_dmy, tb_outcome, episode_id) %>%
  #Convert all columns to character class
  mutate(across(.cols = everything(), .fns = as.character)) %>%
  #Trim all variable contents
  mutate(across(where(is.character), str_trim))

vis_tb <- vis_tb %>%
  select(patient, visit_dmy, next_visit_dmy, episode_id) %>%
  #Convert all columns to character class
  mutate(across(.cols = everything(), .fns = as.character)) %>%
  #Trim all variable contents
  mutate(across(where(is.character), str_trim))

transfers <- transfers %>%
  select(patient, episode_id, facility_from, facility_to, transfer_dmy, transfer_action) %>%
  #Convert all columns to character class
  mutate(across(.cols = everything(), .fns = as.character)) %>%
  #Trim all variable contents
  mutate(across(where(is.character), str_trim))

## Matching ART drugs codes
art_code <-sort(unique(art$art_id))
art_regimen <- data.frame(
  code  = c("J05AE08","J05AE10","J05AF01","J05AF04","J05AF05","J05AF06","J05AF07",
            "J05AF09","J05AG01","J05AG03","J05AR10","J05AX12"),
  ttt   = c("ATV","DRV","AZT","D4T","3TC","ABC","TDF","FTC","NVP","EFV","LPV","DTG")
)

## Match codes with corresponding regimens
art$art_id <- art_regimen$ttt[match(unlist(art$art_id),art_regimen$code)]

## Make art regimens as factor and order them
art$art_id <- factor(art$art_id,
                     levels = c("ABC","ATV","AZT","TDF","3TC","FTC","EFV","NVP",
                                "LPV","DTG","DRV","D4T")
)

################ 3. Cleaning/replacing empty/null values     ##########
#######################################################################

vis[is.null(vis)  | vis == ""] <- NA

pat[is.null(pat)  | pat == "NULL" | pat == ""] <- NA

lab[is.null(lab)  | lab == "NULL" | lab == ""] <- NA

dem[is.null(dem)  | dem == "NULL" | dem == ""] <- NA

art[is.null(art)  | art == "NULL" | art == ""] <- NA

tb[is.null(tb)  | tb == "NULL" | tb == ""] <- NA

vis_tb[is.null(vis_tb)  | vis_tb == "NULL" | vis_tb == ""] <- NA

transfers[is.null(transfers)  | transfers == "NULL" | transfers == ""] <- NA


############# 4. Convert columns classes/add columns  ###############
#####################################################################
vis <- vis %>%
  mutate(
    visit_dmy = parse_date(visit_dmy,"%Y-%m-%d"),
    next_visit_dmy = parse_date(next_visit_dmy,"%Y-%m-%d"),
    who_stage = parse_number(who_stage)
  ) %>%
  mutate(
    visit_dmy = as.Date(as.numeric(visit_dmy),origin ="1970-01-01"),
    next_visit_dmy = as.Date(as.numeric(next_visit_dmy),origin ="1970-01-01"),
    vis_year = year(visit_dmy)
  )



pat <- pat %>%
  mutate(
    birth_dmy = parse_date(birth_dmy,"%Y-%m-%d"),
    frsvis_dmy = parse_date(frsvis_dmy,"%Y-%m-%d"),
    hivp_dmy = parse_date(hivp_dmy,"%Y-%m-%d"),
    haart_dmy = parse_date(haart_dmy,"%Y-%m-%d"),
    transfer_in_dmy = parse_date(transfer_in_dmy,"%Y-%m-%d"),
    outcome_dmy = parse_date(outcome_dmy,"%Y-%m-%d"),
  ) %>%
  mutate(
    birth_dmy = as.Date(as.numeric(birth_dmy),origin ="1970-01-01"),
    frsvis_dmy = as.Date(as.numeric(frsvis_dmy),origin ="1970-01-01"),
    hivp_dmy = as.Date(as.numeric(hivp_dmy),origin ="1970-01-01"),
    haart_dmy = as.Date(as.numeric(haart_dmy),origin ="1970-01-01"),
    transfer_in_dmy = as.Date(as.numeric(transfer_in_dmy),origin ="1970-01-01"),
    outcome_dmy = as.Date(as.numeric(outcome_dmy),origin ="1970-01-01"),
  ) %>%
  mutate(
    #Add enrollment date
    enrol_date = case_when(
      method_into_art =="1" ~ transfer_in_dmy,
      method_into_art =="0" ~ haart_dmy 
    ),
    #Add enrollment year
    enrol_year  = year(enrol_date)
  )

lab <- lab %>%
  mutate(
    lab_dmy = parse_date(lab_dmy,"%Y-%m-%d"),
    lab_v = parse_number(lab_v)
  ) %>%
  mutate(
    lab_dmy = as.Date(as.numeric(lab_dmy),origin ="1970-01-01")
  )

art <- art %>%
  mutate(
    art_sd_dmy = parse_date(art_sd_dmy,"%Y-%m-%d"),
    art_ed_dmy = parse_date(art_ed_dmy,"%Y-%m-%d"),
  )


tb <- tb %>%
  mutate(
    reg_dmy = parse_date(reg_dmy,"%Y-%m-%d"),
    tb_start_dmy = parse_date(tb_start_dmy,"%Y-%m-%d"),
    tb_end_dmy = parse_date(tb_end_dmy,"%Y-%m-%d"),
  ) %>%
  mutate(
    reg_dmy = as.Date(as.numeric(reg_dmy),origin ="1970-01-01"),
    tb_start_dmy = as.Date(as.numeric(tb_start_dmy),origin ="1970-01-01"),
    tb_end_dmy = as.Date(as.numeric(tb_end_dmy),origin ="1970-01-01"),
  )

vis_tb <- vis_tb %>%
  mutate(
    visit_dmy = parse_date(visit_dmy,"%Y-%m-%d"),
    next_visit_dmy = parse_date(next_visit_dmy,"%Y-%m-%d"),
  ) %>%
  mutate(
    visit_dmy = as.Date(as.numeric(visit_dmy),origin ="1970-01-01"),
    next_visit_dmy = as.Date(as.numeric(next_visit_dmy),origin ="1970-01-01")
  )

transfers <- transfers %>%
  mutate(
    transfer_dmy = parse_date(transfer_dmy,"%Y-%m-%d")
  ) %>%
  mutate(
    transfer_dmy = as.Date(as.numeric(transfer_dmy),origin ="1970-01-01")
  )

################ 5. Cleaning duplicate rows      #############
##############################################################
pat<-unique(pat)

vis<-unique(vis)

lab<-unique(lab)

art <-unique(art)

tb<-unique(tb)

vis_tb<-unique(vis_tb)

transfers<-unique(transfers)



################# 6. Merge tables                #################
##################################################################

patient_data <-left_join(pat,dem,"patient")

vis_data <- left_join(vis,dem,"patient") %>%
  left_join(pat,"patient")

left_join(vis_data,pat,"patient")



art_data <-left_join(art,pat,"patient")

################# 7. Add variables/values labels    ##############
##################################################################
# labelled::val_labels(vis_data$gender)  <-c("1" = "Male", "2" = "Female", "95" = NA, "99" = NA)
# labelled::val_labels(vis_data$haart)   <-c("0" = "Not on ART", "1" = "on ART")
# labelled::val_labels(vis_data$outcome) <-c("10"= "Deceased", "20" = "In care", "30" = "Transfered Out", "40" = "LTFU", "95" = "Unknown")
# labelled::val_labels(vis_data$method_in_art_named) <-c("0" = "New", "1" = "Transfer In", "88" = NA )


######## 7. Remove unused/intermediate objects from memory  #######
###################################################################

#rm(list = c("dem_raw","lab_raw","art_raw","pat_raw","tb_raw","transfers_raw",
#            "infant_raw","vis_raw","vis_tb_raw","pregnancy_raw","vis_pregnancy_raw")
#   )








































################----------------------------#######################################



## Arrange art table data
art <- art%>%
  arrange(patient,art_sd_dmy,art_id)
