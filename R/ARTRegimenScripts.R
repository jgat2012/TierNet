pacman::p_load(
  rio,        # importing data  
  here,       # relative file pathways
  readxl,     # read excel
  writexl,    # write excel
  strex,      #String manipulation
  janitor,    # data cleaning and tables
  lubridate,  # working with dates
  tidyverse,  # data management and visualization,
  scales,     # easily convert proportions to percents  
  labelled,   # Variable and values labelling
  flextable,  # Format tables
  RODBC       # Odbc database connection
)

# use here from the here package
here <- here::here
# use clean_names from the janitor package
clean_names <- janitor::clean_names

# load functions used from R scripts
source_path <- here("R", "RScripts.R")
source(source_path,local = knitr::knit_global())

data_folder <- "data"
image_folder <- "images"
output_folder <- "output"

###############################################################################
#################   I. Importing & Exploring Data         #####################
###################                                     #######################
###############################################################################

con<-RODBC::odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb)};DBQ=C:/Users/Gauthier Abdala/Documents/RWD/TierNet/data/TierDESKinshasa042022.mdb")

vis_raw <- RODBC::sqlFetch(con,"VIS") #Follow Up clinic Visit table
pat_raw <- RODBC::sqlFetch(con,"PAT") #Socio demographic char. & outcome table
lab_raw <- RODBC::sqlFetch(con,"LAB") #Laboratory data table
dem_raw <- RODBC::sqlFetch(con,"DEM") #Demographic information
art_raw <- RODBC::sqlFetch(con,"ART") #ART drugs information

tb_raw        <- RODBC::sqlFetch(con,"TB")         #Tuberculosis Information
vis_tb_raw    <- RODBC::sqlFetch(con,"VIS_TB")     #TB Visit Table
transfers_raw <- RODBC::sqlFetch(con,"TRANSFERS")  #Transfer In/Out Table

#Close DB Connection after getting the data
RODBC::odbcClose(con)

#############################################################################
########################      II. Cleaning data    ##########################
#############################################################################


############# 1. Cleaning column names    #################
###########################################################

vis <- vis_raw %>% janitor::clean_names()
pat <- pat_raw %>% janitor::clean_names()
lab <- lab_raw %>% janitor::clean_names()
dem <- dem_raw %>% janitor::clean_names()
art <- art_raw %>% janitor::clean_names()

#pregnancy <- pregnancy_raw  %>% janitor::clean_names()
tb        <- tb_raw         %>% janitor::clean_names()
vis_tb    <- vis_tb_raw     %>% janitor::clean_names()
transfers <- transfers_raw  %>% janitor::clean_names()
#vis_preg  <- vis_preg_raw   %>% janitor::clean_names()
#infant    <- infant_raw     %>% janitor::clean_names()


####### 2. Select,add and/or re order/rename columns #######
############################################################
vis <- vis %>%
  select(patient, visit_dmy, tb_status, who_stage, next_visit_dmy)

pat <- pat %>%
  select(patient, cohort, facility, birth_dmy,gender, frsvis_dmy,hivp_dmy, haart,
         haart_dmy, fhv_stage_who, tb_fhv, method_into_art, transfer_in_dmy, outcome,
         outcome_dmy)

lab <- lab %>%
  select(patient, lab_dmy, lab_id, lab_v, lab_t, episode_type, episode_id)

dem <- dem %>%
  select(patient, folder_number)
art <- art %>%
  select(-c(art_rs,info_source))

tb <- tb %>%
  select(patient, reg_dmy, tb_start_dmy, tb_end_dmy, tb_outcome, episode_id)

vis_tb <- vis_tb %>%
  select(patient, visit_dmy, next_visit_dmy, episode_id)

transfers <- transfers %>%
  select(patient, episode_id, facility_from, facility_to, transfer_dmy, transfer_action)



################ 3. Cleaning/replacing empty/null values     ##########
#######################################################################

vis <-trim_data_columns(vis)
vis[is.null(vis)  | vis == "NULL" | vis == ""] <- NA

pat <-trim_data_columns(pat)
pat[is.null(pat)  | pat == "NULL" | pat == ""] <- NA

lab <-trim_data_columns(lab)
lab[is.null(lab)  | lab == "NULL" | lab == ""] <- NA

dem <-trim_data_columns(dem)
dem[is.null(dem)  | dem == "NULL" | dem == ""] <- NA

art <-trim_data_columns(art)
art[is.null(art)  | art == "NULL" | art == ""] <- NA

tb <-trim_data_columns(tb)
tb[is.null(tb)  | tb == "NULL" | tb == ""] <- NA

vis_tb <-trim_data_columns(vis_tb)
vis_tb[is.null(vis_tb)  | vis_tb == "NULL" | vis_tb == ""] <- NA

transfers <-trim_data_columns(transfers)
transfers[is.null(transfers)  | transfers == "NULL" | transfers == ""] <- NA

## Check if any of the dataframes have missing values
colSums(is.na(vis))
colSums(is.na(pat))
colSums(is.na(lab))
colSums(is.na(dem))
colSums(is.na(art))
colSums(is.na(tb))
colSums(is.na(vis_tb))
colSums(is.na(transfers))

## replacing values in art tables with lookup values

art_code <-sort(unique(art$art_id))
art_regimen <- data.frame(
  code  = c("J05AE08","J05AE10","J05AF01","J05AF04","J05AF05","J05AF06","J05AF07","J05AF09","J05AG01","J05AG03","J05AR10","J05AX12"),
  ttt   = c("ATV","DRV","AZT","D4T","3TC","ABC","TDF","FTC","NVP","EFV","LPV","DTG")
)


#Match codes with corresponding regimens
art$art_id <- art_regimen$ttt[match(unlist(art$art_id),art_regimen$code)]


#Make art regimens as factor and order them
art$art_id <- as.factor(art$art_id)
art$art_id <- factor(art$art_id,
                     levels = c("ABC","ATV","AZT","TDF","3TC","FTC","EFV","NVP","LPV","DTG"
                                ,"DRV","D4T")
)

#Arrange art table data
art <- art%>%
  ## Remove observations with blank art_id
  filter(!is.na(art_id)) %>%
  
  arrange(patient,art_sd_dmy,!is.na(art_ed_dmy), art_id)

## Replace codes with values in outcomes

pat$outcome <- dplyr::recode(pat$outcome,"10"= "Deceased-HIV","11"= "Deceased-Unknow", "20" = "In-Care", "30" = "Trans-Out", "40" = "LTFU", "95" = "Unknown") 

########### 4. Convert columns classes/add columns  #############
#################################################################
#is.date <- function(x) inherits(x, 'Date') #Used to check of column is date


vis <- vis %>%
  mutate(across(.cols = everything(), .fns = as.character)) %>% #Convert all columns to character class
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
  mutate(across(.cols = everything(), .fns = as.character)) %>% #Convert all columns to character class
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
  mutate(across(.cols = everything(), .fns = as.character)) %>% #Convert all columns to character class
  mutate(
    lab_dmy = parse_date(lab_dmy,"%Y-%m-%d"),
    lab_v = parse_number(lab_v)
  ) %>%
  mutate(
    lab_dmy = as.Date(as.numeric(lab_dmy),origin ="1970-01-01")
  )

art <- art %>%
  mutate(across(.cols = art_sd_dmy:art_ed_dmy, .fns = as.character)) %>% #Convert date variables to character class
  mutate(
    art_sd_dmy = parse_date(art_sd_dmy,"%Y-%m-%d"),
    art_ed_dmy = parse_date(art_ed_dmy,"%Y-%m-%d"),
  )


tb <- tb %>%
  mutate(across(.cols = everything(), .fns = as.character)) %>% #Convert all columns to character class
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
  mutate(across(.cols = everything(), .fns = as.character)) %>% #Convert all columns to character class
  mutate(
    visit_dmy = parse_date(visit_dmy,"%Y-%m-%d"),
    next_visit_dmy = parse_date(next_visit_dmy,"%Y-%m-%d"),
  ) %>%
  mutate(
    visit_dmy = as.Date(as.numeric(visit_dmy),origin ="1970-01-01"),
    next_visit_dmy = as.Date(as.numeric(next_visit_dmy),origin ="1970-01-01")
  )

transfers <- transfers %>%
  mutate(across(.cols = everything(), .fns = as.character)) %>% #Convert all columns to character class
  mutate(
    transfer_dmy = parse_date(transfer_dmy,"%Y-%m-%d")
  ) %>%
  mutate(
    transfer_dmy = as.Date(as.numeric(transfer_dmy),origin ="1970-01-01")
  )

# transfers <- transfers %>%
#   filter(transfer_dmy >as.Date("2017-12-31") & facility_from == "CH KABINDA")


################ 5. Cleaning duplicate rows      #############
##############################################################
pat[duplicated(pat),]
pat<-unique(pat)

vis_duplicate <-vis[duplicated(vis),]
vis<-unique(vis)

lab_duplicate <-lab[duplicated(lab),]
lab<-unique(lab)
#TODO Cleaning of lab data for same exame encoded same date, different values...

art <-unique(art)

tb_duplicate <-tb[duplicated(tb),]
tb<-unique(tb)

vis_tb_duplicate <-vis_tb[duplicated(vis_tb),]
vis_tb<-unique(vis_tb)

transfers_duplicate <-transfers[duplicated(transfers),]
transfers<-unique(transfers)

################# 6. Merge tables                #################
##################################################################

patient_full <-left_join(pat,dem,"patient")

vis_data <- left_join(vis,dem,"patient") %>%
  left_join(pat,"patient")

art_data <-left_join(art,pat,"patient")%>%
  left_join(dem,"patient")

################# 7. Add variables/values labels    ##############
##################################################################
# labelled::val_labels(vis_data$gender)  <-c("1" = "Male", "2" = "Female", "95" = NA, "99" = NA)
# labelled::val_labels(vis_data$haart)   <-c("0" = "Not on ART", "1" = "on ART")
# labelled::val_labels(vis_data$method_in_art_named) <-c("0" = "New", "1" = "Transfer In", "88" = NA )


## Remove unused/intermediate objects from memory

rm(list = c("dem_raw","lab_raw","pat_raw","tb_raw","transfers_raw","vis_raw","vis_tb_raw"))



#############################################################################
##############      III. Analyzing data/presenting    #######################
#############################################################################

## Pivot data to display drugs start and end date per patient

art_data_pivot <- art_data %>%
  arrange(art_id) %>%
  pivot_wider(id_cols = c(patient,folder_number,art_sd_dmy,art_ed_dmy),names_from =art_id,values_from = art_id)

## Merge art data and patient demo data
art_patient_data <-art_data_pivot %>%
  left_join(pat,"patient") %>%
  
  ## Only select required variables
  ## @TODO : Make selection of drugs dynamic
  select(patient,folder_number,cohort,facility,birth_dmy,gender,method_into_art,outcome,outcome_dmy,art_sd_dmy,art_ed_dmy,`ABC`:`D4T`)%>%
  
  ## Add extra columns
  mutate(
    period_drug_start = quarter(art_sd_dmy, with_year = T),
    year_drug_start   = year(art_sd_dmy),
    #Calculate age at drug/regimen start in years and rounding it(ceiling())
    age_drug_start    = ceiling(interval(birth_dmy,art_sd_dmy) %/% days(1) / (365)),
    age_drug_st_cat   = case_when(
      age_drug_start >= 15 ~ "adult",
      age_drug_start  < 15 ~ "paediatric",
      TRUE ~ ""
    )
  ) %>%
  
  ## Reorder variables
  ## @TODO : Make selection of drugs dynamic
  select(folder_number,birth_dmy,age_drug_start,age_drug_st_cat,gender,art_sd_dmy,period_drug_start,year_drug_start,art_ed_dmy,ABC:D4T,everything())

## Combine drugs into regimens (merging drugs columns)
art_patient_data <- art_patient_data %>%
  unite(
    "drugs",`ABC`:`DRV`,remove = FALSE,na.rm = TRUE,sep = "/"  
  )%>%
  arrange(patient,art_sd_dmy,!is.na(art_ed_dmy))

###############################
vis_data_filt <- vis_data %>%
  filter(visit_dmy >=as.Date("2021-01-01") & visit_dmy <=as.Date("2021-12-31"))

vis_regimen <- sqldf::sqldf(
  "select vis.patient,vis.folder_number,vis.visit_dmy,vis.birth_dmy,vis.gender,art.art_sd_dmy,art.art_ed_dmy,art.drugs from vis_data_filt vis
    left join art_patient_data art on 
        vis.patient = art.patient and (art.art_sd_dmy <= vis.visit_dmy ) and ( (art.art_ed_dmy IS NULL) or art.art_ed_dmy > vis.visit_dmy  )"
) %>%
  ## Look at variables that are of interest to us
  select(patient,folder_number,visit_dmy,birth_dmy,gender,drugs,art_sd_dmy,art_ed_dmy ) %>%
  ## Exclude visits with no drugs/regimens
  filter(
    drugs !=""
  )

## Clear duplicate visit rows based on patient data (folder_number,dob,sex) and visit data

vis_data_reg_dup <- vis_regimen[duplicated(vis_regimen[,1:6]),]

vis_data_reg<-vis_regimen[!duplicated(vis_regimen[,1:6]),] %>%

## Pivot data to wider formats
pivot_wider(id_cols = c(patient,folder_number,visit_dmy,birth_dmy,gender),names_from = drugs,values_from = drugs) %>%

## Merge drugs columns
## @TODO : Make selection of drugs columns dynamic
  
unite(
  "regimen",`TDF/3TC/DTG`:`AZT/TDF`,remove = TRUE,na.rm = TRUE,sep = "/"  
) %>%
  
  mutate(
    counting = nchar(regimen)
  ) %>%
  mutate(
    ## Creating list with drugs in a regimen based on forward slash
   regimen_split =str_split(regimen,"/")
  ) %>%
  mutate(
    ## Remove drugs appearing as duplicate in regimens
    regimen_final = sapply(regimen_split,unique)
  ) %>%
  mutate(
    ## Format display of regimens back with forward slashes
    regimen_final = sapply(regimen_final,function(x) paste(x,collapse = "/"))
  )

# Cross check art_cur_reg_lvl_data
## Go back to how art_cur_reg is generating the data
#####
## Get current regimens
art_cur_reg <- art_patient_data %>%
  filter(is.na(art_ed_dmy) )
art_cur_reg <-  sqldf::sqldf("
  select d.patient,d.folder_number,d.birth_dmy,d.gender,d.outcome,d.outcome_dmy, d.art_sd_dmy,d.art_ed_dmy,d.drugs,d1.drugs as drugs1
    from art_cur_reg d
    left join art_cur_reg d1 
      on d.patient = d1.patient and (d1.art_sd_dmy <= d.art_sd_dmy )
      and d.art_ed_dmy IS NULL
      and ( (d1.art_ed_dmy IS NULL) or d1.art_ed_dmy > d.art_sd_dmy  )
      and d.drugs <> d1.drugs")


art_cur_reg <-art_cur_reg %>%
  pivot_wider(id_cols = c(patient,folder_number,birth_dmy,gender,outcome,outcome_dmy,art_sd_dmy,art_ed_dmy,drugs),names_from = drugs1,values_from = drugs1) %>%
  
  unite(
    "cur_regimen",9:ncol(.),remove = TRUE,na.rm = TRUE,sep = "/"  
  ) %>%
  
  arrange(desc(art_sd_dmy))

## Get unique regimens
art_cur_reg_data<-art_cur_reg[!duplicated(art_cur_reg[,1:6]),]


## Get Last VL data

lab_data_vl <-lab %>%
  filter((lab_id =="RNA") & !is.na(lab_v))

## Get last VL within a year of current art drugs start date
art_cur_reg_lvl<-sqldf::sqldf(
  "select data.*,vl.lab_dmy,vl.lab_v
    from art_cur_reg data
    left join lab_data_vl vl
      on data.patient = vl.patient
        and vl.lab_dmy between (data.art_sd_dmy -365) and (data.art_sd_dmy)"
)%>%
  rename(
    lastvl_date = lab_dmy,
    lastvl_value= lab_v
  ) %>%
  
  arrange(desc(lastvl_date))

## Unique regimen data
art_cur_reg_lvl_data<-art_cur_reg_lvl[!duplicated(art_cur_reg_lvl[,1:9]),] %>%
  arrange(patient,desc(art_sd_dmy))

test <- art_cur_reg_lvl_data[!duplicated(art_cur_reg_lvl_data[,1:6]),] %>%
  filter(outcome == "In-Care")

## NEXT STEPS
## 1. Make sure order of drugs in regimen is okay
## 2. Add age at regimen start and age at reporting period

