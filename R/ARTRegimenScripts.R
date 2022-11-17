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
  sqldf,      # SQL queries
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
startReportPeriod <- as.Date("2021/01/01")
endReportPeriod   <- as.Date("2021/12/31")

###############################################################################
#################   I. Importing & Exploring Data         #####################
###################                                     #######################
###############################################################################
source(here("R", "TierDataCleaningKinshasa.R"),local = knitr::knit_global())

#############################################################################
##############      III. Analyzing data/presenting    #######################
#############################################################################



## Pivot data to display drugs start and end date per patient

art_data_pivot <- art_data %>%
  arrange(art_id) %>%
  pivot_wider(id_cols = c(patient,art_sd_dmy,art_ed_dmy),names_from =art_id,values_from = art_id)

## Merge art data and patient demo data

art_patient_data <-art_data_pivot %>%
  left_join(pat,"patient") %>%
  
  ## Make sure to move drugs variables to the end of dataset
  select(cohort:enrol_year,everything())%>%
  
  ## Only select required variables
  ## 20:ncol(.) as drugs start from 20th col till end
  select(patient,cohort,facility,birth_dmy,gender,method_into_art,haart_dmy,outcome,outcome_dmy,art_sd_dmy,art_ed_dmy,20:ncol(.))%>%
  
  ## Add extra columns
  mutate(
    period_drug_start = quarter(art_sd_dmy, with_year = T),
    year_drug_start   = year(art_sd_dmy),
    
    #Calculate age and age categories
    cur_age     = ceiling(interval(birth_dmy,endReportPeriod) %/% days(1) / (365)),
    cur_age_cat = case_when(
      cur_age   <5 ~ "<5",
      cur_age >=5 & cur_age <10 ~  "5-9",
      cur_age >=10 & cur_age <15 ~ "10-14",
      cur_age >=15 & cur_age <19 ~ "15-18",
      cur_age > 19 ~ ">18",
      TRUE ~ ""
    ),
    age_drug_start    = ceiling(interval(birth_dmy,art_sd_dmy) %/% days(1) / (365)),
    age_drug_st_cat   = case_when(
      age_drug_start >= 15 ~ "adult",
      age_drug_start  < 15 ~ "paediatric",
      TRUE ~ ""
    )
  ) %>%
  
  ## Reorder variables to put drugs at the end of dataset
  select(period_drug_start:age_drug_st_cat,everything()) %>%
  
  ## Combine drugs into regimens (merging drugs columns)
  unite(
    "drugs",17:ncol(.),remove = FALSE,na.rm = TRUE,sep = "/"  
  )%>%
  arrange(patient,art_sd_dmy,!is.na(art_ed_dmy))

########################### ART REGIMENS PER PATIENT PER VISIT #################
################################################################################

vis_data_filt <- vis_data %>%
  filter(visit_dmy >=startReportPeriod & visit_dmy <=endReportPeriod)

vis_regimen <- sqldf::sqldf(
  "select vis.patient,vis.facility,vis.folder_number,vis.visit_dmy,vis.birth_dmy,vis.gender,vis.haart_dmy,art.art_sd_dmy,art.art_ed_dmy,art.drugs 
    from vis_data_filt vis
    left join art_patient_data art on 
        vis.patient = art.patient and (art.art_sd_dmy <= vis.visit_dmy ) and ( (art.art_ed_dmy IS NULL) or art.art_ed_dmy > vis.visit_dmy  )"
) %>%
  ## Look at variables that are of interest to us
  select(patient,facility,folder_number,visit_dmy,birth_dmy,gender,haart_dmy,drugs,art_sd_dmy,art_ed_dmy ) %>%
  ## Exclude visits with no drugs/regimens
  filter(
    drugs !=""
  )

## Clear duplicate visit rows based on patient data (folder_number,facility,dob,sex) and visit data
vis_data_reg_dup <- vis_regimen[duplicated(vis_regimen[,1:8]),]

vis_data_reg<-vis_regimen[!duplicated(vis_regimen[,1:8]),] %>%

## Pivot to wide format to list drugs per visit per patient
pivot_wider(id_cols = c(patient,folder_number,visit_dmy,birth_dmy,gender),names_from = drugs,values_from = drugs) %>%

## Merge drugs columns
## @TODO : Make selection of drugs columns dynamic
  
unite(
  "regimen",6:ncol(.),remove = TRUE,na.rm = TRUE,sep = "/"  
) %>%
  
  mutate(
    ## Creating list with drugs 
   regimen_split =str_split(regimen,"/")
  ) %>%
  mutate(
    ## Remove drugs appearing as duplicate in regimens
    regimen_final = sapply(regimen_split,unique)
  ) %>%
  mutate(
    ## Format display of regimens back with forward slashes
    regimen_final = sapply(regimen_final,function(x) paste(x,collapse = "/"))
  )%>%
  mutate(
    counting = nchar(regimen_final)
  )

########################### ART CURRENT REGIMENS PER PATIENT   #################
################################################################################

## Get latest patient regimens
art_cur_reg <- art_patient_data %>%
  filter(is.na(art_ed_dmy))

## Get current regimens per patient, grouped per drugs started on same day
art_cur_reg <-  sqldf::sqldf("
  select d.patient,d.birth_dmy,d.gender,d.haart_dmy,d.outcome,d.outcome_dmy, d.art_sd_dmy,d.art_ed_dmy,d.drugs,d1.drugs as drugs1
    from art_cur_reg d
    left join art_cur_reg d1 
      on d.patient = d1.patient and (d1.art_sd_dmy <= d.art_sd_dmy )
      and d.art_ed_dmy IS NULL
      and ( (d1.art_ed_dmy IS NULL) or d1.art_ed_dmy > d.art_sd_dmy  )
      and d.drugs <> d1.drugs") %>%
  na_if("")%>%
  pivot_wider(id_cols = c(patient,birth_dmy,gender,haart_dmy,outcome,outcome_dmy,art_sd_dmy,art_ed_dmy,drugs),names_from = drugs1,values_from = drugs1) %>%
  
  unite(
    ## Individual drugs start from 9th column
    "cur_regimen",9:ncol(.),remove = TRUE,na.rm = TRUE,sep = "/"  
  ) %>%
  
  arrange(desc(art_sd_dmy))

## Get latest current regimens, one line per patient 
## Columns to be considered as unique: [patient,birth_dmy,gender,haart_dmy,outcome,outcome_dmy]
art_cur_reg_data<-art_cur_reg[!duplicated(art_cur_reg[,1:6]),]

## Get latest visits date for each of the patient by joining with visit table
#############################################################################
vis_art_cur_reg  <- art_cur_reg_data %>%
  
  ## Join cur_reg data with visit data
  left_join(vis,"patient") %>%
  
  ## Select all columns from art_cur_reg_data and add visit and next visit date
  select(1:ncol(art_cur_reg_data),visit_dmy,next_visit_dmy) %>%
  
  ##Order by patient and visit date (latest visit first)
  arrange(desc(patient),desc(visit_dmy))

## Remove duplicate based on patient data [patient, dob,sex,art_start_date,outcome,outcome_date]
vis_art_cur_reg_data <- vis_art_cur_reg[!duplicated(vis_art_cur_reg[,1:6]),]


## Get Last VL data
lab_data_vl <-lab %>%
  filter((lab_id =="RNA") & !is.na(lab_v))

## @TODO : NEED TO UPDATE and check VL Date from latest patient visit
#############!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

## Get last VL within a year of end of current reporting/observation period
art_cur_reg_lvl<-fn$sqldf(
  "select data.*,vl.lab_dmy,vl.lab_v
    from vis_art_cur_reg_data data
    left join lab_data_vl vl
      on data.patient = vl.patient
    and vl.lab_dmy between (data.visit_dmy -365) and (data.visit_dmy)",
  verbose = FALSE
)%>%
  rename(
    lastvl_date = lab_dmy,
    lastvl_value= lab_v
  ) %>%
  
  arrange(desc(patient),desc(lastvl_date))

## Remove duplicate patients and keep latest VL results
## unique columns : [patient, dob,sex,art_start_date,outcome,outcome_date]
art_cur_reg_lvl_data<-art_cur_reg_lvl[!duplicated(art_cur_reg_lvl[,1:6]),] %>%
  mutate(
    ## Calculate age and add age categories
    ## Current age calculated from dob to latest visit date
    latest_visit_dmy = visit_dmy,
    cur_age          = floor(interval(birth_dmy,latest_visit_dmy) %/% days(1) / (365)),
    cur_age_cat      = case_when(
            cur_age   <5 ~ "<5",
            cur_age >=5 & cur_age <10 ~  "5-9",
            cur_age >=10 & cur_age <15 ~ "10-14",
            cur_age >=15 & cur_age <20 ~ "15-19",
            cur_age > 19 ~ ">19",
            TRUE ~ ""
    ),
    age_type  = case_when(
      cur_age <=19  ~ "Child",
      cur_age >19 ~ "Adult",
      TRUE ~ ""
    )
  )%>%
  mutate(
    ## Convert age category to factor
    cur_age_cat = factor(cur_age_cat,levels = c("<5","5-9","10-14","15-19",">19")),
    ## Order drugs in regimen
    cur_reg_ord = map(cur_regimen,order_drug_fn)
    
  )%>%
  mutate(
    ## Calculate age at art start
    age_art_start     = floor(interval(birth_dmy,haart_dmy) %/% days(1) / (365)),
    age_art_start_cat = case_when(
        age_art_start   <5 ~ "<5",
        age_art_start >=5 & age_art_start <10 ~  "5-9",
        age_art_start >=10 & age_art_start <15 ~ "10-14",
        age_art_start >=15 & age_art_start <20 ~ "15-19",
        age_art_start > 19 ~ ">19",
        TRUE ~ ""
    ),
    ## Period on ART (date start on art with latest visit)
    age_on_art        = floor(interval(haart_dmy,latest_visit_dmy) %/% days(1) / (365)),
    age_on_art_cat    = case_when(
        age_on_art   < 1 ~ "<1",
        age_on_art   >=1 & age_on_art <=2 ~  "1-2",
        age_on_art   > 2 ~ ">2",
        TRUE ~ ""
    )
  ) %>%
  arrange(desc(patient),desc(art_sd_dmy)) %>%
  
  ## change order of variables
  select(patient,latest_visit_dmy,next_visit_dmy,birth_dmy,gender,haart_dmy,age_art_start,cur_age,age_on_art,age_art_start_cat,cur_age_cat,age_on_art_cat,cur_reg_ord,art_sd_dmy,lastvl_date,lastvl_value)

## Add age categories
######## FROM HERE GOING DOWN : NEED REWORK !!!!! ####################
## Get patient who are currently in care (latest outcome)
art_cur_reg_lvl_incare_data <- art_cur_reg_lvl_data[!duplicated(art_cur_reg_lvl_data[,1:5]),] %>%
  filter(outcome == 20)

##SUMMARIES
art_cur_reg_sum_age <-art_cur_reg_lvl_incare_data %>%
  group_by(cur_age_cat)%>%
  summarise(
    freq  = n(),
  ) %>%
  mutate(
    perc = round((freq/sum(freq))*100,2),
    cums = cumsum(freq),
    cump = round((cums/sum(freq))*100,2),
  ) 


art_cur_reg_sum_reg <- art_cur_reg_lvl_incare_data %>%
  group_by(cur_reg_ord)%>%
  summarise(
    freq  = n(),
  ) %>%
  mutate(
    perc = round((freq/sum(freq))*100,2),
    cums = cumsum(freq),
    cump = round((cums/sum(freq))*100,2),
  ) 
art_cur_reg_sum_reg


### Order drugs in regimen







## NEXT STEPS
## 1. Make sure order of drugs in regimen is okay
## 2. Add age at regimen start and age at reporting period

