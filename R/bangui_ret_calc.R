################################################################################
################## 0. LOAD PACKAGES AND SET UP ################################# 
################################################################################
pacman::p_load(
  rio,        # importing data  
  here,       # relative file pathways
  readxl,     # read excel
  writexl,    # Read/write excel
  openxlsx,   # Read/Write to Excel,
  janitor,    # data cleaning and tables
  lubridate,  # working with dates
  tidyverse,  # data management and visualization,
  gtsummary,  # summary statistics and tests,
  rstatix,    # summary statistics and statistical tests
  ggplot2,
  sqldf,
  data.table, # Create and manage data tables
  fuzzyjoin,  # Join datasets with inexact matching
  httr,       # Useful for working with URLS
  assertthat, # Checks pre and post conditions of functions
  keyring     # Allows storage and retrieval of pwds in OS
)

# use here from the here package
here <- here::here
# use clean_names from the janitor package
clean_names <- janitor::clean_names

# use here from the here package
here <- here::here
# use clean_names from the janitor package
clean_names <- janitor::clean_names

data_folder <- "data/"
image_folder <- "images/"
output_folder <- "output/"

report_year <- 2023

last_day_report_year <-as.Date(sprintf('%s-12-31', report_year))


## Import data
data_hiv_raw       <- readxl::read_xlsx(here(data_folder,"Bangui_Export_HIV_20240105.xlsx"),sheet="Export_HIV_20240105", 
                                    skip = 6)
  
data_tb_raw       <- readxl::read_xlsx(here(data_folder,"Bangui_Export_TB_20240301.xlsx"),sheet="Export_TB_2024322134114")
  
my_data_hiv <- data_hiv_raw %>% 
  janitor::clean_names() %>%
  
  mutate(
   cohort_period  = as.factor(cohort_period),
   age_at_enr_cat = factor(
       case_when(
         age_a_linitiation_tarv >=0 & age_a_linitiation_tarv <= 4 ~"0-4y",
         age_a_linitiation_tarv >=5 & age_a_linitiation_tarv <= 14 ~"5-14y",
         age_a_linitiation_tarv >=15 ~ "15y+",
         TRUE ~ "Unknown age"
     ),levels = c("0-4y","5-14y","15y+","Unknown age")
   ),
   method_into_art= factor(tolower(methode_d_initiation_au_tarv)),
   age_at_eor_cat = factor(
     case_when(
       age_actuel >=0 & age_actuel <= 4 ~"0-4y",
       age_actuel >=5 & age_actuel <= 14 ~"5-14y",
       age_actuel >=15 ~ "15y+",
       TRUE ~ "Unknown age"
     ),levels = c("0-4y","5-14y","15y+","Unknown age")
   ),
   last_visit_date= date_prescription_dernier_tarv,
   next_app_date  = as.Date(date_prochain_rdv_tarv),
   year_art_start = format(date_dinitiation_tarv,"%Y"),
   
   tarv_ant = if_else(
     tarv_anterieur == "None (naïve)","naive",
     "not naive"
   ),
   
   days_missed = last_day_report_year - next_app_date,
   fa = factor(tolower(fa)),
   issue_suivi_tarv = factor(tolower(issue_suivi_tarv)),
   
   cur_outcome = factor(
     case_when(
       #If outcome is dead or transfer out, assign outcome
       issue_suivi_tarv %in% c("tfo","rip") ~ issue_suivi_tarv,
       #Otherwise calculate outcome based on no of missed days 
       days_missed > 90 ~ "ltf",
       #Patients with no last visit
       is.na(days_missed) & is.na(last_visit_date) ~ "ltf",
       #Otherwise, blank == active
       TRUE ~ "active"
       
     )
   ),
   lastvl_value = derniere_charge_virale,
   lastvl_date  = as.Date(date_derniere_charge_virale),
   lastvl_valid = factor(
     if_else(#Last VL within 1year == valid(0)
       (
           last_day_report_year - lastvl_date) <=365,
         0,
         1
       )
     ),
   lastcd4_value = dernier_cd4,
   lastcd4_date  = date_dernier_cd4,
   tpt_start_date = as.Date(date_de_debut_tpi),
   tpt_start_year = format(tpt_start_date,"%Y"),
   tpt_end_date   = as.Date(date_darret_tpi),
   tpt_outcome    = factor(resultat_tpi)
       
  ) %>%
  
  dplyr::select(numero_dossier,sexe,age_a_linitiation_tarv,age_actuel,
                cohort_period,date_dinitiation_tarv,method_into_art,year_art_start,age_a_linitiation_tarv,age_at_enr_cat,age_at_eor_cat,
                cd4_a_l_initiation,issue_suivi_tarv,fa,cur_outcome,date_issue,last_visit_date,next_app_date,days_missed,tarv_ant,
                date_debut_seconde_ligne,lastcd4_value,lastcd4_date,lastvl_value,lastvl_date,lastvl_valid,tpt_start_date,tpt_start_year,tpt_end_date,tpt_outcome) %>%
  
  #Only get patients on ART
  dplyr::filter(!is.na(date_dinitiation_tarv)) %>%
  
  arrange(days_missed)
  
##TB Data
my_data_tb <- data_tb_raw %>% 
  janitor::clean_names() %>%
  mutate(
    tb_ttt_type       = factor(tolower(type_de_traitement)),
    tb_category       = factor(categorie_de_patient),
    tb_classification = factor(classification_tb),
    tb_ttt_start_date = as.Date(date_dinitiation_du_traitement),
    tb_ttt_start_year = format(tb_ttt_start_date,"%Y"),
    tb_outcome        = factor(issue_finale),
    tb_outcome_date   = as.Date(date_issue),
    facility          = factor(tolower(centre_de_sante)),
    sex               = factor(tolower(sexe)),
    age_at_enr_cat    = factor(
      case_when(
        age >=0 & age <= 4 ~"0-4y",
        age >=5 & age <= 14 ~"5-14y",
        age >=15 ~ "15y+",
        TRUE ~ "Unknown age"
      ),levels = c("0-4y","5-14y","15y+","Unknown age")
    )
  ) %>%
  select(numero_dossier,sex,age_at_enr_cat,tb_ttt_type,tb_category,tb_classification,tb_ttt_start_date,tb_ttt_start_year,
         tb_outcome,tb_outcome_date,facility)
  
#BASELINE DATA
################################
base_data <- my_data_hiv %>%
  
  dplyr::filter(
    #Only filter naive patients & only new patients
    tarv_ant == "naive" & method_into_art == "new" &
    #Started ART in reporting year
      year_art_start == report_year
    ) %>% 
  group_by(age_at_enr_cat) %>%
  summarise(
    art_naive          = n(),
    baseline_cd4       = sum(!is.na(cd4_a_l_initiation),na.rm = TRUE),
    baseline_cd4_lt200 = sum(cd4_a_l_initiation < 200,na.rm = TRUE),
  ) %>%
  #Pivot data
  pivot_longer(cols = c("art_naive","baseline_cd4","baseline_cd4_lt200")) %>%
  
  pivot_wider(
    names_from = age_at_enr_cat,values_from = value
  ) %>%
  adorn_totals(where = c("col"))
  
##ACTIVE COHORT
#########################################
active_cohort <- my_data_hiv %>%
  tabyl(cur_outcome,age_at_eor_cat) %>%
  adorn_totals(where=c("row","col"))

## Active cohort by ART Line
active_cohort_line <- my_data_hiv %>%
  #Only active patients
  dplyr::filter(cur_outcome =="active") %>%
  #Get patients on second line
  mutate(
    art_line = case_when(
      !is.na(date_debut_seconde_ligne) ~"2eme ligne",
      TRUE ~"1ere ligne"
    )
  )%>%
  tabyl(art_line,age_at_eor_cat) %>%
  adorn_totals(where=c("row","col"))

##VL and CD4 OPD
################################
cd4_vl_data <- my_data_hiv %>%
  group_by(age_at_eor_cat) %>%
  summarise(
    tot_cv        = sum(format(lastvl_date,"%Y")  ==report_year & !is.na(lastvl_value),na.rm = TRUE),
    tot_cv_lt1000 = sum(format(lastvl_date,"%Y")  ==report_year & lastvl_value <1000,na.rm = TRUE),
    tot_cd4       = sum(format(lastcd4_date,"%Y") ==report_year & !is.na(lastcd4_value),na.rm = TRUE),
    tot_cd4_lt200 = sum(format(lastcd4_date,"%Y") ==report_year & lastcd4_value <200,na.rm = TRUE)
  ) %>%
  #Pivot data
  pivot_longer(cols = starts_with("tot_")) %>%
  
  pivot_wider(
    names_from = age_at_eor_cat,values_from = value
  ) %>%
  adorn_totals(where = c("col"))


##HIV OUTCOME
################################
ret_data <- my_data_hiv %>%
  #Only filter naive patients
  dplyr::filter(tarv_ant == "naive" & method_into_art == "new") %>%
  group_by(cohort_period,age_at_enr_cat) %>%
  summarise(
    art_init      = n(),
    transfer_out  = sum((issue_suivi_tarv =="tfo"),na.rm = TRUE),
    dead          = sum((issue_suivi_tarv =="rip"),na.rm = TRUE),
    lost_to_fu    = sum((issue_suivi_tarv =="ltf"),na.rm = TRUE),
    ret_in_care   = sum((cur_outcome =="active"),na.rm = TRUE),
    `ret_in_care(%)`= round(ret_in_care/(art_init - transfer_out),2)*100,
    vl_due        = sum((cur_outcome =="active"),na.rm = TRUE), #Assuming that VL is done every 12M routinely
    
    vl_done       = sum((lastvl_valid ==0) & !is.na(lastvl_value),na.rm = TRUE),
    vl_supp       = sum((lastvl_valid ==0) & lastvl_value <1000,na.rm = TRUE)
  ) %>%
  
  #Pivot data
  pivot_longer(cols = c("art_init","transfer_out","dead","lost_to_fu","ret_in_care","ret_in_care(%)","vl_due","vl_done","vl_supp")) %>%
  
  pivot_wider(
    names_from = age_at_enr_cat,values_from = value
  ) %>%
  
  ## Add new columns
  mutate(
    category = factor(
        case_when(
        name == "art_init" & cohort_period =="1Year" ~ "ART naïve initiated one year before this reporting year",
        name == "art_init" & cohort_period =="2Year" ~ "ART naïve initiated two years before this reporting year",
        name == "art_init" & cohort_period =="3Year" ~ "ART naïve initiated three years before this reporting year",
        name == "art_init" & cohort_period =="4Year" ~ "ART naïve initiated four years before this reporting year",
        name == "transfer_out" & cohort_period =="1Year" ~ "Transferred out (at 12m)",
        name == "transfer_out" & cohort_period =="2Year" ~ "Transferred out (at 24m)",
        name == "transfer_out" & cohort_period =="3Year" ~ "Transferred out (at 36m)",
        name == "transfer_out" & cohort_period =="4Year" ~ "Transferred out (at 48m)",
        name == "dead" & cohort_period =="1Year" ~ "Dead (at 12m)",
        name == "dead" & cohort_period =="2Year" ~ "Dead (at 24m)",
        name == "dead" & cohort_period =="3Year" ~ "Dead (at 36m)",
        name == "dead" & cohort_period =="4Year" ~ "Dead (at 48m)",
        name == "lost_to_fu" & cohort_period =="1Year" ~ "Lost to FU (at 12m)",
        name == "lost_to_fu" & cohort_period =="2Year" ~ "Lost to FU (at 24m)",
        name == "lost_to_fu" & cohort_period =="3Year" ~ "Lost to FU (at 36m)",
        name == "lost_to_fu" & cohort_period =="4Year" ~ "Lost to FU (at 48m)",
        name == "ret_in_care" & cohort_period =="1Year" ~ "Retained in care (at 12m)",
        name == "ret_in_care" & cohort_period =="2Year" ~ "Retained in care (at 24m)",
        name == "ret_in_care" & cohort_period =="3Year" ~ "Retained in care (at 36m)",
        name == "ret_in_care" & cohort_period =="4Year" ~ "Retained in care (at 48m)",
        name == "vl_due" & cohort_period =="1Year" ~ "VL due (at 12m)",
        name == "vl_due" & cohort_period =="2Year" ~ "VL due (at 24m)",
        name == "vl_due" & cohort_period =="3Year" ~ "VL due (at 36m)",
        name == "vl_due" & cohort_period =="4Year" ~ "VL due (at 48m)",
        name == "vl_done" & cohort_period =="1Year" ~ "VL completed (at 12m)",
        name == "vl_done" & cohort_period =="2Year" ~ "VL completed (at 24m)",
        name == "vl_done" & cohort_period =="3Year" ~ "VL completed (at 36m)",
        name == "vl_done" & cohort_period =="4Year" ~ "VL completed (at 48m)",
        name == "vl_supp" & cohort_period =="1Year" ~ "VL 0 - 999 copies/mL (at 12m)",
        name == "vl_supp" & cohort_period =="2Year" ~ "VL 0 - 999 copies/mL (at 24m)",
        name == "vl_supp" & cohort_period =="3Year" ~ "VL 0 - 999 copies/mL (at 36m)",
        name == "vl_supp" & cohort_period =="4Year" ~ "VL 0 - 999 copies/mL (at 48m)",
        TRUE ~ NA_character_
      ),
      levels = c(
        "ART naïve initiated one year before this reporting year",
        "ART naïve initiated two years before this reporting year",
        "ART naïve initiated three years before this reporting year",
        "ART naïve initiated four years before this reporting year",
        "Transferred out (at 12m)",
        "Transferred out (at 24m)",
        "Transferred out (at 36m)",
        "Transferred out (at 48m)",
        "Dead (at 12m)",
        "Dead (at 24m)",
        "Dead (at 36m)",
        "Dead (at 48m)",
        "Lost to FU (at 12m)",
        "Lost to FU (at 24m)",
        "Lost to FU (at 36m)",
        "Lost to FU (at 48m)",
        "Retained in care (at 12m)",
        "Retained in care (at 24m)",
        "Retained in care (at 36m)",
        "Retained in care (at 48m)",
        "VL due (at 12m)",
        "VL due (at 24m)",
        "VL due (at 36m)",
        "VL due (at 48m)",
        "VL completed (at 12m)",
        "VL completed (at 24m)",
        "VL completed (at 36m)",
        "VL completed (at 48m)",
        "VL 0 - 999 copies/mL (at 12m)",
        "VL 0 - 999 copies/mL (at 24m)",
        "VL 0 - 999 copies/mL (at 36m)",
        "VL 0 - 999 copies/mL (at 48m)"
      )
    )
  ) %>%
  
  ##Filter out data not needed
  dplyr::filter(!is.na(category)) %>%
  
  ## Add total columns
  adorn_totals(where = "col") %>%
  
  ##Order columns and rows
  dplyr::select(category,everything(),-c("cohort_period","name")) %>%
  arrange(category)

## TPT
###############################
tpt_data <- my_data_hiv %>%
  tabyl(tpt_start_year,age_at_eor_cat)%>%
  adorn_totals(where = c("row","col"))



################## TB ######################
############################################
## Started on treatment
tb_start_data <- my_data_tb %>%
  #Exclude Presomptive TB
  #filter(tb_ttt_type != "tb présomptive") %>%
  filter(tb_ttt_start_year == report_year) %>%
  tabyl(tb_classification,age_at_enr_cat)%>%
  adorn_totals(where = c("row","col")) %>%
  filter(Total != 0)


################################################################################
##########################     V. GENERATE OUTPUT    ###########################
################################################################################

wb <- createWorkbook("BanguiDataNoteBook")
## Add a worksheets
openxlsx::addWorksheet(wb, "Init_ARV_CD4_Initiation", gridLines = TRUE)
openxlsx::addWorksheet(wb, "FileActive", gridLines = TRUE)
openxlsx::addWorksheet(wb, "CV_CD4", gridLines = TRUE)
openxlsx::addWorksheet(wb, "Issue_VIH", gridLines = TRUE)
openxlsx::addWorksheet(wb, "IPT_VIH", gridLines = TRUE)
openxlsx::addWorksheet(wb, "Debut_tx_TB", gridLines = TRUE)
## write dataframes to worksheets
openxlsx::writeData(wb, sheet = "Init_ARV_CD4_Initiation", base_data,borderStyle = "thin",withFilter = TRUE)
openxlsx::writeData(wb, sheet = "FileActive", active_cohort_line,borderStyle = "thin",withFilter = TRUE)
openxlsx::writeData(wb, sheet = "CV_CD4", cd4_vl_data,borderStyle = "thin",withFilter = TRUE)
openxlsx::writeData(wb, sheet = "Issue_VIH", ret_data,borderStyle = "thin",withFilter = TRUE)
openxlsx::writeData(wb, sheet = "IPT_VIH", tpt_data,borderStyle = "thin",withFilter = TRUE)
openxlsx::writeData(wb, sheet = "Debut_tx_TB", tb_start_data,borderStyle = "thin",withFilter = TRUE)

openxlsx::saveWorkbook(wb, here("output/bangui_tmar_vih_tb.xlsx"), overwrite = TRUE)



  