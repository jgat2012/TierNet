
knitr::opts_chunk$set(echo = TRUE)
#################################################
# Function : order drugs in regimen 
#################################################
## Order drugs function
order_drug_fn <- function(regimen){
  
  #Drugs in correct order
  regimen_list <- c("TDF","D4T","AZT","ABC","3TC","FTC","EFV","NVP","LPV","ATV","DTG","DRV")
  
  ##1. Split regimen into individual drugs
  regimen_init  <- unlist(str_split(regimen,"/"))
  ##2. Order drugs based on vector with ordered drugs
  regimen_order <- regimen_init[order(match(regimen_init,regimen_list))]
  ##3. Regroup ordered drugs to form regimen
  regimen_final <- paste(regimen_order,collapse = "/")
  regimen_final
}
