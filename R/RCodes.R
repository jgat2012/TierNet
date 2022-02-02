### These codes are to be used on a need basis. 
###Does not necessarily belong to the analysis flow
###################################################

#Plot new initiations per facility per quarter###
#Dataframe needed: new_initiations
#
facility_names <- unique(new_initiations$facility)

for (fac in facility_names) {
  
  facility_init <- incidence2::incidence(
    x = new_initiations %>% filter(facility == fac),   # linelist is filtered to the current hospital
    date_index = visit_dmy,
    interval = "weeks", 
    groups = gender,
    na_as_group = TRUE
  )
  
  plot_fac <- plot(
    facility_init,
    fill = "gender",
    color = "black",
    title = stringr::str_glue("New ART initiations at {fac}")
  )
  
  # print the plot for the current hospital
  print(plot_fac)
}