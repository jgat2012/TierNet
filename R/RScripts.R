
knitr::opts_chunk$set(echo = TRUE)

#################################################
# Trim all columns from a data frame and returned
# trimmed data frame
#################################################
trim_data_columns <-function(data){
  for(i in 1:ncol(data)) {       # for-loop over columns
    data[ , i] <- trimws(data[ , i])
  }
  return (data)
}

