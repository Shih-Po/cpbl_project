# 需要人工校閱的條件, to_check <- 1
check_function = function(dummy_list, log_row) {
  
  # length greater than 40  
  if (nchar(log_row) > 40) {
    dummy_list$to_check <- 1
  }
  
  return(dummy_list)
}



history()

library(dplyr)
