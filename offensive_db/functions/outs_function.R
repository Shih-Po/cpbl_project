# 抓出局
outs_function = function(dummy_list, log_row) {
  
  if (grepl("三出局", log_row)) {
    dummy_list$rem_type <- "NA"
    dummy_list$base1 <- "NA"
    dummy_list$base2 <- "NA"
    dummy_list$base3 <- "NA"
    dummy_list$player <- "NA" 
  } else {
    # do nothing
  }
  
  
  # output dummy_list
  return(dummy_list)
}
