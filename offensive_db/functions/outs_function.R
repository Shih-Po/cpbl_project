
# 抓出局
outs_function = function(dummy_list, log_row) {
  
<<<<<<< HEAD
  rem_type <- dummy_list$rem_type
  base1 <- dummy_list$base1
  base2 <- dummy_list$base2
  base3 <- dummy_list$base3
  player <- dummy_list$player
  
  if (grepl("一出局", log_row) || grepl("一人出局", log_row)) {
    if(rem_type %in% c(1,4,7,10,13,16,19,22)){
      rem_type <- rem_type + 1 
    }
  }else if (grepl("兩出局", log_row) || grepl("兩人出局", log_row)) {
    if(rem_type %in% c(2,5,8,11,14,17,20,23)){
      rem_type <- rem_type + 1
    }
  }else if (grepl("三出局", log_row) || grepl("三人出局", log_row)){
    if(rem_type %in% c(3,6,9,12,15,18,21,24)){
      rem_type <- "NA"
      base1 <- "NA"
      base2 <- "NA"
      base3 <- "NA"
      player <- "NA"
    }
=======
  if (grepl("三出局", log_row) || grepl("三人出局", log_row) ) {
    dummy_list$rem_type <- "NA"
    dummy_list$base1 <- "NA"
    dummy_list$base2 <- "NA"
    dummy_list$base3 <- "NA"
    dummy_list$player <- substr(log_row, regexpr("：",log_row)-3, regexpr("：",log_row)-1 )  
>>>>>>> origin/master
  } else {
    # do nothing
  }
  
  dummy_list$rem_type <- rem_type
  dummy_list$base1 <- base1
  dummy_list$base2 <- base2
  dummy_list$base3 <- base3 
  dummy_list$player <- player
  
  # output dummy_list
  return(dummy_list)
}
