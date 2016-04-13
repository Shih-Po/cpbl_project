# 抓安打
hit_function = function(dummy_list, log_row) {
  
  # 展開 dummy_list 的值
  rem_type <- dummy_list$rem_type
  base1 <- dummy_list$base1
  base2 <- dummy_list$base2
  base3 <- dummy_list$base3
  player <- dummy_list$player

  if (grepl("安打", log_row)) {
    # hit_function start work
    # catch player name
    player <- substr (log_row, regexpr("：",log_row)-3, regexpr("：",log_row)-1 )
    
    # 依情境辦事
    # 一壘安打
    if (rem_type == 1) {
      base1 <- player
      # empty to 1B
      rem_type <- rem_type + 3 
      
    } else if (rem_type == 4) {
      base2 <- base1
      base1 <- player
      # 1B to 1B_2B
      rem_type <- rem_type + 9 
      
    } else if (rem_type == 7) {
      base3 <- base2
      base1 <- player
      # 2B to 1B_3B
      rem_type <- rem_type + 9
      
    } else if (rem_type == 10) {
      # 得一分
      base3 <- "NA"
      base1 <- player
      # 3B to 1B
      rem_type <- rem_type - 9 
      
    } else if (rem_type == 13) {
      base3 <- base2
      base2 <- base1
      base1 <- player
      # 1B_2B to 1B_2B_3B
      rem_type <- rem_type + 9 
      
    } else if (rem_type == 16) {
      # 得一分
      base3 <- "NA"
      base2 <- base1
      base1 <- player
      # 1B_3B to 1B_2B
      rem_type <- rem_type - 3
      
    } else if (rem_type == 19) {
      base3 <- base2
      base2 <- "NA"
      base1 <- player
      # 2B_3B to 1B_3B
      rem_type <- rem_type - 3
      
    } else if (rem_type == 22) {
      base3 <- base2
      base2 <- base1
      base1 <- player
      # 1B_2B_3B to 1B_2B_3B, rem_type no change
    }
  } else {
    # dummy_list 不變
  }
  
  # 將區域變數復原為 dummy_list 傳出
  dummy_list$rem_type <- rem_type
  dummy_list$base1 <- base1
  dummy_list$base2 <- base2
  dummy_list$base3 <- base3 
  dummy_list$player <- player
  return(dummy_list)
}