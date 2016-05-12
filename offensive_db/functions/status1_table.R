cpbl_table <- read.csv(file("/Users/farmereric/Documents/Others/output042303.csv", encoding="big5"), header = TRUE, sep = ",",stringsAsFactors = FALSE)
# cpbl_player <- read.table(file("C:/Users/Student/Desktop/CPBL/batter.csv", encoding="UTF-8"), header = TRUE, sep = ",",stringsAsFactors = FALSE)
# player_library <- unique(cpbl_player$name)

total1 <- 0

#宣告變數
#零出局
total1_0_other <- total1_0_other_OA <- total1_0_other_2nd <- total1_0_other_3rd <- total1_0_other_score <- 0
total1_0_left <- total1_0_left_OA <- total1_0_left_2nd <- total1_0_left_3rd <- total1_0_left_score <- 0
total1_0_center <- total1_0_center_OA <- total1_0_center_2nd <- total1_0_center_3rd <- total1_0_center_score <- 0
total1_0_right <- total1_0_right_OA <- total1_0_right_2nd <- total1_0_right_3rd <- total1_0_right_score <- 0

total1_0_1_OA_player <- total1_0_1_2nd_player <- total1_0_1_3rd_player <- total1_0_1_score_player <- 
total1_0_2_OA_player <- total1_0_2_2nd_player <- total1_0_2_3rd_player <- total1_0_2_score_player <- 
total1_0_3_OA_player <- total1_0_3_2nd_player <- total1_0_3_3rd_player <- total1_0_3_score_player <- 
total1_0_4_OA_player <- total1_0_4_2nd_player <- total1_0_4_3rd_player <- total1_0_4_score_player <- NULL

#一出局
total1_1_other <- total1_1_other_OA <- total1_1_other_2nd <- total1_1_other_3rd <- total1_1_other_score <- 0
total1_1_left <- total1_1_left_OA <- total1_1_left_2nd <- total1_1_left_3rd <- total1_1_left_score <- 0
total1_1_center <- total1_1_center_OA <- total1_1_center_2nd <- total1_1_center_3rd <- total1_1_center_score <- 0
total1_1_right <- total1_1_right_OA <- total1_1_right_2nd <- total1_1_right_3rd <- total1_1_right_score <- 0

total1_1_1_OA_player <- total1_1_1_2nd_player <- total1_1_1_3rd_player <- total1_1_1_score_player <- 
total1_1_2_OA_player <- total1_1_2_2nd_player <- total1_1_2_3rd_player <- total1_1_2_score_player <- 
total1_1_3_OA_player <- total1_1_3_2nd_player <- total1_1_3_3rd_player <- total1_1_3_score_player <- 
total1_1_4_OA_player <- total1_1_4_2nd_player <- total1_1_4_3rd_player <- total1_1_4_score_player <- NULL

#兩出局
total1_2_other <- total1_2_other_OA <- total1_2_other_2nd <- total1_2_other_3rd <- total1_2_other_score <- 0
total1_2_left <- total1_2_left_OA <- total1_2_left_2nd <- total1_2_left_3rd <- total1_2_left_score <- 0
total1_2_center <- total1_2_center_OA <- total1_2_center_2nd <- total1_2_center_3rd <- total1_2_center_score <- 0
total1_2_right <- total1_2_right_OA <- total1_2_right_2nd <- total1_2_right_3rd <- total1_2_right_score <- 0

total1_2_1_OA_player <- total1_2_1_2nd_player <- total1_2_1_3rd_player <- total1_2_1_score_player <- 
total1_2_2_OA_player <- total1_2_2_2nd_player <- total1_2_2_3rd_player <- total1_2_2_score_player <- 
total1_2_3_OA_player <- total1_2_3_2nd_player <- total1_2_3_3rd_player <- total1_2_3_score_player <- 
total1_2_4_OA_player <- total1_2_4_2nd_player <- total1_2_4_3rd_player <- total1_2_4_score_player <- NULL


for (i in 1:nrow(cpbl_table)){
  table_row <- cpbl_table[i,]
  # j <- i + 1
  #一壘有人，二三壘無人，一壘安打狀況
  if (is.na(table_row$base1) !=TRUE && is.na(table_row$base2) == TRUE && is.na(table_row$base3) == TRUE &&
      table_row$result %in% c("一壘安打", "左外野安打", "中外野安打", "右外野安打", 
                              "穿越安打", "平飛安打", "內野安打", "中間方向安打", "德州安打")){
    
    #第一種情況總數量
    total1 = total1 + 1 
    
    if (table_row$out == "零出局" && table_row$direction == "內"){
      total1_0_other = total1_0_other + 1 
      i = i + 1
      table_row_1 <- cpbl_table[i,]
      
      if(is.na(table_row_1$base2) != TRUE) {
        
        total1_0_other_2nd = total1_0_other_2nd + 1 
        total1_0_1_2nd_player <- c(total1_0_1_2nd_player,table_row$base1)
        #total1_1_2_player <- table(total1_0_1_2nd_player)
        
        #print("跑者上二壘")
        
      }else if(is.na(table_row_1$base3) != TRUE){
        
        total1_0_other_3rd = total1_0_other_3rd + 1 
        total1_0_1_3rd_player <- c(total1_0_1_3rd_player,table_row$base1)
        #total1_1_3_player <- table(total1_0_1_3rd_player)
        
        #print("跑者上三壘")
        
      }else if(table_row$out == table_row_1$out){
        
        total1_0_other_score = total1_0_other_score + 1 
        total1_0_1_score_player <- c(total1_0_1_score_player,table_row$base1)
        #total1_1_4_player <- table(total1_0_1_score_player)
        
        #print("跑者回來得分")
        
      }else if(table_row$out != table_row_1$out){
        
        total1_0_other_OA = total1_0_other_OA + 1 
        total1_0_1_OA_player <- c(total1_0_1_OA_player,table_row$base1)
        #total1_1_1_player <- table(total1_0_1_OA_player)
        
        
        #print("跑者出局")
        
      }
      
    }else if(table_row$out == "零出局" && table_row$direction == "左"){
      
      total1_0_left = total1_0_left + 1
      i = i + 1
      table_row_1 <- cpbl_table[i,]
      
      if(is.na(table_row_1$base2) != TRUE){
        
        total1_0_left_2nd = total1_0_left_2nd + 1
        total1_0_2_2nd_player <- c(total1_0_2_2nd_player,table_row$base1)
        
        #total1_2_2_player <- table(total1_0_2_2nd_player)
        
        #print("跑者上二壘")
        
      }else if(is.na(table_row_1$base3) != TRUE){
        
        total1_0_left_3rd = total1_0_left_3rd + 1 
        total1_0_2_3rd_player <- c(total1_0_2_3rd_player,table_row$base1)
        #total1_2_3_player <- table(total1_0_2_3rd_player)
        
        #print("跑者上三壘")
        
      }else if(table_row$out == table_row_1$out){
        
        total1_0_left_score = total1_0_left_score + 1
        total1_0_2_score_player <- c(total1_0_2_score_player,table_row$base1)
        #total1_2_4_player <- table(total1_0_2_score_player)
        
        #print("跑者回來得分")
        
      }else if(table_row$out != table_row_1$out){
        
        total1_0_left_OA = total1_0_left_OA + 1 
        total1_0_2_OA_player <- c(total1_0_2_OA_player,table_row$base1)
        #total1_2_1_player <- table(total1_0_2_OA_player)
        
        #print("跑者出局")
        
      }
      
    }else if(table_row$out == "零出局" && table_row$direction == "中"){
      total1_0_center = total1_0_center + 1 
      i = i + 1
      table_row_1 <- cpbl_table[i,]
      
      if(is.na(table_row_1$base2) != TRUE){
        
        total1_0_center_2nd = total1_0_center_2nd + 1 
        total1_0_3_2nd_player <- c(total1_0_3_2nd_player,table_row$base1)
        #total1_3_2_player <- table(total1_0_3_2nd_player)
        
        #print("跑者上二壘")
        
      }else if(is.na(table_row_1$base3) != TRUE){
        
        total1_0_center_3rd = total1_0_center_3rd + 1
        total1_0_3_3rd_player <- c(total1_0_3_3rd_player,table_row$base1)
        #total1_3_3_player <- table(total1_0_3_3rd_player)
        
        #print("跑者上三壘")
        
      }else if(table_row$out == table_row_1$out){
        
        total1_0_center_score = total1_0_center_score + 1
        total1_0_3_score_player <- c(total1_0_3_score_player,table_row$base1)
        #total1_3_4_player <- table(total1_0_3_score_player)
        
        #print("跑者回來得分")
        
      }else if(table_row$out != table_row_1$out){
        
        total1_0_center_OA = total1_0_center_OA + 1
        total1_0_3_OA_player <- c(total1_0_3_OA_player,table_row$base1)
        #total1_3_1_player <- table(total1_0_3_OA_player)
        
        #print("跑者出局")
        
      }
      
    }else if(table_row$out == "零出局" && table_row$direction == "右"){
      
      total1_0_right = total1_0_right + 1
      i = i + 1
      table_row_1 <- cpbl_table[i,]
      
      if(is.na(table_row_1$base2) != TRUE){
        
        total1_0_right_2nd = total1_0_right_2nd + 1
        total1_0_4_2nd_player <- c(total1_0_4_2nd_player,table_row$base1)
        #total1_4_2player <- table(total1_0_4_2nd_player)
        
        #print("跑者上二壘")
        
      }else if(is.na(table_row_1$base3) != TRUE){
        
        total1_0_right_3rd = total1_0_right_3rd + 1  
        total1_0_4_3rd_player <- c(total1_0_4_3rd_player,table_row$base1)
        #total1_4_3_player <- table(total1_0_4_3rd_player)
        
        #print("跑者上三壘")
        
      }else if(table_row$out == table_row_1$out){
        
        total1_0_right_score = total1_0_right_score + 1
        total1_0_4_score_player <- c(total1_0_4_score_player,table_row$base1)
        #total1_4_4_player <- table(total1_0_4_score_player)
        
        #print("跑者回來得分")
        
      }else if(table_row$out != table_row_1$out){
        
        total1_0_right_OA = total1_0_right_OA + 1
        total1_0_4_OA_player <- c(total1_0_4_OA_player,table_row$base1)
        #total1_0_right_OA_players <- table(total1_0_4_OA_player)
        
        #print("跑者出局")
        
      }
    }
    
    if (table_row$out == "一出局" && table_row$direction == "內"){
      total1_1_other = total1_1_other + 1 
      i = i + 1
      table_row_1 <- cpbl_table[i,]
      
      if(is.na(table_row_1$base2) != TRUE){
        
        total1_1_other_2nd = total1_1_other_2nd + 1 
        total1_1_1_2nd_player <- c(total1_1_1_2nd_player,table_row$base1)
        #total2_1_2_player <- table(total1_1_1_2nd_player)
        
        #print("跑者上二壘")
        
      }else if(is.na(table_row_1$base3) != TRUE){
        
        total1_1_other_3rd = total1_1_other_3rd + 1 
        total1_1_1_3rd_player <- c(total1_1_1_3rd_player,table_row$base1)
        #total2_1_3_player <- table(total1_1_1_3rd_player)
        
        #print("跑者上三壘")
        
      }else if(table_row$out == table_row_1$out){
        
        total1_1_other_score = total1_1_other_score + 1 
        total1_1_1_score_player <- c(total1_1_1_score_player,table_row$base1)
        #total2_1_4_player <- table(total1_1_1_score_player)
        
        #print("跑者回來得分")
        
      }else if(table_row$out != table_row_1$out){
        
        total1_1_other_OA = total1_1_other_OA + 1 
        total1_1_1_OA_player <- c(total1_1_1_OA_player,table_row$base1)
        #total2_1_1_player <- table(total1_1_1_OA_player)
        
        
        #print("跑者出局")
        
      }
      
    }else if(table_row$out == "一出局" && table_row$direction == "左"){
      total1_1_left = total1_1_left + 1
      i = i + 1
      table_row_1 <- cpbl_table[i,]
      
      if(is.na(table_row_1$base2) != TRUE){
        
        total1_1_left_2nd = total1_1_left_2nd + 1
        total1_1_2_2nd_player <- c(total1_1_2_2nd_player,table_row$base1)
        #total2_2_2_player <- table(total1_1_2_2nd_player)
        
        #print("跑者上二壘")
        
      }else if(is.na(table_row_1$base3) != TRUE){
        
        total1_1_left_3rd = total1_1_left_3rd + 1 
        total1_1_2_3rd_player <- c(total1_1_2_3rd_player,table_row$base1)
        #total2_2_3_player <- table(total1_1_2_3rd_player)
        
        #print("跑者上三壘")
      }else if(table_row$out == table_row_1$out){
        
        total1_1_left_score = total1_1_left_score + 1
        total1_1_2_score_player <- c(total1_1_2_score_player,table_row$base1)
        #total2_2_4_player <- table(total1_1_2_score_player)
        
        #print("跑者回來得分")
        
      }else if(table_row$out != table_row_1$out){
        
        total1_1_left_OA = total1_1_left_OA + 1 
        total1_1_2_OA_player <- c(total1_1_2_OA_player,table_row$base1)
        #total2_2_1_player <- table(total1_1_2_OA_player)
        
        #print("跑者出局")
        
      }
      
    }else if(table_row$out == "一出局" && table_row$direction == "中"){
      total1_1_center = total1_1_center + 1 
      i = i + 1
      table_row_1 <- cpbl_table[i,]
      
      if(is.na(table_row_1$base2) != TRUE){
        
        total1_1_center_2nd = total1_1_center_2nd + 1
        total1_1_3_2nd_player <- c(total1_1_3_2nd_player,table_row$base1)
        #total2_3_2_player <- table(total1_1_3_2nd_player)
        
        #print("跑者上二壘")
      }else if(is.na(table_row_1$base3) != TRUE){
        
        total1_1_center_3rd = total1_1_center_3rd + 1
        total1_1_3_3rd_player <- c(total1_1_3_3rd_player,table_row$base1)
        #total2_3_3_player <- table(total1_1_3_3rd_player)
        
        #print("跑者上三壘")
      }else if(table_row$out == table_row_1$out){
        
        total1_1_center_score = total1_1_center_score + 1
        total1_1_3_score_player <- c(total1_1_3_score_player,table_row$base1)
        #total2_3_4_player <- table(total1_1_3_score_player)
        
        #print("跑者回來得分")
        
      }else if(table_row$out != table_row_1$out){
        
        total1_1_center_OA = total1_1_center_OA + 1
        total1_1_3_OA_player <- c(total1_1_3_OA_player,table_row$base1)
        #total2_3_1_player <- table(total1_1_3_OA_player)
        
        #print("跑者出局")
      }
      
    }else if(table_row$out == "一出局" && table_row$direction == "右"){
      total1_1_right = total1_1_right + 1
      i = i + 1
      table_row_1 <- cpbl_table[i,]
      
      if(is.na(table_row_1$base2) != TRUE){
        
        total1_1_right_2nd = total1_1_right_2nd + 1
        total1_1_4_2nd_player <- c(total1_1_4_2nd_player,table_row$base1)
        #total2_4_2player <- table(total1_1_4_2nd_player)
        
        #print("跑者上二壘")
      }else if(is.na(table_row_1$base3) != TRUE){
        
        total1_1_right_3rd = total1_1_right_3rd + 1
        total1_1_4_3rd_player <- c(total1_1_4_3rd_player,table_row$base1)
        #total2_4_3_player <- table(total1_1_4_3rd_player)
        
        #print("跑者上三壘")
      }else if(table_row$out == table_row_1$out){
        
        total1_1_right_score = total1_1_right_score + 1
        total1_1_4_score_player <- c(total1_1_4_score_player,table_row$base1)
        #total2_4_4_player <- table(total1_1_4_score_player)
        
        #print("跑者回來得分")
        
      }else if(table_row$out != table_row_1$out){
        
        total1_1_right_OA = total1_1_right_OA + 1
        total1_1_4_OA_player <- c(total1_1_4_OA_player,table_row$base1)
        #total2_4_1_player <- table(total1_1_4_OA_player)
        
        #print("跑者出局")
      }
    }
    
    if (table_row$out == "二出局" && table_row$direction == "內"){
      total1_2_other = total1_2_other + 1 
      i = i + 1
      table_row_1 <- cpbl_table[i,]
      
      if(is.na(table_row_1$base2) != TRUE){
        
        total1_2_other_2nd = total1_2_other_2nd + 1 
        total1_2_1_2nd_player <- c(total1_2_1_2nd_player,table_row$base1)
        #total3_1_2_player <- table(total1_2_1_2nd_player)
        
        #print("跑者上二壘")
      }else if(is.na(table_row_1$base3) != TRUE){
        
        total1_2_other_3rd = total1_2_other_3rd + 1 
        total1_2_1_3rd_player <- c(total1_2_1_3rd_player,table_row$base1)
        #total3_1_3_player <- table(total1_2_1_3rd_player)
        
        #print("跑者上三壘")
      }else if(table_row$out == table_row_1$out){
        
        total1_2_other_score = total1_2_other_score + 1 
        total1_2_1_score_player <- c(total1_2_1_score_player,table_row$base1)
        #total3_1_4_player <- table(total1_2_1_score_player)
        
        #print("跑者回來得分")
        
      }else if(table_row$out != table_row_1$out){
        
        total1_2_other_OA = total1_2_other_OA + 1 
        total1_2_1_OA_player <- c(total1_2_1_OA_player,table_row$base1)
        #total3_1_1_player <- table(total1_2_1_OA_player)
        
        
        #print("跑者出局")
      }
      
    }else if(table_row$out == "二出局" && table_row$direction == "左"){
      total1_2_left = total1_2_left + 1
      i = i + 1
      table_row_1 <- cpbl_table[i,]
      
      if(is.na(table_row_1$base2) != TRUE){
        
        total1_2_left_2nd = total1_2_left_2nd + 1
        total1_2_2_2nd_player <- c(total1_2_2_2nd_player,table_row$base1)
        #total3_2_2_player <- table(total1_2_2_2nd_player)
        
        #print("跑者上二壘")
      }else if(is.na(table_row_1$base3) != TRUE){
        
        total1_2_left_3rd = total1_2_left_3rd + 1 
        total1_2_2_3rd_player <- c(total1_2_2_3rd_player,table_row$base1)
        #total3_2_3_player <- table(total1_2_2_3rd_player)
        
        #print("跑者上三壘")
      }else if(table_row$out == table_row_1$out){
        
        total1_2_left_score = total1_2_left_score + 1
        total1_2_2_score_player <- c(total1_2_2_score_player,table_row$base1)
        #total3_2_4_player <- table(total1_2_2_score_player)
        
        #print("跑者回來得分")
        
      }else if(table_row$out != table_row_1$out){
        
        total1_2_left_OA = total1_2_left_OA + 1 
        total1_2_2_OA_player <- c(total1_2_2_OA_player,table_row$base1)
        #total3_2_1_player <- table(total1_2_2_OA_player)
        
        #print("跑者出局")
      }
      
    }else if(table_row$out == "二出局" && table_row$direction == "中"){
      total1_2_center = total1_2_center + 1 
      i = i + 1
      table_row_1 <- cpbl_table[i,]
      
      if(is.na(table_row_1$base2) != TRUE){
        
        total1_2_center_2nd = total1_2_center_2nd + 1 
        total1_2_3_2nd_player <- c(total1_2_3_2nd_player,table_row$base1)
        #total3_3_2_player <- table(total1_2_3_2nd_player)
        
        #print("跑者上二壘")
      }else if(is.na(table_row_1$base3) != TRUE){
        
        total1_2_center_3rd = total1_2_center_3rd + 1
        total1_2_3_3rd_player <- c(total1_2_3_3rd_player,table_row$base1)
        #total3_3_3_player <- table(total1_2_3_3rd_player)
        
        #print("跑者上三壘")
      }else if(table_row$out == table_row_1$out){
        
        total1_2_center_score = total1_2_center_score + 1
        total1_2_3_score_player <- c(total1_2_3_score_player,table_row$base1)
        #total3_3_4_player <- table(total1_2_3_score_player)
        
        #print("跑者回來得分")
        
      }else if(table_row$out != table_row_1$out){
        
        total1_2_center_OA = total1_2_center_OA + 1
        total1_2_3_OA_player <- c(total1_2_3_OA_player,table_row$base1)
        #total3_3_1_player <- table(total1_2_3_OA_player)
        
        #print("跑者出局")
      }
      
      
    }else if(table_row$out == "二出局" && table_row$direction == "右"){
      total1_2_right = total1_2_right + 1
      i = i + 1
      table_row_1 <- cpbl_table[i,]
      
      if(is.na(table_row_1$base2) != TRUE){
        
        total1_2_right_2nd = total1_2_right_2nd + 1
        total1_2_4_2nd_player <- c(total1_2_4_2nd_player,table_row$base1)
        #total3_4_2player <- table(total1_2_4_2nd_player)
        
        #print("跑者上二壘")
      }else if(is.na(table_row_1$base3) != TRUE){
        
        total1_2_right_3rd = total1_2_right_3rd + 1  
        total1_2_4_3rd_player <- c(total1_2_4_3rd_player,table_row$base1)
        #total3_4_3_player <- table(total1_2_4_3rd_player)
        
        #print("跑者上三壘")
      }else if(table_row$out == table_row_1$out){
        
        total1_2_right_score = total1_2_right_score + 1
        total1_2_4_score_player <- c(total1_2_4_score_player,table_row$base1)
        #total3_4_4_player <- table(total1_2_4_score_player)
        
        #print("跑者回來得分")
      }else if(table_row$out != table_row_1$out){
        
        total1_2_right_OA = total1_2_right_OA + 1
        total1_2_4_OA_player <- c(total1_2_4_OA_player,table_row$base1)
        #total3_4_1_player <- table(total1_2_4_OA_player)
        
        #print("跑者出局")
      }
    }
    
    
  }
  
}


#計算status1(零出局)，跑者到兩壘，到三壘，得分，出局的機率
#prob(第幾狀況)_(幾出局)_(方向)_(狀況)

#安打方向(內安)(other = 1)
prob1_0_1_OA <- round(total1_0_other_OA/total1_0_other,digits = 3)
prob1_0_1_2nd <- round(total1_0_other_2nd/total1_0_other,digits = 3)
prob1_0_1_3rd <- round(total1_0_other_3rd/total1_0_other,digits = 3)
prob1_0_1_score <- round(total1_0_other_score/total1_0_other,digits = 3)

#安打方向(左安)(left = 2)
prob1_0_2_OA <- round(total1_0_left_OA/total1_0_left,digits = 3)
prob1_0_2_2nd <- round(total1_0_left_2nd/total1_0_left,digits = 3)
prob1_0_2_3rd <- round(total1_0_left_3rd/total1_0_left,digits = 3)
prob1_0_2_score <- round(total1_0_left_score/total1_0_left,digits = 3)

#安打方向(中安)(center = 3)
prob1_0_3_OA <- round(total1_0_center_OA/total1_0_center,digits = 3)
prob1_0_3_2nd <- round(total1_0_center_2nd/total1_0_center,digits = 3)
prob1_0_3_3rd <- round(total1_0_center_3rd/total1_0_center,digits = 3)
prob1_0_3_score <- round(total1_0_center_score/total1_0_center,digits = 3)

#安打方向(右安)(right = 4)
prob1_0_4_OA <- round(total1_0_right_OA/total1_0_right,digits = 3)
prob1_0_4_2nd <- round(total1_0_right_2nd/total1_0_right,digits = 3)
prob1_0_4_3rd <- round(total1_0_right_3rd/total1_0_right,digits = 3)
prob1_0_4_score <- round(total1_0_right_score/total1_0_right,digits = 3)


#計算status1(一出局)，跑者到兩壘，到三壘，得分，出局的機率

#安打方向(內安)
prob1_1_1_OA <- round(total1_1_other_OA/total1_1_other,digits = 3)
prob1_1_1_2nd <- round(total1_1_other_2nd/total1_1_other,digits = 3)
prob1_1_1_3rd <- round(total1_1_other_3rd/total1_1_other,digits = 3)
prob1_1_1_score <- round(total1_1_other_score/total1_1_other,digits = 3)

#安打方向(左安)
prob1_1_2_OA <- round(total1_1_left_OA/total1_1_left,digits = 3)
prob1_1_2_2nd <- round(total1_1_left_2nd/total1_1_left,digits = 3)
prob1_1_2_3rd <- round(total1_1_left_3rd/total1_1_left,digits = 3)
prob1_1_2_score <- round(total1_1_left_score/total1_1_left,digits = 3)

#安打方向(中安)
prob1_1_3_OA <- round(total1_1_center_OA/total1_1_center,digits = 3)
prob1_1_3_2nd <- round(total1_1_center_2nd/total1_1_center,digits = 3)
prob1_1_3_3rd <- round(total1_1_center_3rd/total1_1_center,digits = 3)
prob1_1_3_score <- round(total1_1_center_score/total1_1_center,digits = 3)

#安打方向(右安)
prob1_1_4_OA <- round(total1_1_right_OA/total1_1_right,digits = 3)
prob1_1_4_2nd <- round(total1_1_right_2nd/total1_1_right,digits = 3)
prob1_1_4_3rd <- round(total1_1_right_3rd/total1_1_right,digits = 3)
prob1_1_4_score <- round(total1_1_right_score/total1_1_right,digits = 3)


#計算status1(二出局)，跑者到兩壘，到三壘，得分，出局的機率

#安打方向(內安)
prob1_2_1_OA <- round(total1_2_other_OA/total1_2_other,digits = 3)
prob1_2_1_2nd <- round(total1_2_other_2nd/total1_2_other,digits = 3)
prob1_2_1_3rd <- round(total1_2_other_3rd/total1_2_other,digits = 3)
prob1_2_1_score <- round(total1_2_other_score/total1_2_other,digits = 3)

#安打方向(左安)
prob1_2_2_OA <- round(total1_2_left_OA/total1_2_left,digits = 3)
prob1_2_2_2nd <- round(total1_2_left_2nd/total1_2_left,digits = 3)
prob1_2_2_3rd <- round(total1_2_left_3rd/total1_2_left,digits = 3)
prob1_2_2_score <- round(total1_2_left_score/total1_2_left,digits = 3)

#安打方向(中安)
prob1_2_3_OA <- round(total1_2_center_OA/total1_2_center,digits = 3)
prob1_2_3_2nd <- round(total1_2_center_2nd/total1_2_center,digits = 3)
prob1_2_3_3rd <- round(total1_2_center_3rd/total1_2_center,digits = 3)
prob1_2_3_score <- round(total1_2_center_score/total1_2_center,digits = 3)

#安打方向(右安)
prob1_2_4_OA <- round(total1_2_right_OA/total1_2_right,digits = 3)
prob1_2_4_2nd <- round(total1_2_right_2nd/total1_2_right,digits = 3)
prob1_2_4_3rd <- round(total1_2_right_3rd/total1_2_right,digits = 3)
prob1_2_4_score <- round(total1_2_right_score/total1_2_right,digits = 3)

#輸出表格
#0出局
extrabase1_table = rbind(
list(Outs_Where ="0-other" ,Opp= total1_0_other, To2nd =prob1_0_1_2nd, To3rd = prob1_0_1_3rd, Score= prob1_0_1_score, OA= prob1_0_1_OA ),
list(Outs_Where ="0-left" ,Opp= total1_0_left, To2nd = prob1_0_2_2nd , To3rd =prob1_0_2_3rd, Score= prob1_0_2_score, OA= prob1_0_2_OA),
list(Outs_Where ="0-center" ,Opp= total1_0_center,To2nd =prob1_0_3_2nd, To3rd = prob1_0_3_3rd, Score= prob1_0_3_score, OA= prob1_0_3_OA),
list(Outs_Where ="0-right" ,Opp= total1_0_right,To2nd = prob1_0_4_2nd, To3rd = prob1_0_4_3rd, Score= prob1_0_4_score, OA= prob1_0_4_OA),

#1出局
list(Outs_Where ="1-other" ,Opp= total1_1_other, To2nd = prob1_1_1_2nd, To3rd =prob1_1_1_3rd, Score= prob1_1_1_score, OA= prob1_1_1_OA),
list(Outs_Where ="1-left" ,Opp= total1_1_left, To2nd = prob1_1_2_2nd , To3rd =prob1_1_2_3rd, Score= prob1_1_2_score, OA= prob1_1_2_OA),
list(Outs_Where ="1-center" ,Opp= total1_1_center,To2nd = prob1_1_3_2nd, To3rd =prob1_1_3_3rd, Score= prob1_1_3_score, OA= prob1_1_3_OA),
list(Outs_Where ="1-right" ,Opp= total1_1_right,To2nd = prob1_1_4_2nd, To3rd = prob1_1_4_3rd, Score= prob1_1_4_score, OA= prob1_1_4_OA),

#2出局
list(Outs_Where ="2-other" ,Opp= total1_2_other, To2nd = prob1_2_1_2nd, To3rd =prob1_2_1_3rd, Score= prob1_2_1_score, OA= prob1_2_1_OA),
list(Outs_Where ="2-left" ,Opp= total1_2_left, To2nd = prob1_2_2_2nd , To3rd =prob1_2_2_3rd, Score= prob1_2_2_score, OA= prob1_2_2_OA),
list(Outs_Where ="2-center" ,Opp= total1_2_center,To2nd = prob1_2_3_2nd, To3rd =prob1_2_3_3rd, Score= prob1_2_3_score, OA= prob1_2_3_OA),
list(Outs_Where ="2-right" ,Opp= total1_2_right,To2nd = prob1_2_4_2nd, To3rd = prob1_2_4_3rd, Score= prob1_2_4_score, OA= prob1_2_4_OA)
)

View (extrabase1_table)
