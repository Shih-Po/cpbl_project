cpbl_table <- read.csv(file("/Users/farmereric/Documents/Others/output042303.csv", encoding="big5"), header = TRUE, sep = ",",stringsAsFactors = FALSE)
# cpbl_player <- read.table(file("C:/Users/Student/Desktop/CPBL/batter.csv", encoding="UTF-8"), header = TRUE, sep = ",",stringsAsFactors = FALSE)
# player_library <- unique(cpbl_player$name)

total3 <- 0
#第三種狀況：一壘有人，二壘無人，二壘安打
#total"第幾狀況"_"幾出局"_"方向"_"壘包狀況"

#零出局
total3_0_other <- total3_0_other_OA <- total3_0_other_3rd <- total3_0_other_score <- 
total3_0_left <- total3_0_left_OA <- total3_0_left_3rd <- total3_0_left_score <- 
total3_0_center <- total3_0_center_OA <- total3_0_center_3rd <- total3_0_center_score <- 
total3_0_right <- total3_0_right_OA <- total3_0_right_3rd <- total3_0_right_score <- 0

total3_0_1_OA_player <- total3_0_1_3rd_player <- total3_0_1_score_player <- 
total3_0_2_OA_player <- total3_0_2_3rd_player <- total3_0_2_score_player <- 
total3_0_3_OA_player <- total3_0_3_3rd_player <- total3_0_3_score_player <- 
total3_0_4_OA_player <- total3_0_4_3rd_player <- total3_0_4_score_player <- NULL

#一出局
total3_1_other <- total3_1_other_OA <- total3_1_other_3rd <- total3_1_other_score <- 
total3_1_left <- total3_1_left_OA <- total3_1_left_3rd <- total3_1_left_score <- 
total3_1_center <- total3_1_center_OA <- total3_1_center_3rd <- total3_1_center_score <- 
total3_1_right <- total3_1_right_OA <- total3_1_right_3rd <- total3_1_right_score <- 0

total3_1_1_OA_player <- total3_1_1_3rd_player <- total3_1_1_score_player <- 
total3_1_2_OA_player <- total3_1_2_3rd_player <- total3_1_2_score_player <- 
total3_1_3_OA_player <- total3_1_3_3rd_player <- total3_1_3_score_player <- 
total3_1_4_OA_player <- total3_1_4_3rd_player <- total3_1_4_score_player <-NULL

#三出局
total3_2_other <- total3_2_other_OA <- total3_2_other_3rd <- total3_2_other_score <- 
total3_2_left <- total3_2_left_OA <- total3_2_left_3rd <- total3_2_left_score <- 
total3_2_center <- total3_2_center_OA <- total3_2_center_3rd <- total3_2_center_score <- 
total3_2_right <- total3_2_right_OA <- total3_2_right_3rd <- total3_2_right_score <- 0

total3_2_1_OA_player <- total3_2_1_3rd_player <- total3_2_1_score_player <- 
total3_2_2_OA_player <- total3_2_2_3rd_player <- total3_2_2_score_player <- 
total3_2_3_OA_player <- total3_2_3_3rd_player <- total3_2_3_score_player <- 
total3_2_4_OA_player <- total3_2_4_3rd_player <- total3_2_4_score_player <-NULL


for (i in 1:nrow(cpbl_table)){
  table_row <- cpbl_table[i,]
  
  base1 <- table_row$base1
  base2 <- table_row$base2
  base3 <- table_row$base3 
  result <- table_row$result 
  out <- table_row$out
  home <- table_row$home
  away <- table_row$away
  direction <- table_row$direction
  
  if (is.na(base1) !=TRUE && is.na(base2) == TRUE && is.na(base3) == TRUE &&
      table_row$result %in% c("二壘安打","深遠安打","上方安打")){
    
    total3 = total3 + 1 
    
    #零出局-內
    if (table_row$out == "零出局" && table_row$direction == "內"){
      total3_0_other = total3_0_other + 1 
      
      i = i + 1
      table_row_1 <- cpbl_table[i,]
      
            #跑者出局
          if(is.na(table_row_1$base3) == TRUE && 
             out != table_row_1$out){
            
            total3_0_other_OA <- total3_0_other_OA + 1 
            total3_0_1_OA_player <- c(total3_0_1_OA_player,base1)
            #total3_0_1_OA_player <- table(total3_0_1_OA_player)
            
            
            #跑者上三壘
          }else if(is.na(table_row_1$base3) != TRUE){
            
            total3_0_other_3rd = total3_0_other_3rd + 1
            total3_0_1_3rd_player <- c(total3_0_1_3rd_player,base1)
            #total3_0_1_3rd_player <- table(total3_0_1_3rd_player)
            
            #跑者得分
          }else if(is.na(table_row_1$base1) == TRUE && out == table_row_1$out){
            
            total3_0_other_score = total3_0_other_score + 1 
            total3_0_1_score_player <- c(total3_0_1_score_player,table_row$base1)
            #total3_0_1_score_player <- table(total3_0_1_score_player)
            
          }
    }
    
    #零出局-左
    if (table_row$out == "零出局" && table_row$direction == "左"){
      total3_0_left = total3_0_left + 1 
      
      
      i = i + 1
      table_row_1 <- cpbl_table[i,]
      
            #跑者出局
          if(is.na(table_row_1$base3) == TRUE && 
             out != table_row_1$out){
            
            total3_0_left_OA <- total3_0_left_OA + 1 
            total3_0_2_OA_player <- c(total3_0_2_OA_player,base1)
            #total3_0_2_OA_player <- table(total3_0_2_OA_player)
            
            
            #跑者上三壘
          }else if(is.na(table_row_1$base3) != TRUE){
            
            total3_0_left_3rd = total3_0_left_3rd + 1
            total3_0_2_3rd_player <- c(total3_0_2_3rd_player,base1)
            #total3_0_2_3rd_player <- table(total3_0_2_3rd_player)
            
            #跑者得分
          }else if(is.na(table_row_1$base1) == TRUE && out == table_row_1$out){
            
            total3_0_left_score = total3_0_left_score + 1 
            total3_0_2_score_player <- c(total3_0_2_score_player,table_row$base1)
            #total3_0_2_score_player <- table(total3_0_2_score_player)
            
          }
    }
    
    #零出局-中
    if (table_row$out == "零出局" && table_row$direction == "中"){
      total3_0_center = total3_0_center + 1 
      
      i = i + 1
      table_row_1 <- cpbl_table[i,]
      
            #跑者出局
          if(is.na(table_row_1$base3) == TRUE && 
             out != table_row_1$out){
            
            total3_0_center_OA <- total3_0_center_OA + 1 
            total3_0_3_OA_player <- c(total3_0_3_OA_player,base1)
            #total3_0_3_OA_player <- table(total3_0_3_OA_player)
            
            
            #跑者上三壘
          }else if(is.na(table_row_1$base3) != TRUE){
            
            total3_0_center_3rd = total3_0_center_3rd + 1
            total3_0_3_3rd_player <- c(total3_0_3_3rd_player,base1)
            #total3_0_3_3rd_player <- table(total3_0_3_3rd_player)
            
            #跑者得分
          }else if(is.na(table_row_1$base1) == TRUE && out == table_row_1$out){
            
            total3_0_center_score = total3_0_center_score + 1 
            total3_0_3_score_player <- c(total3_0_3_score_player,table_row$base1)
            #total3_0_3_score_player <- table(total3_0_3_score_player)
            
          }
    }
    
    #零出局-右
    if (table_row$out == "零出局" && table_row$direction == "右"){
      total3_0_right = total3_0_right + 1 
      
      i = i + 1
      table_row_1 <- cpbl_table[i,]
      
          #跑者出局
          if(is.na(table_row_1$base3) == TRUE && 
             out != table_row_1$out){
            
            total3_0_right_OA <- total3_0_right_OA + 1 
            total3_0_4_OA_player <- c(total3_0_4_OA_player,base1)
            #total3_0_4_OA_player <- table(total3_0_4_OA_player)
            
            #跑者上三壘
          }else if(is.na(table_row_1$base3) != TRUE){
            
            total3_0_right_3rd = total3_0_right_3rd + 1
            total3_0_4_3rd_player <- c(total3_0_4_3rd_player,base1)
            #total3_0_4_3rd_player <- table(total3_0_4_3rd_player)
            
            #跑者得分
          }else if(is.na(table_row_1$base1) == TRUE && out == table_row_1$out){
            
            total3_0_right_score = total3_0_right_score + 1 
            total3_0_4_score_player <- c(total3_0_4_score_player,table_row$base1)
            #total3_0_4_score_player <- table(total3_0_4_score_player)
            
          }
    }
    
    #ㄧ出局-內
    if (table_row$out == "一出局" && table_row$direction == "內"){
      total3_1_other = total3_1_other + 1 
      
      i = i + 1
      table_row_1 <- cpbl_table[i,]
      
          #跑者出局
          if(is.na(table_row_1$base3) == TRUE && 
             out != table_row_1$out){
            
            total3_1_other_OA <- total3_1_other_OA + 1 
            total3_1_1_OA_player <- c(total3_1_1_OA_player,base1)
            #total3_1_1_OA_player <- table(total3_1_1_OA_player)
            
            #跑者上三壘
          }else if(is.na(table_row_1$base3) != TRUE){
            
            total3_1_other_3rd = total3_1_other_3rd + 1
            total3_1_1_3rd_player <- c(total3_1_1_3rd_player,base1)
            #total3_1_1_3rd_player <- table(total3_1_1_3rd_player)
            
            #跑者得分
          }else if(is.na(table_row_1$base1) == TRUE && out == table_row_1$out){
            
            total3_1_other_score = total3_1_other_score + 1 
            total3_1_1_score_player <- c(total3_1_1_score_player,table_row$base1)
            #total3_1_1_score_player <- table(total3_1_1_score_player)
            
          }
    }
    
    #一出局-左
    if (table_row$out == "一出局" && table_row$direction == "左"){
      total3_1_left = total3_1_left + 1 
      
      i = i + 1
      table_row_1 <- cpbl_table[i,]
      
          #跑者出局
          if(is.na(table_row_1$base3) == TRUE && 
             out != table_row_1$out){
            
            total3_1_left_OA <- total3_1_left_OA + 1 
            total3_1_2_OA_player <- c(total3_1_2_OA_player,base1)
            #total3_1_2_OA_player <- table(total3_1_2_OA_player)
            
            #跑者上三壘
          }else if(is.na(table_row_1$base3) != TRUE){
            
            total3_1_left_3rd = total3_1_left_3rd + 1
            total3_1_2_3rd_player <- c(total3_1_2_3rd_player,base1)
            #total3_1_2_3rd_player <- table(total3_1_2_3rd_player)
            
            #跑者得分
          }else if(is.na(table_row_1$base1) == TRUE && out == table_row_1$out){
            
            total3_1_left_score = total3_1_left_score + 1 
            total3_1_2_score_player <- c(total3_1_2_score_player,table_row$base1)
            #total3_1_2_score_player <- table(total3_1_2_score_player)
            
          }
    }
    
    #一出局-中
    if (table_row$out == "一出局" && table_row$direction == "中"){
      total3_1_center = total3_1_center + 1 
      
      i = i + 1
      table_row_1 <- cpbl_table[i,]
      
          #跑者出局
          if(is.na(table_row_1$base3) == TRUE && 
             out != table_row_1$out){
            
            total3_1_center_OA <- total3_1_center_OA + 1 
            total3_1_3_OA_player <- c(total3_1_3_OA_player,base1)
            #total3_1_3_OA_player <- table(total3_1_3_OA_player)
            
            #跑者上三壘
          }else if(is.na(table_row_1$base3) != TRUE){
            
            total3_1_center_3rd = total3_1_center_3rd + 1
            total3_1_3_3rd_player <- c(total3_1_3_3rd_player,base1)
            #total3_1_3_3rd_player <- table(total3_1_3_3rd_player)
            
            #跑者得分
          }else if(is.na(table_row_1$base1) == TRUE && out == table_row_1$out){
            
            total3_1_center_score = total3_1_center_score + 1 
            total3_1_3_score_player <- c(total3_1_3_score_player,table_row$base1)
            #total3_1_3_score_player <- table(total3_1_3_score_player)
            
          }
    }
    
    #一出局-右
    if (table_row$out == "一出局" && table_row$direction == "右"){
      total3_1_right = total3_1_right + 1 
      
      i = i + 1
      table_row_1 <- cpbl_table[i,]
      
          #跑者出局
          if(is.na(table_row_1$base3) == TRUE && 
             out != table_row_1$out){
            
            total3_1_right_OA <- total3_1_right_OA + 1 
            total3_1_4_OA_player <- c(total3_1_4_OA_player,base1)
            #total3_1_4_OA_player <- table(total3_1_4_OA_player)
            
            #跑者上三壘
          }else if(is.na(table_row_1$base3) != TRUE){
            
            total3_1_right_3rd = total3_1_right_3rd + 1
            total3_1_4_3rd_player <- c(total3_1_4_3rd_player,base1)
            #total3_1_4_3rd_player <- table(total3_1_4_3rd_player)
            
            #跑者得分
          }else if(is.na(table_row_1$base1) == TRUE && out == table_row_1$out){
            
            total3_1_right_score = total3_1_right_score + 1 
            total3_1_4_score_player <- c(total3_1_4_score_player,table_row$base1)
            #total3_1_4_score_player <- table(total3_1_4_score_player)
            
          }
    }
    
    #二出局-內
    if (table_row$out == "二出局" && table_row$direction == "內"){
      total3_2_other = total3_2_other + 1 
      
      i = i + 1
      table_row_1 <- cpbl_table[i,]
      
          #跑者出局
          if(is.na(table_row_1$base3) == TRUE && 
             out != table_row_1$out){
            
            total3_2_other_OA <- total3_2_other_OA + 1 
            total3_2_1_OA_player <- c(total3_2_1_OA_player,base1)
            #total3_2_1_OA_player <- table(total3_2_1_OA_player)
            
            #跑者上三壘
          }else if(is.na(table_row_2$base3) != TRUE){
            
            total3_2_other_3rd = total3_2_other_3rd + 1
            total3_2_1_3rd_player <- c(total3_2_1_3rd_player,base1)
            #total3_2_1_3rd_player <- table(total3_2_1_3rd_player)
            
            #跑者得分
          }else if(table_row_2$home != home || table_row_2$away != away){
            
            total3_2_other_score = total3_2_other_score + 1 
            total3_2_1_score_player <- c(total3_2_1_score_player,table_row$base1)
            #total3_2_1_score_player <- table(total3_2_1_score_player)
            
          }
    }
    
    #二出局-左
    if (table_row$out == "二出局" && table_row$direction == "左"){
      total3_2_left = total3_2_left + 1 
      
      i = i + 1
      table_row_1 <- cpbl_table[i,]
      
          #跑者出局
          if(is.na(table_row_1$base3) == TRUE && 
             out != table_row_1$out){
            
            total3_2_left_OA <- total3_2_left_OA + 1 
            total3_2_2_OA_player <- c(total3_2_2_OA_player,base1)
            #total3_2_2_OA_player <- table(total3_2_2_OA_player)
            
            #跑者上三壘
          }else if(is.na(table_row_1$base3) != TRUE){
            
            total3_2_left_3rd = total3_2_left_3rd + 1
            total3_2_2_3rd_player <- c(total3_2_2_3rd_player,base1)
            #total3_2_2_3rd_player <- table(total3_2_2_3rd_player)
            
            #跑者得分
          }else if(is.na(table_row_1$base1) == TRUE && out == table_row_1$out){
            
            total3_2_left_score = total3_2_left_score + 1 
            total3_2_2_score_player <- c(total3_2_2_score_player,table_row$base1)
            #total3_2_2_score_player <- table(total3_2_2_score_player)
            
          }
    }
    
    #二出局-中
    if (table_row$out == "二出局" && table_row$direction == "中"){
      total3_2_center = total3_2_center + 1 
      
      i = i + 1
      table_row_1 <- cpbl_table[i,]
      
          #跑者出局
          if(is.na(table_row_1$base3) == TRUE && 
             out != table_row_1$out){
            
            total3_2_center_OA <- total3_2_center_OA + 1 
            total3_2_3_OA_player <- c(total3_2_3_OA_player,base1)
            #total3_2_3_OA_player <- table(total3_2_3_OA_player)
            
            #跑者上三壘
          }else if(is.na(table_row_1$base3) != TRUE){
            
            total3_2_center_3rd = total3_2_center_3rd + 1
            total3_2_3_3rd_player <- c(total3_2_3_3rd_player,base1)
            #total3_2_3_3rd_player <- table(total3_2_3_3rd_player)
            
            #跑者得分
          }else if(is.na(table_row_1$base1) == TRUE && out == table_row_1$out){
            
            total3_2_center_score = total3_2_center_score + 1 
            total3_2_3_score_player <- c(total3_2_3_score_player,table_row$base1)
            #total3_2_3_score_player <- table(total3_2_3_score_player)
            
          }
    }
    
    #二出局-右
    if (table_row$out == "二出局" && table_row$direction == "右"){
      total3_2_right = total3_2_right + 1 
      
      i = i + 1
      table_row_1 <- cpbl_table[i,]
      
          #跑者出局
          if(is.na(table_row_1$base3) == TRUE && 
             out != table_row_1$out){
            
            total3_2_right_OA <- total3_2_right_OA + 1 
            total3_2_4_OA_player <- c(total3_2_4_OA_player,base1)
            #total3_2_4_OA_player <- table(total3_2_4_OA_player)
            
            #跑者上三壘
          }else if(is.na(table_row_1$base3) != TRUE){
            
            total3_2_right_3rd = total3_2_right_3rd + 1
            total3_2_4_3rd_player <- c(total3_2_4_3rd_player,base1)
            #total3_2_4_3rd_player <- table(total3_2_4_3rd_player)
            
            #跑者得分
          }else if(is.na(table_row_1$base1) == TRUE && out == table_row_1$out){
            
            total3_2_right_score = total3_2_right_score + 1 
            total3_2_4_score_player <- c(total3_2_4_score_player,table_row$base1)
            #total3_2_4_score_player <- table(total3_2_4_score_player)
            
          }
    }
  }
}


#計算零人出局跑壘預期值
#內
prob3_0_1_3rd <- round(total3_0_other_3rd / total3_0_other, digits = 3)
prob3_0_1_score <- round(total3_0_other_score/ total3_0_other, digits = 3)
prob3_0_1_OA <- total3_0_other_OA / total3_0_other

#左
prob3_0_2_3rd <- round(total3_0_left_3rd / total3_0_left, digits = 3)
prob3_0_2_score <- round(total3_0_left_score/ total3_0_left, digits = 3)
prob3_0_2_OA <- total3_0_left_OA / total3_0_left

#中
prob3_0_3_3rd <- round(total3_0_center_3rd / total3_0_center, digits = 3)
prob3_0_3_score <- round(total3_0_center_score/ total3_0_center, digits = 3)
prob3_0_3_OA <- total3_0_center_OA / total3_0_center

#右
prob3_0_4_3rd <- round(total3_0_right_3rd / total3_0_right, digits = 3)
prob3_0_4_score <- round(total3_0_right_score/ total3_0_right, digits = 3)
prob3_0_4_OA <- total3_0_right_OA / total3_0_right

#計算一人出局跑壘預期值
#內
prob3_1_1_3rd <- round(total3_1_other_3rd / total3_1_other, digits = 3)
prob3_1_1_score <- round(total3_1_other_score/ total3_1_other, digits = 3)
prob3_1_1_OA <- total3_1_other_OA / total3_1_other

#左
prob3_1_2_3rd <- round(total3_1_left_3rd / total3_1_left, digits = 3)
prob3_1_2_score <- round(total3_1_left_score/ total3_1_left, digits = 3)
prob3_1_2_OA <- total3_1_left_OA / total3_1_left

#中
prob3_1_3_3rd <- round(total3_1_center_3rd / total3_1_center, digits = 3)
prob3_1_3_score <- round(total3_1_center_score/ total3_1_center, digits = 3)
prob3_1_3_OA <- total3_1_center_OA / total3_1_center

#右
prob3_1_4_3rd <- round(total3_1_right_3rd / total3_1_right, digits = 3)
prob3_1_4_score <- round(total3_1_right_score/ total3_1_right, digits = 3)
prob3_1_4_OA <- total3_1_right_OA / total3_1_right

#計算二人出局跑壘預期值
#內
prob3_2_1_3rd <- round(total3_2_other_3rd / total3_2_other, digits = 3)
prob3_2_1_score <- round(total3_2_other_score/ total3_2_other, digits = 3)
prob3_2_1_OA <- total3_2_other_OA / total3_2_other

#左
prob3_2_2_3rd <- round(total3_2_left_3rd / total3_2_left, digits = 3)
prob3_2_2_score <- round(total3_2_left_score/ total3_2_left, digits = 3)
prob3_2_2_OA <- total3_2_left_OA / total3_2_left

#中
prob3_2_3_3rd <- round(total3_2_center_3rd / total3_2_center, digits = 3)
prob3_2_3_score <- round(total3_2_center_score/ total3_2_center, digits = 3)
prob3_2_3_OA <- total3_2_center_OA / total3_2_center

#右
prob3_2_4_3rd <- round(total3_2_right_3rd / total3_2_right, digits = 3)
prob3_2_4_score <- round(total3_2_right_score/ total3_2_right, digits = 3)
prob3_2_4_OA <- total3_2_right_OA / total3_2_right

extrabase3_table = rbind(
#製成表格
list(Outs_Where = "0-other", Opp=total3_0_other, To3rd=prob3_0_1_3rd, Score=prob3_0_1_score, OA=prob3_0_1_OA),
list(Outs_Where = "0-left", Opp=total3_0_left, To3rd=prob3_0_2_3rd, Score=prob3_0_2_score, OA=prob3_0_2_OA),
list(Outs_Where = "0-center", Opp=total3_0_center, To3rd=prob3_0_3_3rd, Score=prob3_0_3_score, OA=prob3_0_3_OA),
list(Outs_Where = "0-right", Opp=total3_0_right, To3rd=prob3_0_4_3rd, Score=prob3_0_4_score, OA=prob3_0_4_OA),

list(Outs_Where = "1-other", Opp=total3_1_other, To3rd=prob3_1_1_3rd, Score=prob3_1_1_score, OA=prob3_1_1_OA),
list(Outs_Where = "1-left", Opp=total3_1_left, To3rd=prob3_1_2_3rd, Score=prob3_1_2_score, OA=prob3_1_2_OA),
list(Outs_Where = "1-center", Opp=total3_1_center, To3rd=prob3_1_3_3rd, Score=prob3_1_3_score, OA=prob3_1_3_OA),
list(Outs_Where = "1-right", Opp=total3_1_right, To3rd=prob3_1_4_3rd, Score=prob3_1_4_score, OA=prob3_1_4_OA),

list(Outs_Where = "2-other", Opp=total3_2_other, To3rd=prob3_2_1_3rd, Score=prob3_2_1_score, OA=prob3_2_1_OA),
list(Outs_Where = "2-left", Opp=total3_2_left, To3rd=prob3_2_2_3rd, Score=prob3_2_2_score, OA=prob3_2_2_OA),
list(Outs_Where = "2-center", Opp=total3_2_center, To3rd=prob3_2_3_3rd, Score=prob3_2_3_score, OA=prob3_2_3_OA),
list(Outs_Where = "2-right", Opp=total3_2_right, To3rd=prob3_2_4_3rd, Score=prob3_2_4_score, OA=prob3_2_4_OA)
)

View(extrabase3_table)