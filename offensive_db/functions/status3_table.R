cpbl_table <- read.csv(file("/Users/farmereric/Documents/Others/output042303.csv", encoding="big5"), header = TRUE, sep = ",",stringsAsFactors = FALSE)
# cpbl_player <- read.table(file("C:/Users/Student/Desktop/CPBL/batter.csv", encoding="UTF-8"), header = TRUE, sep = ",",stringsAsFactors = FALSE)
# player_library <- unique(cpbl_player$name)

#第三種狀況：一壘有人，二壘無人，二壘安打

total3 <- total3_0_other <- total3_0_other_OA <- total3_0_other_3rd <- total3_0_other_score <- 
          total3_0_left <- total3_0_left_OA <- total3_0_left_3rd <- total3_0_left_score <- 
          total3_0_center <- total3_0_center_OA <- total3_0_center_3rd <- total3_0_center_score <- 
          total3_0_right <- total3_0_right_OA <- total3_0_right_3rd <- total3_0_right_score <- 
  
          total3_1_other <- total3_1_other_OA <- total3_1_other_3rd <- total3_1_other_score <- 
          total3_1_left <- total3_1_left_OA <- total3_1_left_3rd <- total3_1_left_score <- 
          total3_1_center <- total3_1_center_OA <- total3_1_center_3rd <- total3_1_center_score <- 
          total3_1_right <- total3_1_right_OA <- total3_1_right_3rd <- total3_1_right_score <-
          
          total3_2_other <- total3_2_other_OA <- total3_2_other_3rd <- total3_2_other_score <- 
          total3_2_left <- total3_2_left_OA <- total3_2_left_3rd <- total3_2_left_score <- 
          total3_2_center <- total3_2_center_OA <- total3_2_center_3rd <- total3_2_center_score <- 
          total3_2_right <- total3_2_right_OA <- total3_2_right_3rd <- total3_2_right_score <- 0

total3_0_other+ total3_0_left +total3_0_center + total3_0_right +total3_1_other+total3_1_left +total3_1_center + total3_1_right +total3_2_other +
  total3_2_left+total3_2_center+total3_2_right

total3_0_other_OA_players <- total3_0_other_3rd_player <- total3_0_other_score_player <- 
total3_0_left_OA_players <- total3_0_left_3rd_player <- total3_0_left_score_player <- 
total3_0_center_OA_players <- total3_0_center_3rd_player <- total3_0_center_score_player <- 
total3_0_right_OA_players <- total3_0_right_3rd_player <- total3_0_right_score_player <- 
  
total3_1_other_OA_players <- total3_1_other_3rd_player <- total3_1_other_score_player <- 
total3_1_left_OA_players <- total3_1_left_3rd_player <- total3_1_left_score_player <- 
total3_1_center_OA_players <- total3_1_center_3rd_player <- total3_1_center_score_player <- 
total3_1_right_OA_players <- total3_1_right_3rd_player <- total3_1_right_score_player <-

total3_2_other_OA_players <- total3_2_other_3rd_player <- total3_2_other_score_player <- 
total3_2_left_OA_players <- total3_2_left_3rd_player <- total3_2_left_score_player <- 
total3_2_center_OA_players <- total3_2_center_3rd_player <- total3_2_center_score_player <- 
total3_2_right_OA_players <- total3_2_right_3rd_player <- total3_2_right_score_player <-NULL


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
  
  if (is.na(base1) !=TRUE && is.na(base2) == TRUE &&
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
        total3_0_other_OA_player <- c(total3_0_other_OA_players,base1)
        total3_0_other_OA_players <- table(total3_0_other_OA_players)
        
        # print(table_row$base1)
        
        #跑者上三壘
      }else if(is.na(table_row_1$base3) != TRUE){
        
        total3_0_other_3rd = total3_0_other_3rd + 1
        total3_0_other_3rd_player <- c(total3_0_other_3rd_player,base1)
        total3_0_other_3rd_player <- table(total3_0_other_3rd_player)
        
        #跑者得分
      }else if(is.na(table_row_1$base1) == TRUE && out == table_row_1$out){
        
        total3_0_other_score = total3_0_other_score + 1 
        total3_0_other_score_player <- c(total3_0_other_score_player,table_row$base1)
        total3_0_other_score_player <- table(total3_0_other_score_player)
        
      }
    }
    
    #零出局-左
    if (table_row$out == "零出局" && table_row$direction == "左"){
      total3_0_left = total3_0_left + 1 
      
      #print(table_row)
      
      i = i + 1
      table_row_1 <- cpbl_table[i,]
      
      #跑者出局
      if(is.na(table_row_1$base3) == TRUE && 
         out != table_row_1$out){
        
        total3_0_left_OA <- total3_0_left_OA + 1 
        total3_0_left_OA_player <- c(total3_0_left_OA_players,base1)
        total3_0_left_OA_players <- table(total3_0_left_OA_players)
        
        # print(table_row$base1)
        
        #跑者上三壘
      }else if(is.na(table_row_1$base3) != TRUE){
        
        total3_0_left_3rd = total3_0_left_3rd + 1
        total3_0_left_3rd_player <- c(total3_0_left_3rd_player,base1)
        total3_0_left_3rd_player <- table(total3_0_left_3rd_player)
        
        #跑者得分
      }else if(is.na(table_row_1$base1) == TRUE && out == table_row_1$out){
        
        total3_0_left_score = total3_0_left_score + 1 
        total3_0_left_score_player <- c(total3_0_left_score_player,table_row$base1)
        total3_0_left_score_player <- table(total3_0_left_score_player)
        
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
        total3_0_center_OA_player <- c(total3_0_center_OA_players,base1)
        total3_0_center_OA_players <- table(total3_0_center_OA_players)
        
        # print(table_row$base1)
        
        #跑者上三壘
      }else if(is.na(table_row_1$base3) != TRUE){
        
        total3_0_center_3rd = total3_0_center_3rd + 1
        total3_0_center_3rd_player <- c(total3_0_center_3rd_player,base1)
        total3_0_center_3rd_player <- table(total3_0_center_3rd_player)
        
        #跑者得分
      }else if(is.na(table_row_1$base1) == TRUE && out == table_row_1$out){
        
        total3_0_center_score = total3_0_center_score + 1 
        total3_0_center_score_player <- c(total3_0_center_score_player,table_row$base1)
        total3_0_center_score_player <- table(total3_0_center_score_player)
        
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
        total3_0_right_OA_player <- c(total3_0_right_OA_players,base1)
        total3_0_right_OA_players <- table(total3_0_right_OA_players)
        
        # print(table_row$base1)
        
        #跑者上三壘
      }else if(is.na(table_row_1$base3) != TRUE){
        
        total3_0_right_3rd = total3_0_right_3rd + 1
        total3_0_right_3rd_player <- c(total3_0_right_3rd_player,base1)
        total3_0_right_3rd_player <- table(total3_0_right_3rd_player)
        
        #跑者得分
      }else if(is.na(table_row_1$base1) == TRUE && out == table_row_1$out){
        
        total3_0_right_score = total3_0_right_score + 1 
        total3_0_right_score_player <- c(total3_0_right_score_player,table_row$base1)
        total3_0_right_score_player <- table(total3_0_right_score_player)
        
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
        total3_1_other_OA_player <- c(total3_1_other_OA_players,base1)
        total3_1_other_OA_players <- table(total3_1_other_OA_players)
        
        # print(table_row$base1)
        
        #跑者上三壘
      }else if(is.na(table_row_1$base3) != TRUE){
        
        total3_1_other_3rd = total3_1_other_3rd + 1
        total3_1_other_3rd_player <- c(total3_1_other_3rd_player,base1)
        total3_1_other_3rd_player <- table(total3_1_other_3rd_player)
        
        #跑者得分
      }else if(is.na(table_row_1$base1) == TRUE && out == table_row_1$out){
        
        total3_1_other_score = total3_1_other_score + 1 
        total3_1_other_score_player <- c(total3_1_other_score_player,table_row$base1)
        total3_1_other_score_player <- table(total3_1_other_score_player)
        
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
        total3_1_left_OA_player <- c(total3_1_left_OA_players,base1)
        total3_1_left_OA_players <- table(total3_1_left_OA_players)
        
        # print(table_row$base1)
        
        #跑者上三壘
      }else if(is.na(table_row_1$base3) != TRUE){
        
        total3_1_left_3rd = total3_1_left_3rd + 1
        total3_1_left_3rd_player <- c(total3_1_left_3rd_player,base1)
        total3_1_left_3rd_player <- table(total3_1_left_3rd_player)
        
        #跑者得分
      }else if(is.na(table_row_1$base1) == TRUE && out == table_row_1$out){
        
        total3_1_left_score = total3_1_left_score + 1 
        total3_1_left_score_player <- c(total3_1_left_score_player,table_row$base1)
        total3_1_left_score_player <- table(total3_1_left_score_player)
        
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
        total3_1_center_OA_player <- c(total3_1_center_OA_players,base1)
        total3_1_center_OA_players <- table(total3_1_center_OA_players)
        
        # print(table_row$base1)
        
        #跑者上三壘
      }else if(is.na(table_row_1$base3) != TRUE){
        
        total3_1_center_3rd = total3_1_center_3rd + 1
        total3_1_center_3rd_player <- c(total3_1_center_3rd_player,base1)
        total3_1_center_3rd_player <- table(total3_1_center_3rd_player)
        
        #跑者得分
      }else if(is.na(table_row_1$base1) == TRUE && out == table_row_1$out){
        
        total3_1_center_score = total3_1_center_score + 1 
        total3_1_center_score_player <- c(total3_1_center_score_player,table_row$base1)
        total3_1_center_score_player <- table(total3_1_center_score_player)
        
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
        total3_1_right_OA_player <- c(total3_1_right_OA_players,base1)
        total3_1_right_OA_players <- table(total3_1_right_OA_players)
        
        # print(table_row$base1)
        
        #跑者上三壘
      }else if(is.na(table_row_1$base3) != TRUE){
        
        total3_1_right_3rd = total3_1_right_3rd + 1
        total3_1_right_3rd_player <- c(total3_1_right_3rd_player,base1)
        total3_1_right_3rd_player <- table(total3_1_right_3rd_player)
        
        #跑者得分
      }else if(is.na(table_row_1$base1) == TRUE && out == table_row_1$out){
        
        total3_1_right_score = total3_1_right_score + 1 
        total3_1_right_score_player <- c(total3_1_right_score_player,table_row$base1)
        total3_1_right_score_player <- table(total3_1_right_score_player)
        
      }
    }
    
    #二出局-內
    if (table_row$out == "二出局" && table_row$direction == "內"){
      total3_1_other = total3_1_other + 1 
      
      i = i + 1
      table_row_1 <- cpbl_table[i,]
      
      #跑者出局
      if(is.na(table_row_1$base3) == TRUE && 
         out != table_row_1$out){
        
        total3_2_other_OA <- total3_2_other_OA + 1 
        total3_2_other_OA_player <- c(total3_2_other_OA_players,base1)
        total3_2_other_OA_players <- table(total3_2_other_OA_players)
        
        # print(table_row$base1)
        
        #跑者上三壘
      }else if(is.na(table_row_2$base3) != TRUE){
        
        total3_2_other_3rd = total3_2_other_3rd + 1
        total3_2_other_3rd_player <- c(total3_2_other_3rd_player,base1)
        total3_2_other_3rd_player <- table(total3_2_other_3rd_player)
        
        #跑者得分
      }else if(table_row_2$home != home || table_row_2$away != away){
        
        total3_2_other_score = total3_2_other_score + 1 
        total3_2_other_score_player <- c(total3_2_other_score_player,table_row$base1)
        total3_2_other_score_player <- table(total3_2_other_score_player)
        
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
        total3_2_left_OA_player <- c(total3_2_left_OA_players,base1)
        total3_2_left_OA_players <- table(total3_2_left_OA_players)
        
        # print(table_row$base1)
        
        #跑者上三壘
      }else if(is.na(table_row_1$base3) != TRUE){
        
        total3_2_left_3rd = total3_2_left_3rd + 1
        total3_2_left_3rd_player <- c(total3_2_left_3rd_player,base1)
        total3_2_left_3rd_player <- table(total3_2_left_3rd_player)
        
        #跑者得分
      }else if(is.na(table_row_1$base1) == TRUE && out == table_row_1$out){
        
        total3_2_left_score = total3_2_left_score + 1 
        total3_2_left_score_player <- c(total3_2_left_score_player,table_row$base1)
        total3_2_left_score_player <- table(total3_2_left_score_player)
        
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
        total3_2_center_OA_player <- c(total3_2_center_OA_players,base1)
        total3_2_center_OA_players <- table(total3_2_center_OA_players)
        
        # print(table_row$base1)
        
        #跑者上三壘
      }else if(is.na(table_row_1$base3) != TRUE){
        
        total3_2_center_3rd = total3_2_center_3rd + 1
        total3_2_center_3rd_player <- c(total3_2_center_3rd_player,base1)
        total3_2_center_3rd_player <- table(total3_2_center_3rd_player)
        
        #跑者得分
      }else if(is.na(table_row_1$base1) == TRUE && out == table_row_1$out){
        
        total3_2_center_score = total3_2_center_score + 1 
        total3_2_center_score_player <- c(total3_2_center_score_player,table_row$base1)
        total3_2_center_score_player <- table(total3_2_center_score_player)
        
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
        total3_2_right_OA_player <- c(total3_2_right_OA_players,base1)
        total3_2_right_OA_players <- table(total3_2_right_OA_players)
        
        # print(table_row$base1)
        
        #跑者上三壘
      }else if(is.na(table_row_1$base3) != TRUE){
        
        total3_2_right_3rd = total3_2_right_3rd + 1
        total3_2_right_3rd_player <- c(total3_2_right_3rd_player,base1)
        total3_2_right_3rd_player <- table(total3_2_right_3rd_player)
        
        #跑者得分
      }else if(is.na(table_row_1$base1) == TRUE && out == table_row_1$out){
        
        total3_2_right_score = total3_2_right_score + 1 
        total3_2_right_score_player <- c(total3_2_right_score_player,table_row$base1)
        total3_2_right_score_player <- table(total3_2_right_score_player)
        
      }
    }
    # else{
    #   i = i + 1
    #   table_row_1 <- cpbl_table[i,]
    #   print (table_row)
    #   print (table_row_1)
    # }
    
  }
}


    
      
      
      
 


#計算零人出局跑壘預期值
#內
opp0other <- total3_0_other
prob0other_3rd <- round(total3_0_other_3rd / total3_0_other, digits = 3)
prob0other_score <- round(total3_0_other_score/ total3_0_other, digits = 3)
prob0other_OA <- total3_0_other_OA / total3_0_other

#左
opp0left <- total3_0_left
prob0left_3rd <- round(total3_0_left_3rd / total3_0_left, digits = 3)
prob0left_score <- round(total3_0_left_score/ total3_0_left, digits = 3)
prob0left_OA <- total3_0_left_OA / total3_0_left

#中
opp0center <- total3_0_center
prob0center_3rd <- round(total3_0_center_3rd / total3_0_center, digits = 3)
prob0center_score <- round(total3_0_center_score/ total3_0_center, digits = 3)
prob0center_OA <- total3_0_center_OA / total3_0_center

#右
opp0right <- total3_0_right
prob0right_3rd <- round(total3_0_right_3rd / total3_0_right, digits = 3)
prob0right_score <- round(total3_0_right_score/ total3_0_right, digits = 3)
prob0right_OA <- total3_0_right_OA / total3_0_right

#計算一人出局跑壘預期值
#內
opp1other <- total3_1_other
prob1other_3rd <- round(total3_1_other_3rd / total3_1_other, digits = 3)
prob1other_score <- round(total3_1_other_score/ total3_1_other, digits = 3)
prob1other_OA <- total3_1_other_OA / total3_1_other

#左
opp1left <- total3_1_left
prob1left_3rd <- round(total3_1_left_3rd / total3_1_left, digits = 3)
prob1left_score <- round(total3_1_left_score/ total3_1_left, digits = 3)
prob1left_OA <- total3_1_left_OA / total3_1_left

#中
opp1center <- total3_1_center
prob1center_3rd <- round(total3_1_center_3rd / total3_1_center, digits = 3)
prob1center_score <- round(total3_1_center_score/ total3_1_center, digits = 3)
prob1center_OA <- total3_1_center_OA / total3_1_center

#右
opp1right <- total3_1_right
prob1right_3rd <- round(total3_1_right_3rd / total3_1_right, digits = 3)
prob1right_score <- round(total3_1_right_score/ total3_1_right, digits = 3)
prob1right_OA <- total3_1_right_OA / total3_1_right

#計算二人出局跑壘預期值
#內
opp2other <- total3_2_other
prob2other_3rd <- round(total3_2_other_3rd / total3_2_other, digits = 3)
prob2other_score <- round(total3_2_other_score/ total3_2_other, digits = 3)
prob2other_OA <- total3_2_other_OA / total3_2_other

#左
opp2left <- total3_2_left
prob2left_3rd <- total3_2_left_3rd / total3_2_left
prob2left_score <- round(total3_2_left_score/ total3_2_left, digits = 3)
prob2left_OA <- total3_2_left_OA / total3_2_left

#中
opp2center <- total3_2_center
prob2center_3rd <- round(total3_2_center_3rd / total3_2_center, digits = 3)
prob2center_score <- round(total3_2_center_score/ total3_2_center, digits = 3)
prob2center_OA <- total3_2_center_OA / total3_2_center

#右
opp2right <- total3_2_right
prob2right_3rd <- round(total3_2_right_3rd / total3_2_right, digits = 3)
prob2right_score <- round(total3_2_right_score/ total3_2_right, digits = 3)
prob2right_OA <- total3_2_right_OA / total3_2_right

#製成表格
other0 <- list(Outs_Where = "0-other", Opp=opp0other, To3rd=prob0other_3rd, Score=prob0other_score, OA=prob0other_OA)
left0 <- list(Outs_Where = "0-left", Opp=opp0left, To3rd=prob0left_3rd, Score=prob0left_score, OA=prob0left_OA)
center0 <- list(Outs_Where = "0-center", Opp=opp0center, To3rd=prob0center_3rd, Score=prob0center_score, OA=prob0center_OA)
right0 <- list(Outs_Where = "0-other", Opp=opp0right, To3rd=prob0right_3rd, Score=prob0right_score, OA=prob0right_OA)

other1 <- list(Outs_Where = "1-other", Opp=opp1other, To3rd=prob1other_3rd, Score=prob1other_score, OA=prob1other_OA)
left1 <- list(Outs_Where = "1-left", Opp=opp1left, To3rd=prob1left_3rd, Score=prob1left_score, OA=prob1left_OA)
center1 <- list(Outs_Where = "1-center", Opp=opp1center, To3rd=prob1center_3rd, Score=prob1center_score, OA=prob1center_OA)
right1 <- list(Outs_Where = "1-other", Opp=opp1right, To3rd=prob1right_3rd, Score=prob1right_score, OA=prob1right_OA)

other2 <- list(Outs_Where = "2-other", Opp=opp2other, To3rd=prob2other_3rd, Score=prob2other_score, OA=prob2other_OA)
left2 <- list(Outs_Where = "2-left", Opp=opp2left, To3rd=prob2left_3rd, Score=prob2left_score, OA=prob2left_OA)
center2 <- list(Outs_Where = "2-center", Opp=opp2center, To3rd=prob2center_3rd, Score=prob2center_score, OA=prob2center_OA)
right2 <- list(Outs_Where = "2-other", Opp=opp2right, To3rd=prob2right_3rd, Score=prob2right_score, OA=prob2right_OA)

extrabase3_table = rbind(other0, left0, center0, right0,other1, left1, center1, right1,other2, left2, center2, right2)
View(extrabase3_table_test)
