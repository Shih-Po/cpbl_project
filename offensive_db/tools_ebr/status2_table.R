cpbl_table <- read.csv(file("/Users/farmereric/Documents/Others/output042303.csv", encoding="big5"), header = TRUE, sep = ",",stringsAsFactors = FALSE)
# cpbl_player <- read.table(file("C:/Users/Student/Desktop/CPBL/batter.csv", encoding="UTF-8"), header = TRUE, sep = ",",stringsAsFactors = FALSE)
# player_library <- unique(cpbl_player$name)

total2 <- 0

#零出局
total2_0_other <- total2_0_other_OA <- total2_0_other_3rd <- total2_0_other_score <- 0
total2_0_left <- total2_0_left_OA <- total2_0_left_3rd <- total2_0_left_score <- 0
total2_0_center <- total2_0_center_OA <- total2_0_center_3rd <- total2_0_center_score <- 0
total2_0_right <- total2_0_right_OA <- total2_0_right_3rd <- total2_0_right_score <- 0

total2_0_1_OA_player <- total2_0_1_3rd_player <- total2_0_1_score_player <-  
total2_0_2_OA_player <- total2_0_2_3rd_player <- total2_0_2_score_player <-  
total2_0_3_OA_player <- total2_0_3_3rd_player <- total2_0_3_score_player <-  
total2_0_4_OA_player <- total2_0_4_3rd_player <- total2_0_4_score_player <-  NULL
    
#一出局
total2_1_other <- total2_1_other_OA <- total2_1_other_3rd <- total2_1_other_score <-  0
total2_1_left <- total2_1_left_OA <- total2_1_left_3rd <- total2_1_left_score <-  0
total2_1_center <- total2_1_center_OA <- total2_1_center_3rd <- total2_1_center_score <-  0
total2_1_right <- total2_1_right_OA <- total2_1_right_3rd <- total2_1_right_score <-  0

total2_1_1_OA_player <- total2_1_1_3rd_player <- total2_1_1_score_player <- 
total2_1_2_OA_player <- total2_1_2_3rd_player <- total2_1_2_score_player <- 
total2_1_3_OA_player <- total2_1_3_3rd_player <- total2_1_3_score_player <- 
total2_1_4_OA_player <- total2_1_4_3rd_player <- total2_1_4_score_player <- NULL

#兩出局
total2_2_other <- total2_2_other_OA <- total2_2_other_3rd <- total2_2_other_score <-  0
total2_2_left <- total2_2_left_OA <- total2_2_left_3rd <- total2_2_left_score <-  0
total2_2_center <- total2_2_center_OA <- total2_2_center_3rd <- total2_2_center_score <-  0
total2_2_right <- total2_2_right_OA <- total2_2_right_3rd <- total2_2_right_score <-  0

total2_2_1_OA_player <- total2_2_1_3rd_player <- total2_2_1_score_player <- 
total2_2_2_OA_player <- total2_2_2_3rd_player <- total2_2_2_score_player <- 
total2_2_3_OA_player <- total2_2_3_3rd_player <- total2_2_3_score_player <- 
total2_2_4_OA_player <- total2_2_4_3rd_player <- total2_2_4_score_player <-NULL


for (i in 1:nrow(cpbl_table)){
  table_row <- cpbl_table[i,]
  # j <- i + 1
  if (is.na(table_row$base1) == TRUE && is.na(table_row$base2) != TRUE && is.na(table_row$base3) == TRUE && 
      table_row$result %in% c("一壘安打", "左外野安打", "中外野安打", "右外野安打", 
                              "穿越安打", "平飛安打", "內野安打", "中間方向安打", "德州安打")){

    total2 = total2 + 1 
    
    if (table_row$out == "零出局" && table_row$direction == "內"){
      total2_0_other = total2_0_other + 1 
      
      i = i + 1
      table_row_1 <- cpbl_table[i,]
      
            #跑者上三壘
           if(is.na(table_row_1$base3) != TRUE){
             
            total2_0_other_3rd = total2_0_other_3rd + 1 
            total2_0_1_3rd_player <- c(total2_0_1_3rd_player,table_row$base2)
            #total1_1_2_player <- table(total2_0_1_3rd_player)
            
            #跑者得分
          }else if(is.na(table_row_1$base3) == TRUE && table_row$out == table_row_1$out){
            
            total2_0_other_score = total2_0_other_score + 1 
            total2_0_1_score_player <- c(total2_0_1_score_player,table_row$base2)
            #total1_1_3_player <- table(total2_0_1_score_player)
           
            #跑者出局
          }else if(table_row$out != table_row_1$out){
            
            total2_0_other_OA = total2_0_other_OA + 1 
            total2_0_1_OA_player <- c(total2_0_1_OA_player,table_row$base2)
            #total1_1_1_player <- table(total2_0_1_OA_player)
            
          }
      
    }else if(table_row$out == "零出局" && table_row$direction == "左"){
      total2_0_left = total2_0_left + 1
      
      i = i + 1
      table_row_1 <- cpbl_table[i,]
      
            #跑者上三壘
          if(is.na(table_row_1$base3) != TRUE){
            
            total2_0_left_3rd = total2_0_left_3rd + 1 
            total2_0_2_3rd_player <- c(total2_0_2_3rd_player,table_row$base2)
            #total1_1_2_player <- table(total2_0_2_3rd_player)
            
            #跑者得分
          }else if(is.na(table_row_1$base3) == TRUE && table_row$out == table_row_1$out){
            
            total2_0_left_score = total2_0_left_score + 1 
            total2_0_2_score_player <- c(total2_0_2_score_player,table_row$base2)
            #total1_1_3_player <- table(total2_0_2_score_player)
            
            #跑者出局
          }else if(table_row$out != table_row_1$out){
            
            total2_0_left_OA = total2_0_left_OA + 1 
            total2_0_2_OA_player <- c(total2_0_2_OA_player,table_row$base2)
            #total1_1_1_player <- table(total2_0_2_OA_player)
            
          }
      
    }else if(table_row$out == "零出局" && table_row$direction == "中"){
      total2_0_center = total2_0_center + 1 
      
      i = i + 1
      table_row_1 <- cpbl_table[i,]
      
            #跑者上三壘
          if(is.na(table_row_1$base3) != TRUE){
            
            total2_0_center_3rd = total2_0_center_3rd + 1 
            total2_0_3_3rd_player <- c(total2_0_3_3rd_player,table_row$base2)
            #total1_1_2_player <- table(total2_0_3_3rd_player)
            
            #跑者得分
          }else if(is.na(table_row_1$base3) == TRUE && table_row$out == table_row_1$out){
            
            total2_0_center_score = total2_0_center_score + 1 
            total2_0_3_score_player <- c(total2_0_3_score_player,table_row$base2)
            #total1_1_3_player <- table(total2_0_3_score_player)
            
            #跑者出局
          }else if(table_row$out != table_row_1$out){
            
            total2_0_center_OA = total2_0_center_OA + 1 
            total2_0_3_OA_player <- c(total2_0_3_OA_player,table_row$base2)
            #total1_1_1_player <- table(total2_0_3_OA_player)
            
          }
      
    }else if(table_row$out == "零出局" && table_row$direction == "右"){
      total2_0_right = total2_0_right + 1
      
      i = i + 1
      table_row_1 <- cpbl_table[i,]
      
            #跑者上三壘
          if(is.na(table_row_1$base3) != TRUE){
            
            total2_0_right_3rd = total2_0_right_3rd + 1 
            total2_0_4_3rd_player <- c(total2_0_4_3rd_player,table_row$base2)
            #total1_1_2_player <- table(total2_0_4_3rd_player)
            
            #跑者得分
          }else if(is.na(table_row_1$base3) == TRUE && table_row$out == table_row_1$out){
            
            total2_0_right_score = total2_0_right_score + 1 
            total2_0_4_score_player <- c(total2_0_4_score_player,table_row$base2)
            #total1_1_3_player <- table(total2_0_4_score_player)
            
            #跑者出局
          }else if(table_row$out != table_row_1$out){
            
            total2_0_right_OA = total2_0_right_OA + 1 
            total2_0_4_OA_player <- c(total2_0_4_OA_player,table_row$base2)
            #total1_1_1_player <- table(total2_0_4_OA_player)
            
          }
    }
    
    if (table_row$out == "一出局" && table_row$direction == "內"){
      total2_1_other = total2_1_other + 1 
      
      i = i + 1
      table_row_1 <- cpbl_table[i,]
      
            #跑者上三壘
          if(is.na(table_row_1$base3) != TRUE){
            
            total2_1_other_3rd = total2_1_other_3rd + 1 
            total2_1_1_3rd_player <- c(total2_1_1_3rd_player,table_row$base2)
            #total1_1_2_player <- table(total2_1_1_3rd_player)
            
            #跑者得分
          }else if(is.na(table_row_1$base3) == TRUE && table_row$out == table_row_1$out){
            
            total2_1_other_score = total2_1_other_score + 1 
            total2_1_1_score_player <- c(total2_1_1_score_player,table_row$base2)
            #total1_1_3_player <- table(total2_1_1_score_player)
            
            #跑者出局
          }else if(table_row$out != table_row_1$out){
            
            total2_1_other_OA = total2_1_other_OA + 1 
            total2_1_1_OA_player <- c(total2_1_1_OA_player,table_row$base2)
            #total1_1_1_player <- table(total2_1_1_OA_player)
            
          }
      
    }else if(table_row$out == "一出局" && table_row$direction == "左"){
      total2_1_left = total2_1_left + 1
      
      i = i + 1
      table_row_1 <- cpbl_table[i,]
      
            #跑者上三壘
          if(is.na(table_row_1$base3) != TRUE){
            
            total2_1_left_3rd = total2_1_left_3rd + 1 
            total2_1_2_3rd_player <- c(total2_1_2_3rd_player,table_row$base2)
            #total1_1_2_player <- table(total2_1_2_3rd_player)
            
            #跑者得分
          }else if(is.na(table_row_1$base3) == TRUE && table_row$out == table_row_1$out){
            
            total2_1_left_score = total2_1_left_score + 1 
            total2_1_2_score_player <- c(total2_1_2_score_player,table_row$base2)
            #total1_1_3_player <- table(total2_1_2_score_player)
            
            #跑者出局
          }else if(table_row$out != table_row_1$out){
            
            total2_1_left_OA = total2_1_left_OA + 1 
            total2_1_2_OA_player <- c(total2_1_2_OA_player,table_row$base2)
            #total1_1_1_player <- table(total2_1_2_OA_player)
            
          }
      
    }else if(table_row$out == "一出局" && table_row$direction == "中"){
      total2_1_center = total2_1_center + 1 
      
      i = i + 1
      table_row_1 <- cpbl_table[i,]
      
            #跑者上三壘
          if(is.na(table_row_1$base3) != TRUE){
      
            total2_1_center_3rd = total2_1_center_3rd + 1 
            total2_1_3_3rd_player <- c(total2_1_3_3rd_player,table_row$base2)
            #total1_1_2_player <- table(total2_1_3_3rd_player)
            
            #跑者得分
          }else if(is.na(table_row_1$base3) == TRUE && table_row$out == table_row_1$out){
            
            total2_1_center_score = total2_1_center_score + 1 
            total2_1_3_score_player <- c(total2_1_3_score_player,table_row$base2)
            #total1_1_3_player <- table(total2_1_3_score_player)
            
            #跑者出局
          }else if(table_row$out != table_row_1$out){
            
            total2_1_center_OA = total2_1_center_OA + 1 
            total2_1_3_OA_player <- c(total2_1_3_OA_player,table_row$base2)
            #total1_1_1_player <- table(total2_1_3_OA_player)
            
          }
      
    }else if(table_row$out == "一出局" && table_row$direction == "右"){
      total2_1_right = total2_1_right + 1
     
       i = i + 1
      table_row_1 <- cpbl_table[i,]
      
          #跑者上三壘
          if(is.na(table_row_1$base3) != TRUE){
            
            total2_1_right_3rd = total2_1_right_3rd + 1 
            total2_1_4_3rd_player <- c(total2_1_4_3rd_player,table_row$base2)
            #total1_1_2_player <- table(total2_1_4_3rd_player)
            
            #跑者得分
          }else if(is.na(table_row_1$base3) == TRUE && table_row$out == table_row_1$out){
            
            total2_1_right_score = total2_1_right_score + 1 
            total2_1_4_score_player <- c(total2_1_4_score_player,table_row$base2)
            #total1_1_3_player <- table(total2_1_4_score_player)
            
            #跑者出局
          }else if(table_row$out != table_row_1$out){
            
            total2_1_right_OA = total2_1_right_OA + 1 
            total2_1_4_OA_player <- c(total2_1_4_OA_player,table_row$base2)
            #total1_1_1_player <- table(total2_1_4_OA_player)
            
          }
    }
    
    if (table_row$out == "二出局" && table_row$direction == "內"){
      total2_2_other = total2_2_other + 1 
      
      i = i + 1
      table_row_1 <- cpbl_table[i,]
      
          #跑者上三壘
          if(is.na(table_row_1$base3) != TRUE){
            
            total2_2_other_3rd = total2_2_other_3rd + 1 
            total2_2_1_3rd_player <- c(total2_2_1_3rd_player,table_row$base2)
            #total1_1_2_player <- table(total2_2_1_3rd_player)
            
            #跑者得分
          }else if(is.na(table_row_1$base3) == TRUE && table_row$out == table_row_1$out){
            
            total2_2_other_score = total2_2_other_score + 1 
            total2_2_1_score_player <- c(total2_2_1_score_player,table_row$base2)
            #total1_1_3_player <- table(total2_2_1_score_player)
            
            #跑者出局
          }else if(table_row$out != table_row_1$out){
            
            total2_2_other_OA = total2_2_other_OA + 1 
            total2_2_1_OA_player <- c(total2_2_1_OA_player,table_row$base2)
            #total1_1_1_player <- table(total2_2_1_OA_player)
            
          }
      
    }else if(table_row$out == "二出局" && table_row$direction == "左"){
      total2_2_left = total2_2_left + 1
      
      i = i + 1
      table_row_1 <- cpbl_table[i,]
      
      #跑者上三壘
      if(is.na(table_row_1$base3) != TRUE){
        
        total2_2_left_3rd = total2_2_left_3rd + 1 
        total2_2_2_3rd_player <- c(total2_2_2_3rd_player,table_row$base2)
        #total1_1_2_player <- table(total2_2_2_3rd_player)
        
        #跑者得分
      }else if(is.na(table_row_1$base3) == TRUE && table_row$out == table_row_1$out){
        
        total2_2_left_score = total2_2_left_score + 1 
        total2_2_2_score_player <- c(total2_2_2_score_player,table_row$base2)
        #total1_1_3_player <- table(total2_2_2_score_player)
        
        #跑者出局
      }else if(table_row$out != table_row_1$out){
        
        total2_2_left_OA = total2_2_left_OA + 1 
        total2_2_2_OA_player <- c(total2_2_2_OA_player,table_row$base2)
        #total1_1_1_player <- table(total2_2_2_OA_player)
        
      }
      
    }else if(table_row$out == "二出局" && table_row$direction == "中"){
      total2_2_center = total2_2_center + 1 
      
      i = i + 1
      table_row_1 <- cpbl_table[i,]
      
          #跑者上三壘
          if(is.na(table_row_1$base3) != TRUE){
            
            total2_2_center_3rd = total2_2_center_3rd + 1 
            total2_2_3_3rd_player <- c(total2_2_3_3rd_player,table_row$base2)
            #total1_1_2_player <- table(total2_2_3_3rd_player)
            
            #跑者得分
          }else if(is.na(table_row_1$base3) == TRUE && table_row$out == table_row_1$out){
            
            total2_2_center_score = total2_2_center_score + 1 
            total2_2_3_score_player <- c(total2_2_3_score_player,table_row$base2)
            #total1_1_3_player <- table(total2_2_3_score_player)
            
            #跑者出局
          }else if(table_row$out != table_row_1$out){
            
            total2_2_center_OA = total2_2_center_OA + 1 
            total2_2_3_OA_player <- c(total2_2_3_OA_player,table_row$base2)
            #total1_1_1_player <- table(total2_2_3_OA_player)
            
          }
      
    }else if(table_row$out == "二出局" && table_row$direction == "右"){
      total2_2_right = total2_2_right + 1
      
      i = i + 1
      table_row_1 <- cpbl_table[i,]
      
          #跑者上三壘
          if(is.na(table_row_1$base3) != TRUE){
            
            total2_2_right_3rd = total2_2_right_3rd + 1 
            total2_2_4_3rd_player <- c(total2_2_4_3rd_player,table_row$base2)
            #total1_1_2_player <- table(total2_2_4_3rd_player)
            
            #跑者得分
          }else if(is.na(table_row_1$base3) == TRUE && table_row$out == table_row_1$out){
            
            total2_2_right_score = total2_2_right_score + 1 
            total2_2_4_score_player <- c(total2_2_4_score_player,table_row$base2)
            #total1_1_3_player <- table(total2_2_4_score_player)
            
            #跑者出局
          }else if(table_row$out != table_row_1$out){
            
            total2_2_right_OA = total2_2_right_OA + 1 
            total2_2_4_OA_player <- c(total2_2_4_OA_player,table_row$base2)
            #total1_1_1_player <- table(total2_2_4_OA_player)
            
          }
    }
    
    
  }
  
}

#計算status1(零出局)，跑者到三壘，得分，出局的機率
#prob(第幾狀況)_(幾出局)_(方向)_(狀況)

#安打方向(內安)
prob2_0_1_OA <-  round(total2_0_other_OA/total2_0_other,digits = 3)
prob2_0_1_3rd <-  round(total2_0_other_3rd/total2_0_other,digits = 3)
prob2_0_1_score <-  round(total2_0_other_score/total2_0_other,digits = 3)

#安打方向(左安)
prob2_0_2_OA <-  round(total2_0_left_OA/total2_0_left,digits = 3)
prob2_0_2_3rd <-  round(total2_0_left_3rd/total2_0_left,digits = 3)
prob2_0_2_score <-  round(total2_0_left_score/total2_0_left,digits = 3)

#安打方向(中安)
prob2_0_3_OA <-  round(total2_0_center_OA/total2_0_center,digits = 3)
prob2_0_3_3rd <-  round(total2_0_center_3rd/total2_0_center,digits = 3)
prob2_0_3_score <-  round(total2_0_center_score/total2_0_center,digits = 3)

#安打方向(右安)
prob2_0_4_OA <-  round(total2_0_right_OA/total2_0_right,digits = 3)
prob2_0_4_3rd <-  round(total2_0_right_3rd/total2_0_right,digits = 3)
prob2_0_4_score <-  round(total2_0_right_score/total2_0_right,digits = 3)


#計算status2(一出局)，跑者到三壘，得分，出局的機率

#安打方向(內安)
prob2_1_1_OA <-  round(total2_1_other_OA/total2_1_other,digits = 3)
prob2_1_1_3rd <-  round(total2_1_other_3rd/total2_1_other,digits = 3)
prob2_1_1_score <-  round(total2_1_other_score/total2_1_other,digits = 3)

#安打方向(左安)
prob2_1_2_OA <-  round(total2_1_left_OA/total2_1_left,digits = 3)
prob2_1_2_3rd <-  round(total2_1_left_3rd/total2_1_left,digits = 3)
prob2_1_2_score <-  round(total2_1_left_score/total2_1_left,digits = 3)

#安打方向(中安)
prob2_1_3_OA <-  round(total2_1_center_OA/total2_1_center,digits = 3)
prob2_1_3_3rd <-  round(total2_1_center_3rd/total2_1_center,digits = 3)
prob2_1_3_score <-  round(total2_1_center_score/total2_1_center,digits = 3)

#安打方向(右安)
prob2_1_4_OA <-  round(total2_1_right_OA/total2_1_right,digits = 3)
prob2_1_4_3rd <-  round(total2_1_right_3rd/total2_1_right,digits = 3)
prob2_1_4_score <-  round(total2_1_right_score/total2_1_right,digits = 3)


#計算status2(兩出局)，跑者到三壘，得分，出局的機率

#安打方向(內安)
prob2_2_1_OA <-  round(total2_2_other_OA/total2_2_other,digits = 3)
prob2_2_1_3rd <-  round(total2_2_other_3rd/total2_2_other,digits = 3)
prob2_2_1_score <-  round(total2_2_other_score/total2_2_other,digits = 3)

#安打方向(左安)
prob2_2_2_OA <-  round(total2_2_left_OA/total2_2_left,digits = 3)
prob2_2_2_3rd <-  round(total2_2_left_3rd/total2_2_left,digits = 3)
prob2_2_2_score <-  round(total2_2_left_score/total2_2_left,digits = 3)

#安打方向(中安)
prob2_2_3_OA <-  round(total2_2_center_OA/total2_2_center,digits = 3)
prob2_2_3_3rd <-  round(total2_2_center_3rd/total2_2_center,digits = 3)
prob2_2_3_score <-  round(total2_2_center_score/total2_2_center,digits = 3)

#安打方向(右安)
prob2_2_4_OA <-  round(total2_2_right_OA/total2_2_right,digits = 3)
prob2_2_4_3rd <-  round(total2_2_right_3rd/total2_2_right,digits = 3)
prob2_2_4_score <-  round(total2_2_right_score/total2_2_right,digits = 3)


extrabase2_table = rbind(
#0出局
list(Outs_Where = "0-other", Opp = total2_0_other, To3rd = prob2_0_1_3rd, Score = prob2_0_1_score, OA = prob2_0_1_OA),
list(Outs_Where = "0-left", Opp = total2_0_left, To3rd = prob2_0_2_3rd, Score = prob2_0_2_score, OA = prob2_0_2_OA),
list(Outs_Where = "0-center", Opp = total2_0_center, To3rd = prob2_0_3_3rd, Score = prob2_0_3_score, OA = prob2_0_3_OA),
list(Outs_Where = "0-right", Opp = total2_0_right, To3rd = prob2_0_4_3rd, Score = prob2_0_4_score, OA = prob2_0_4_OA),

#1出局
list(Outs_Where = "1-other", Opp = total2_1_other, To3rd = prob2_1_1_3rd, Score = prob2_1_1_score, OA = prob2_1_1_OA),
list(Outs_Where = "1-left", Opp = total2_1_left, To3rd = prob2_1_2_3rd, Score = prob2_1_2_score, OA = prob2_1_2_OA),
list(Outs_Where = "1-center", Opp = total2_1_center, To3rd = prob2_1_3_3rd, Score = prob2_1_3_score, OA = prob2_1_3_OA),
list(Outs_Where = "1-right", Opp = total2_1_right, To3rd = prob2_1_4_3rd, Score = prob2_1_4_score, OA = prob2_1_4_OA),

#2出局
list(Outs_Where ="2-other" ,Opp= total2_2_other, To3rd =prob2_2_1_3rd, Score= prob2_2_1_score, OA= prob2_2_1_OA),
list(Outs_Where ="2-left" ,Opp= total2_2_left, To3rd =prob2_2_2_3rd, Score= prob2_2_2_score, OA= prob2_2_2_OA),
list(Outs_Where ="2-center" ,Opp= total2_2_center, To3rd =prob2_2_3_3rd, Score= prob2_2_3_score, OA= prob2_2_3_OA),
list(Outs_Where ="2-right" ,Opp= total2_2_right, To3rd =prob2_2_4_3rd, Score= prob2_2_4_score, OA= prob2_2_4_OA)
)

View (extrabase2_table)