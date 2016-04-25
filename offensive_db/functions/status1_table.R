cpbl_table <- read.csv(file("C:/Users/Student/Desktop/output042303.csv", encoding="big5"), header = TRUE, sep = ",",stringsAsFactors = FALSE)
# cpbl_player <- read.table(file("C:/Users/Student/Desktop/CPBL/batter.csv", encoding="UTF-8"), header = TRUE, sep = ",",stringsAsFactors = FALSE)
# player_library <- unique(cpbl_player$name)

total1 <- 0

#零出局
total1_1 <- total1_1_1 <- total1_1_2 <- total1_1_3 <- total1_1_4 <- 0
total1_2 <- total1_2_1 <- total1_2_2 <- total1_2_3 <- total1_2_4 <- 0
total1_3 <- total1_3_1 <- total1_3_2 <- total1_3_3 <- total1_3_4 <- 0
total1_4 <- total1_4_1 <- total1_4_2 <- total1_4_3 <- total1_4_4 <- 0

total1_1_1_player <- total1_1_2_player <- total1_1_3_player <- total1_1_4_player <- 
total1_2_1_player <- total1_2_2_player <- total1_2_3_player <- total1_2_4_player <- 
total1_3_1_player <- total1_3_2_player <- total1_3_3_player <- total1_3_4_player <- 
total1_4_1_player <- total1_4_2_player <- total1_4_3_player <- total1_4_4_player <- NULL
#一出局
total2_1 <- total2_1_1 <- total2_1_2 <- total2_1_3 <- total2_1_4 <- 0
total2_2 <- total2_2_1 <- total2_2_2 <- total2_2_3 <- total2_2_4 <- 0
total2_3 <- total2_3_1 <- total2_3_2 <- total2_3_3 <- total2_3_4 <- 0
total2_4 <- total2_4_1 <- total2_4_2 <- total2_4_3 <- total2_4_4 <- 0

total2_1_1_player <- total2_1_2_player <- total2_1_3_player <- total2_1_4_player <- 
total2_2_1_player <- total2_2_2_player <- total2_2_3_player <- total2_2_4_player <- 
total2_3_1_player <- total2_3_2_player <- total2_3_3_player <- total2_3_4_player <- 
total2_4_1_player <- total2_4_2_player <- total2_4_3_player <- total2_4_4_player <- NULL
#兩出局
total3_1 <- total3_1_1 <- total3_1_2 <- total3_1_3 <- total3_1_4 <- 0
total3_2 <- total3_2_1 <- total3_2_2 <- total3_2_3 <- total3_2_4 <- 0
total3_3 <- total3_3_1 <- total3_3_2 <- total3_3_3 <- total3_3_4 <- 0
total3_4 <- total3_4_1 <- total3_4_2 <- total3_4_3 <- total3_4_4 <- 0

total3_1_1_player <- total3_1_2_player <- total3_1_3_player <- total3_1_4_player <- 
total3_2_1_player <- total3_2_2_player <- total3_2_3_player <- total3_2_4_player <- 
total3_3_1_player <- total3_3_2_player <- total3_3_3_player <- total3_3_4_player <- 
total3_4_1_player <- total3_4_2_player <- total3_4_3_player <- total3_4_4_player <- NULL


for (i in 1:nrow(cpbl_table)){
  table_row <- cpbl_table[i,]
  # j <- i + 1
  if (is.na(table_row$base1) !=TRUE && is.na(table_row$base2) == TRUE && is.na(table_row$base3) == TRUE &&
      table_row$result %in% c("一壘安打", "左外野安打", "中外野安打", "右外野安打", 
                              "穿越安打", "平飛安打", "內野安打", "中間方向安打", "德州安打")){
    
    total1 = total1 + 1 
    
    if (table_row$out == "零出局" && table_row$direction == "內"){
      total1_1 = total1_1 + 1 
      i = i + 1
      table_row_1 <- cpbl_table[i,]
      if(is.na(table_row_1$base2) == TRUE && is.na(table_row_1$base3) == TRUE && 
         table_row$away == table_row_1$away || table_row$home == table_row_1$home && 
         table_row$out != table_row_1$out){
        total1_1_1 = total1_1_1 + 1 
        total1_1_1_player <- c(total1_1_1_player,table_row$base1)
        total1_1_1_players <- table(total1_1_1_player)
        
        # print(table_row$base1)
        #print("跑者出局")
      }else if(is.na(table_row_1$base2) != TRUE && is.na(table_row_1$base3) == TRUE && 
               table_row$away == table_row_1$away || table_row$home == table_row_1$home && 
               table_row$out == table_row_1$out){
        total1_1_2 = total1_1_2 + 1 
        total1_1_2_player <- c(total1_1_2_player,table_row$base1)
        total1_1_2_players <- table(total1_1_2_player)
        
        #print("跑者上二壘")
      }else if(is.na(table_row_1$base2) == TRUE && is.na(table_row_1$base3) != TRUE && 
               table_row$away == table_row_1$away || table_row$home == table_row_1$home && 
               table_row$out == table_row_1$out){
        total1_1_3 = total1_1_3 + 1 
        total1_1_3_player <- c(total1_1_3_player,table_row$base1)
        total1_1_3_players <- table(total1_1_3_player)
        
        #print("跑者上三壘")
      }else if(is.na(table_row_1$base2) == TRUE && is.na(table_row_1$base3) == TRUE && 
               table_row$away != table_row_1$away || table_row$home != table_row_1$home && 
               table_row$out == table_row_1$out){
        total1_1_4 = total1_1_4 + 1 
        total1_1_4_player <- c(total1_1_4_player,table_row$base1)
        total1_1_4_players <- table(total1_1_4_player)
        
        #print("跑者回來得分")
      }
      
    }else if(table_row$out == "零出局" && table_row$direction == "左"){
      total1_2 = total1_2 + 1
      i = i + 1
      table_row_1 <- cpbl_table[i,]
      if(is.na(table_row_1$base2) == TRUE && is.na(table_row_1$base3) == TRUE && 
         table_row$away == table_row_1$away || table_row$home == table_row_1$home && 
         table_row$out != table_row_1$out){
        total1_2_1 = total1_2_1 + 1 
        total1_2_1_player <- c(total1_2_1_player,table_row$base1)
        total1_2_1_players <- table(total1_2_1_player)
        
        #print("跑者出局")
      }else if(is.na(table_row_1$base2) != TRUE && is.na(table_row_1$base3) == TRUE && 
               table_row$away == table_row_1$away || table_row$home == table_row_1$home && 
               table_row$out == table_row_1$out){
        total1_2_2 = total1_2_2 + 1
        total1_2_2_player <- c(total1_2_2_player,table_row$base1)
        total1_2_2_players <- table(total1_2_2_player)
        
        #print("跑者上二壘")
      }else if(is.na(table_row_1$base2) == TRUE && is.na(table_row_1$base3) != TRUE && 
               table_row$away == table_row_1$away || table_row$home == table_row_1$home && 
               table_row$out == table_row_1$out){
        total1_2_3 = total1_2_3 + 1 
        total1_2_3_player <- c(total1_2_3_player,table_row$base1)
        total1_2_3_players <- table(total1_2_3_player)
        
        #print("跑者上三壘")
      }else if(is.na(table_row_1$base2) == TRUE && is.na(table_row_1$base3) == TRUE && 
               table_row$away != table_row_1$away || table_row$home != table_row_1$home && 
               table_row$out == table_row_1$out){
        total1_2_4 = total1_2_4 + 1
        total1_2_4_player <- c(total1_2_4_player,table_row$base1)
        total1_2_4_players <- table(total1_2_4_player)
        
        #print("跑者回來得分")
      }
      
    }else if(table_row$out == "零出局" && table_row$direction == "中"){
      total1_3 = total1_3 + 1 
      i = i + 1
      table_row_1 <- cpbl_table[i,]
      if(is.na(table_row_1$base2) == TRUE && is.na(table_row_1$base3) == TRUE && 
         table_row$away == table_row_1$away || table_row$home == table_row_1$home && 
         table_row$out != table_row_1$out){
        total1_3_1 = total1_3_1 + 1
        total1_3_1_player <- c(total1_3_1_player,table_row$base1)
        total1_3_1_players <- table(total1_3_1_player)
        
        #print("跑者出局")
      }else if(is.na(table_row_1$base2) != TRUE && is.na(table_row_1$base3) == TRUE && 
               table_row$away == table_row_1$away || table_row$home == table_row_1$home && 
               table_row$out == table_row_1$out){
        total1_3_2 = total1_3_2 + 1 
        total1_3_2_player <- c(total1_3_2_player,table_row$base1)
        total1_3_2_players <- table(total1_3_2_player)
        
        #print("跑者上二壘")
      }else if(is.na(table_row_1$base2) == TRUE && is.na(table_row_1$base3) != TRUE && 
               table_row$away == table_row_1$away || table_row$home == table_row_1$home && 
               table_row$out == table_row_1$out){
        total1_3_3 = total1_3_3 + 1
        total1_3_3_player <- c(total1_3_3_player,table_row$base1)
        total1_3_3_players <- table(total1_3_3_player)
        
        #print("跑者上三壘")
      }else if(is.na(table_row_1$base2) == TRUE && is.na(table_row_1$base3) == TRUE && 
               table_row$away != table_row_1$away || table_row$home != table_row_1$home && 
               table_row$out == table_row_1$out){
        total1_3_4 = total1_3_4 + 1
        total1_3_4_player <- c(total1_3_4_player,table_row$base1)
        total1_3_4_players <- table(total1_3_4_player)
        
        #print("跑者回來得分")
      }
      
    }else if(table_row$out == "零出局" && table_row$direction == "右"){
      total1_4 = total1_4 + 1
      i = i + 1
      table_row_1 <- cpbl_table[i,]
      if(is.na(table_row_1$base2) == TRUE && is.na(table_row_1$base3) == TRUE && 
         table_row$away == table_row_1$away || table_row$home == table_row_1$home && 
         table_row$out != table_row_1$out){
        total1_4_1 = total1_4_1 + 1
        total1_4_1_player <- c(total1_4_1_player,table_row$base1)
        total1_4_1_players <- table(total1_4_1_player)
        
        #print("跑者出局")
      }else if(is.na(table_row_1$base2) != TRUE && is.na(table_row_1$base3) == TRUE && 
               table_row$away == table_row_1$away || table_row$home == table_row_1$home && 
               table_row$out == table_row_1$out){
        total1_4_2 = total1_4_2 + 1
        total1_4_2_player <- c(total1_4_2_player,table_row$base1)
        total1_4_2players <- table(total1_4_2_player)
        
        #print("跑者上二壘")
      }else if(is.na(table_row_1$base2) == TRUE && is.na(table_row_1$base3) != TRUE && 
               table_row$away == table_row_1$away || table_row$home == table_row_1$home && 
               table_row$out == table_row_1$out){
        total1_4_3 = total1_4_3 + 1  
        total1_4_3_player <- c(total1_4_3_player,table_row$base1)
        total1_4_3_players <- table(total1_4_3_player)
        
        #print("跑者上三壘")
      }else if(is.na(table_row_1$base2) == TRUE && is.na(table_row_1$base3) == TRUE && 
               table_row$away != table_row_1$away || table_row$home != table_row_1$home && 
               table_row$out == table_row_1$out){
        total1_4_4 = total1_4_4 + 1
        total1_4_4_player <- c(total1_4_4_player,table_row$base1)
        total1_4_4_players <- table(total1_4_4_player)
        
        #print("跑者回來得分")
      }
    }
    
    if (table_row$out == "一出局" && table_row$direction == "內"){
      total2_1 = total2_1 + 1 
      i = i + 1
      table_row_1 <- cpbl_table[i,]
      if(is.na(table_row_1$base2) == TRUE && is.na(table_row_1$base3) == TRUE && 
         table_row$away == table_row_1$away || table_row$home == table_row_1$home && 
         table_row$out != table_row_1$out){
        total2_1_1 = total2_1_1 + 1 
        total2_1_1_player <- c(total2_1_1_player,table_row$base1)
        total2_1_1_players <- table(total2_1_1_player)
        
        # print(table_row$base1)
        #print("跑者出局")
      }else if(is.na(table_row_1$base2) != TRUE && is.na(table_row_1$base3) == TRUE && 
               table_row$away == table_row_1$away || table_row$home == table_row_1$home && 
               table_row$out == table_row_1$out){
        total2_1_2 = total2_1_2 + 1 
        total2_1_2_player <- c(total2_1_2_player,table_row$base1)
        total2_1_2_players <- table(total2_1_2_player)
        
        #print("跑者上二壘")
      }else if(is.na(table_row_1$base2) == TRUE && is.na(table_row_1$base3) != TRUE && 
               table_row$away == table_row_1$away || table_row$home == table_row_1$home && 
               table_row$out == table_row_1$out){
        total2_1_3 = total2_1_3 + 1 
        total2_1_3_player <- c(total2_1_3_player,table_row$base1)
        total2_1_3_players <- table(total2_1_3_player)
        
        #print("跑者上三壘")
      }else if(is.na(table_row_1$base2) == TRUE && is.na(table_row_1$base3) == TRUE && 
               table_row$away != table_row_1$away || table_row$home != table_row_1$home && 
               table_row$out == table_row_1$out){
        total2_1_4 = total2_1_4 + 1 
        total2_1_4_player <- c(total2_1_4_player,table_row$base1)
        total2_1_4_players <- table(total2_1_4_player)
        
        #print("跑者回來得分")
      }
      
    }else if(table_row$out == "一出局" && table_row$direction == "左"){
      total2_2 = total2_2 + 1
      i = i + 1
      table_row_1 <- cpbl_table[i,]
      if(is.na(table_row_1$base2) == TRUE && is.na(table_row_1$base3) == TRUE && 
         table_row$away == table_row_1$away || table_row$home == table_row_1$home && 
         table_row$out != table_row_1$out){
        total2_2_1 = total2_2_1 + 1 
        total2_2_1_player <- c(total2_2_1_player,table_row$base1)
        total2_2_1_players <- table(total2_2_1_player)
        
        #print("跑者出局")
      }else if(is.na(table_row_1$base2) != TRUE && is.na(table_row_1$base3) == TRUE && 
               table_row$away == table_row_1$away || table_row$home == table_row_1$home && 
               table_row$out == table_row_1$out){
        total2_2_2 = total2_2_2 + 1
        total2_2_2_player <- c(total2_2_2_player,table_row$base1)
        total2_2_2_players <- table(total2_2_2_player)
        
        #print("跑者上二壘")
      }else if(is.na(table_row_1$base2) == TRUE && is.na(table_row_1$base3) != TRUE && 
               table_row$away == table_row_1$away || table_row$home == table_row_1$home && 
               table_row$out == table_row_1$out){
        total2_2_3 = total2_2_3 + 1 
        total2_2_3_player <- c(total2_2_3_player,table_row$base1)
        total2_2_3_players <- table(total2_2_3_player)
        
        #print("跑者上三壘")
      }else if(is.na(table_row_1$base2) == TRUE && is.na(table_row_1$base3) == TRUE && 
               table_row$away != table_row_1$away || table_row$home != table_row_1$home && 
               table_row$out == table_row_1$out){
        total2_2_4 = total2_2_4 + 1
        total2_2_4_player <- c(total2_2_4_player,table_row$base1)
        total2_2_4_players <- table(total2_2_4_player)
        
        #print("跑者回來得分")
      }
      
    }else if(table_row$out == "一出局" && table_row$direction == "中"){
      total2_3 = total2_3 + 1 
      i = i + 1
      table_row_1 <- cpbl_table[i,]
      if(is.na(table_row_1$base2) == TRUE && is.na(table_row_1$base3) == TRUE && 
         table_row$away == table_row_1$away || table_row$home == table_row_1$home && 
         table_row$out != table_row_1$out){
        total2_3_1 = total2_3_1 + 1
        total2_3_1_player <- c(total2_3_1_player,table_row$base1)
        total2_3_1_players <- table(total2_3_1_player)
        
        #print("跑者出局")
      }else if(is.na(table_row_1$base2) != TRUE && is.na(table_row_1$base3) == TRUE && 
               table_row$away == table_row_1$away || table_row$home == table_row_1$home && 
               table_row$out == table_row_1$out){
        total2_3_2 = total2_3_2 + 1 
        total2_3_2_player <- c(total2_3_2_player,table_row$base1)
        total2_3_2_players <- table(total2_3_2_player)
        
        #print("跑者上二壘")
      }else if(is.na(table_row_1$base2) == TRUE && is.na(table_row_1$base3) != TRUE && 
               table_row$away == table_row_1$away || table_row$home == table_row_1$home && 
               table_row$out == table_row_1$out){
        total2_3_3 = total2_3_3 + 1
        total2_3_3_player <- c(total2_3_3_player,table_row$base1)
        total2_3_3_players <- table(total2_3_3_player)
        
        #print("跑者上三壘")
      }else if(is.na(table_row_1$base2) == TRUE && is.na(table_row_1$base3) == TRUE && 
               table_row$away != table_row_1$away || table_row$home != table_row_1$home && 
               table_row$out == table_row_1$out){
        total2_3_4 = total2_3_4 + 1
        total2_3_4_player <- c(total2_3_4_player,table_row$base1)
        total2_3_4_players <- table(total2_3_4_player)
        
        #print("跑者回來得分")
      }
      
    }else if(table_row$out == "一出局" && table_row$direction == "右"){
      total2_4 = total2_4 + 1
      i = i + 1
      table_row_1 <- cpbl_table[i,]
      if(is.na(table_row_1$base2) == TRUE && is.na(table_row_1$base3) == TRUE && 
         table_row$away == table_row_1$away || table_row$home == table_row_1$home && 
         table_row$out != table_row_1$out){
        total2_4_1 = total2_4_1 + 1
        total2_4_1_player <- c(total2_4_1_player,table_row$base1)
        total2_4_1_players <- table(total2_4_1_player)
        
        #print("跑者出局")
      }else if(is.na(table_row_1$base2) != TRUE && is.na(table_row_1$base3) == TRUE && 
               table_row$away == table_row_1$away || table_row$home == table_row_1$home && 
               table_row$out == table_row_1$out){
        total2_4_2 = total2_4_2 + 1
        total2_4_2_player <- c(total2_4_2_player,table_row$base1)
        total2_4_2players <- table(total2_4_2_player)
        
        #print("跑者上二壘")
      }else if(is.na(table_row_1$base2) == TRUE && is.na(table_row_1$base3) != TRUE && 
               table_row$away == table_row_1$away || table_row$home == table_row_1$home && 
               table_row$out == table_row_1$out){
        total2_4_3 = total2_4_3 + 1  
        total2_4_3_player <- c(total2_4_3_player,table_row$base1)
        total2_4_3_players <- table(total2_4_3_player)
        
        #print("跑者上三壘")
      }else if(is.na(table_row_1$base2) == TRUE && is.na(table_row_1$base3) == TRUE && 
               table_row$away != table_row_1$away || table_row$home != table_row_1$home && 
               table_row$out == table_row_1$out){
        total2_4_4 = total2_4_4 + 1
        total2_4_4_player <- c(total2_4_4_player,table_row$base1)
        total2_4_4_players <- table(total2_4_4_player)
        
        #print("跑者回來得分")
      }
    }
    
    if (table_row$out == "二出局" && table_row$direction == "內"){
      total3_1 = total3_1 + 1 
      i = i + 1
      table_row_1 <- cpbl_table[i,]
      if(is.na(table_row_1$base2) == TRUE && is.na(table_row_1$base3) == TRUE && 
         table_row$away == table_row_1$away || table_row$home == table_row_1$home && 
         table_row$out != table_row_1$out){
        total3_1_1 = total3_1_1 + 1 
        total3_1_1_player <- c(total3_1_1_player,table_row$base1)
        total3_1_1_players <- table(total3_1_1_player)
        
        # print(table_row$base1)
        #print("跑者出局")
      }else if(is.na(table_row_1$base2) != TRUE && is.na(table_row_1$base3) == TRUE && 
               table_row$away == table_row_1$away || table_row$home == table_row_1$home && 
               table_row$out == table_row_1$out){
        total3_1_2 = total3_1_2 + 1 
        total3_1_2_player <- c(total3_1_2_player,table_row$base1)
        total3_1_2_players <- table(total3_1_2_player)
        
        #print("跑者上二壘")
      }else if(is.na(table_row_1$base2) == TRUE && is.na(table_row_1$base3) != TRUE && 
               table_row$away == table_row_1$away || table_row$home == table_row_1$home && 
               table_row$out == table_row_1$out){
        total3_1_3 = total3_1_3 + 1 
        total3_1_3_player <- c(total3_1_3_player,table_row$base1)
        total3_1_3_players <- table(total3_1_3_player)
        
        #print("跑者上三壘")
      }else if(is.na(table_row_1$base2) == TRUE && is.na(table_row_1$base3) == TRUE && 
               table_row$away != table_row_1$away || table_row$home != table_row_1$home && 
               table_row$out == table_row_1$out){
        total3_1_4 = total3_1_4 + 1 
        total3_1_4_player <- c(total3_1_4_player,table_row$base1)
        total3_1_4_players <- table(total3_1_4_player)
        
        #print("跑者回來得分")
      }
      
    }else if(table_row$out == "二出局" && table_row$direction == "左"){
      total3_2 = total3_2 + 1
      i = i + 1
      table_row_1 <- cpbl_table[i,]
      if(is.na(table_row_1$base2) == TRUE && is.na(table_row_1$base3) == TRUE && 
         table_row$away == table_row_1$away || table_row$home == table_row_1$home && 
         table_row$out != table_row_1$out){
        total3_2_1 = total3_2_1 + 1 
        total3_2_1_player <- c(total3_2_1_player,table_row$base1)
        total3_2_1_players <- table(total3_2_1_player)
        
        #print("跑者出局")
      }else if(is.na(table_row_1$base2) != TRUE && is.na(table_row_1$base3) == TRUE && 
               table_row$away == table_row_1$away || table_row$home == table_row_1$home && 
               table_row$out == table_row_1$out){
        total3_2_2 = total3_2_2 + 1
        total3_2_2_player <- c(total3_2_2_player,table_row$base1)
        total3_2_2_players <- table(total3_2_2_player)
        
        #print("跑者上二壘")
      }else if(is.na(table_row_1$base2) == TRUE && is.na(table_row_1$base3) != TRUE && 
               table_row$away == table_row_1$away || table_row$home == table_row_1$home && 
               table_row$out == table_row_1$out){
        total3_2_3 = total3_2_3 + 1 
        total3_2_3_player <- c(total3_2_3_player,table_row$base1)
        total3_2_3_players <- table(total3_2_3_player)
        
        #print("跑者上三壘")
      }else if(is.na(table_row_1$base2) == TRUE && is.na(table_row_1$base3) == TRUE && 
               table_row$away != table_row_1$away || table_row$home != table_row_1$home && 
               table_row$out == table_row_1$out){
        total3_2_4 = total3_2_4 + 1
        total3_2_4_player <- c(total3_2_4_player,table_row$base1)
        total3_2_4_players <- table(total3_2_4_player)
        
        #print("跑者回來得分")
      }
      
    }else if(table_row$out == "二出局" && table_row$direction == "中"){
      total3_3 = total3_3 + 1 
      i = i + 1
      table_row_1 <- cpbl_table[i,]
      if(is.na(table_row_1$base2) == TRUE && is.na(table_row_1$base3) == TRUE && 
         table_row$away == table_row_1$away || table_row$home == table_row_1$home && 
         table_row$out != table_row_1$out){
        total3_3_1 = total3_3_1 + 1
        total3_3_1_player <- c(total3_3_1_player,table_row$base1)
        total3_3_1_players <- table(total3_3_1_player)
        
        #print("跑者出局")
      }else if(is.na(table_row_1$base2) != TRUE && is.na(table_row_1$base3) == TRUE && 
               table_row$away == table_row_1$away || table_row$home == table_row_1$home && 
               table_row$out == table_row_1$out){
        total3_3_2 = total3_3_2 + 1 
        total3_3_2_player <- c(total3_3_2_player,table_row$base1)
        total3_3_2_players <- table(total3_3_2_player)
        
        #print("跑者上二壘")
      }else if(is.na(table_row_1$base2) == TRUE && is.na(table_row_1$base3) != TRUE && 
               table_row$away == table_row_1$away || table_row$home == table_row_1$home && 
               table_row$out == table_row_1$out){
        total3_3_3 = total3_3_3 + 1
        total3_3_3_player <- c(total3_3_3_player,table_row$base1)
        total3_3_3_players <- table(total3_3_3_player)
        
        #print("跑者上三壘")
      }else if(is.na(table_row_1$base2) == TRUE && is.na(table_row_1$base3) == TRUE && 
               table_row$away != table_row_1$away || table_row$home != table_row_1$home && 
               table_row$out == table_row_1$out){
        total3_3_4 = total3_3_4 + 1
        total3_3_4_player <- c(total3_3_4_player,table_row$base1)
        total3_3_4_players <- table(total3_3_4_player)
        
        #print("跑者回來得分")
      }
      
    }else if(table_row$out == "二出局" && table_row$direction == "右"){
      total3_4 = total3_4 + 1
      i = i + 1
      table_row_1 <- cpbl_table[i,]
      if(is.na(table_row_1$base2) == TRUE && is.na(table_row_1$base3) == TRUE && 
         table_row$away == table_row_1$away || table_row$home == table_row_1$home && 
         table_row$out != table_row_1$out){
        total3_4_1 = total3_4_1 + 1
        total3_4_1_player <- c(total3_4_1_player,table_row$base1)
        total3_4_1_players <- table(total3_4_1_player)
        
        #print("跑者出局")
      }else if(is.na(table_row_1$base2) != TRUE && is.na(table_row_1$base3) == TRUE && 
               table_row$away == table_row_1$away || table_row$home == table_row_1$home && 
               table_row$out == table_row_1$out){
        total3_4_2 = total3_4_2 + 1
        total3_4_2_player <- c(total3_4_2_player,table_row$base1)
        total3_4_2players <- table(total3_4_2_player)
        
        #print("跑者上二壘")
      }else if(is.na(table_row_1$base2) == TRUE && is.na(table_row_1$base3) != TRUE && 
               table_row$away == table_row_1$away || table_row$home == table_row_1$home && 
               table_row$out == table_row_1$out){
        total3_4_3 = total3_4_3 + 1  
        total3_4_3_player <- c(total3_4_3_player,table_row$base1)
        total3_4_3_players <- table(total3_4_3_player)
        
        #print("跑者上三壘")
      }else if(is.na(table_row_1$base2) == TRUE && is.na(table_row_1$base3) == TRUE && 
               table_row$away != table_row_1$away || table_row$home != table_row_1$home && 
               table_row$out == table_row_1$out){
        total3_4_4 = total3_4_4 + 1
        total3_4_4_player <- c(total3_4_4_player,table_row$base1)
        total3_4_4_players <- table(total3_4_4_player)
        
        #print("跑者回來得分")
      }
    }
    
    
  }
  
}


#計算status1(零出局)，跑者到兩壘，到三壘，得分，出局的機率

#安打方向(內安)
total1_1_1_prob <- total1_1_1/total1_1
total1_1_2_prob <- total1_1_2/total1_1
total1_1_3_prob <- total1_1_3/total1_1
total1_1_4_prob <- total1_1_4/total1_1

#安打方向(左安)
total1_2_1_prob <- total1_2_1/total1_2
total1_2_2_prob <- total1_2_2/total1_2
total1_2_3_prob <- total1_2_3/total1_2
total1_2_4_prob <- total1_2_4/total1_2

#安打方向(中安)
total1_3_1_prob <- total1_3_1/total1_3
total1_3_2_prob <- total1_3_2/total1_3
total1_3_3_prob <- total1_3_3/total1_3
total1_3_4_prob <- total1_3_4/total1_3

#安打方向(右安)
total1_4_1_prob <- total1_4_1/total1_4
total1_4_2_prob <- total1_4_2/total1_4
total1_4_3_prob <- total1_4_3/total1_4
total1_4_4_prob <- total1_4_4/total1_4


#計算status1(一出局)，跑者到兩壘，到三壘，得分，出局的機率

#安打方向(內安)
total2_1_1_prob <- total2_1_1/total2_1
total2_1_2_prob <- total2_1_2/total2_1
total2_1_3_prob <- total2_1_3/total2_1
total2_1_4_prob <- total2_1_4/total2_1

#安打方向(左安)
total2_2_1_prob <- total2_2_1/total2_2
total2_2_2_prob <- total2_2_2/total2_2
total2_2_3_prob <- total2_2_3/total2_2
total2_2_4_prob <- total2_2_4/total2_2

#安打方向(中安)
total2_3_1_prob <- total2_3_1/total2_3
total2_3_2_prob <- total2_3_2/total2_3
total2_3_3_prob <- total2_3_3/total2_3
total2_3_4_prob <- total2_3_4/total2_3

#安打方向(右安)
total2_4_1_prob <- total2_4_1/total2_4
total2_4_2_prob <- total2_4_2/total2_4
total2_4_3_prob <- total2_4_3/total2_4
total2_4_4_prob <- total2_4_4/total2_4


#計算status1(二出局)，跑者到兩壘，到三壘，得分，出局的機率

#安打方向(內安)
total3_1_1_prob <- total3_1_1/total3_1
total3_1_2_prob <- total3_1_2/total3_1
total3_1_3_prob <- total3_1_3/total3_1
total3_1_4_prob <- total3_1_4/total3_1

#安打方向(左安)
total3_2_1_prob <- total3_2_1/total3_2
total3_2_2_prob <- total3_2_2/total3_2
total3_2_3_prob <- total3_2_3/total3_2
total3_2_4_prob <- total3_2_4/total3_2

#安打方向(中安)
total3_3_1_prob <- total3_3_1/total3_3
total3_3_2_prob <- total3_3_2/total3_3
total3_3_3_prob <- total3_3_3/total3_3
total3_3_4_prob <- total3_3_4/total3_3

#安打方向(右安)
total3_4_1_prob <- total3_4_1/total3_4
total3_4_2_prob <- total3_4_2/total3_4
total3_4_3_prob <- total3_4_3/total3_4
total3_4_4_prob <- total3_4_4/total3_4




