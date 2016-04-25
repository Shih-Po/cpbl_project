cpbl_table <- read.csv(file("D:/BaseballData/output042303.csv", encoding="big5"), header = TRUE, sep = ",",stringsAsFactors = FALSE)
# cpbl_player <- read.table(file("C:/Users/Student/Desktop/CPBL/batter.csv", encoding="UTF-8"), header = TRUE, sep = ",",stringsAsFactors = FALSE)
# player_library <- unique(cpbl_player$name)

total1 <- 0

#零出局
#A<-matrix(1:16,4,4)
total1_1 <- total1_1_1 <- total1_1_2 <- total1_1_3 <- 0
total1_2 <- total1_2_1 <- total1_2_2 <- total1_2_3 <- 0
total1_3 <- total1_3_1 <- total1_3_2 <- total1_3_3 <- 0
total1_4 <- total1_4_1 <- total1_4_2 <- total1_4_3 <- 0

total1_1_1_player <- total1_1_2_player <- total1_1_3_player <-  
total1_2_1_player <- total1_2_2_player <- total1_2_3_player <-  
total1_3_1_player <- total1_3_2_player <- total1_3_3_player <-  
total1_4_1_player <- total1_4_2_player <- total1_4_3_player <-  NULL
#一出局
total2_1 <- total2_1_1 <- total2_1_2 <- total2_1_3 <-  0
total2_2 <- total2_2_1 <- total2_2_2 <- total2_2_3 <-  0
total2_3 <- total2_3_1 <- total2_3_2 <- total2_3_3 <-  0
total2_4 <- total2_4_1 <- total2_4_2 <- total2_4_3 <-  0


total2_1_1_player <- total2_1_2_player <- total2_1_3_player <-  
total2_2_1_player <- total2_2_2_player <- total2_2_3_player <-  
total2_3_1_player <- total2_3_2_player <- total2_3_3_player <-  
total2_4_1_player <- total2_4_2_player <- total2_4_3_player <-  NULL
#兩出局
total3_1 <- total3_1_1 <- total3_1_2 <- total3_1_3 <-  0
total3_2 <- total3_2_1 <- total3_2_2 <- total3_2_3 <-  0
total3_3 <- total3_3_1 <- total3_3_2 <- total3_3_3 <-  0
total3_4 <- total3_4_1 <- total3_4_2 <- total3_4_3 <-  0

total3_1_1_player <- total3_1_2_player <- total3_1_3_player <-  
total3_2_1_player <- total3_2_2_player <- total3_2_3_player <-  
total3_3_1_player <- total3_3_2_player <- total3_3_3_player <-  
total3_4_1_player <- total3_4_2_player <- total3_4_3_player <-  NULL


for (i in 1:nrow(cpbl_table)){
  table_row <- cpbl_table[i,]
  
  
  # j <- i + 1 （$取值）（取每ROW (base1 and base2 and base3 的值如果為NA) and result那row裡的文字為安打類）
  # j <- i + 1
  if (is.na(table_row$base2) !=TRUE && is.na(table_row$base1) == TRUE && is.na(table_row$base3) == TRUE &&
      table_row$result %in% c("一壘安打", "左外野安打", "中外野安打", "右外野安打", 
                              "穿越安打", "平飛安打", "內野安打", "中間方向安打", "德州安打")){
  #(total1 +1迴圈)  
    total1 = total1 + 1 
    
    #if(取out那row值為"零出局" and directio那row為"內")
    #total1_1 +1迴圈 
    #table_row_1 <= cpbl_table每列
    #if(table_row_1的base2 and base3 值為na and table_row的away ＝ and table_row1的away 
    #或 and table_row(原始資料)的home ＝ and table_row1(取原始資料的一列)的home (分數沒動)and out欄)
    #{ total1_1_1 + 1 total1_1_1打者<-打者 }
    #total1_1_1_player為null值避免輸入覆蓋
    
    if (table_row$out == "零出局" && table_row$direction == "內"){
      total1_1 = total1_1 + 1 
      
      i = i + 1
      table_row_1 <- cpbl_table[i,]
      
      #(table_row_1$base1) == NA && table_row_1$base3
      #3壘NA = 2壘有人 
      
      if(is.na(table_row_1$base2) == TRUE && is.na(table_row_1$base3) == TRUE && 
         table_row$away == table_row_1$away || table_row$home == table_row_1$home && 
         table_row$out != table_row_1$out){
        total1_1_1 = total1_1_1 + 1 
        total1_1_1_player <- c(total1_1_1_player,table_row$base2)
        total1_1_1_players <- table(total1_1_1_player)
        # print(table_row$base1)
        #print("跑者出局")
        
      }else if(is.na(table_row_1$base3) != TRUE && is.na(table_row_1$base2) == TRUE &&
               table_row$away == table_row_1$away || table_row$home == table_row_1$home && 
               table_row$out == table_row_1$out){
        total1_1_2 = total1_1_2 + 1 
        total1_1_2_player <- c(total1_1_2_player,table_row$base2)
        total1_1_2_players <- table(total1_1_2_player)
        #print("跑者上三壘")
     
     
      }else if(is.na(table_row_1$base2) == TRUE && is.na(table_row_1$base3) == TRUE && 
               table_row$away != table_row_1$away || table_row$home != table_row_1$home && 
               table_row$out == table_row_1$out){
        total1_1_3 = total1_1_3 + 1 
        total1_1_3_player <- c(total1_1_3_player,table_row$base2)
        total1_1_3_players <- table(total1_1_3_player)
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
        total1_2_1_player <- c(total1_2_1_player,table_row$base2)
        total1_2_1_players <- table(total1_2_1_player)
       #print("跑者出局")
        
      }else if(is.na(table_row_1$base3) != TRUE && is.na(table_row_1$base2) == TRUE && 
               table_row$away == table_row_1$away || table_row$home == table_row_1$home && 
               table_row$out == table_row_1$out){
        total1_2_2 = total1_2_2 + 1
        total1_2_2_player <- c(total1_2_2_player,table_row$base2)
        total1_2_2_players <- table(total1_2_2_player)
        #print("跑者上三壘")
    
      }else if(is.na(table_row_1$base2) == TRUE && is.na(table_row_1$base3) == TRUE && 
               table_row$away != table_row_1$away || table_row$home != table_row_1$home && 
               table_row$out == table_row_1$out){
        total1_2_3 = total1_2_3 + 1
        total1_2_3_player <- c(total1_2_3_player,table_row$base2)
        total1_2_3_players <- table(total1_2_3_player)
        
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
        total1_3_1_player <- c(total1_3_1_player,table_row$base2)
        total1_3_1_players <- table(total1_3_1_player)
        
        #print("跑者出局")
      }else if(is.na(table_row_1$base3) != TRUE && is.na(table_row_1$base2) == TRUE && 
               table_row$away == table_row_1$away || table_row$home == table_row_1$home && 
               table_row$out == table_row_1$out){
        total1_3_2 = total1_3_2 + 1 
        total1_3_2_player <- c(total1_3_2_player,table_row$base2)
        total1_3_2_players <- table(total1_3_2_player)
        #print("跑者上三壘")
        
      }else if(is.na(table_row_1$base2) == TRUE && is.na(table_row_1$base3) == TRUE && 
               table_row$away != table_row_1$away || table_row$home != table_row_1$home && 
               table_row$out == table_row_1$out){
        total1_3_3 = total1_3_3 + 1
        total1_3_3_player <- c(total1_3_3_player,table_row$base2)
        total1_3_3_players <- table(total1_3_3_player)
        
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
        total1_4_1_player <- c(total1_4_1_player,table_row$base2)
        total1_4_1_players <- table(total1_4_1_player)
        
        #print("跑者出局")
      }else if(is.na(table_row_1$base3) != TRUE && is.na(table_row_1$base2) == TRUE && 
               table_row$away == table_row_1$away || table_row$home == table_row_1$home && 
               table_row$out == table_row_1$out){
        total1_4_2 = total1_4_2 + 1
        total1_4_2_player <- c(total1_4_2_player,table_row$base2)
        total1_4_2players <- table(total1_4_2_player)
        #print("跑者上三壘")
        
      }else if(is.na(table_row_1$base2) == TRUE && is.na(table_row_1$base3) == TRUE && 
               table_row$away != table_row_1$away || table_row$home != table_row_1$home && 
               table_row$out == table_row_1$out){
        total1_4_3 = total1_4_3 + 1
        total1_4_3_player <- c(total1_4_3_player,table_row$base2)
        total1_4_3_players <- table(total1_4_3_player)
        
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
        total2_1_1_player <- c(total2_1_1_player,table_row$base2)
        total2_1_1_players <- table(total2_1_1_player)
        
        # print(table_row$base1)
        #print("跑者出局")
      }else if(is.na(table_row_1$base3) != TRUE && is.na(table_row_1$base2) == TRUE && 
               table_row$away == table_row_1$away || table_row$home == table_row_1$home && 
               table_row$out == table_row_1$out){
        total2_1_2 = total2_1_2 + 1 
        total2_1_2_player <- c(total2_1_2_player,table_row$base2)
        total2_1_2_players <- table(total2_1_2_player)
        #print("跑者上三壘")
     
      }else if(is.na(table_row_1$base2) == TRUE && is.na(table_row_1$base3) == TRUE && 
               table_row$away != table_row_1$away || table_row$home != table_row_1$home && 
               table_row$out == table_row_1$out){
        total2_1_3 = total2_1_3 + 1 
        total2_1_3_player <- c(total2_1_3_player,table_row$base2)
        total2_1_3_players <- table(total2_1_3_player)
        
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
        total2_2_1_player <- c(total2_2_1_player,table_row$base2)
        total2_2_1_players <- table(total2_2_1_player)
        
        #print("跑者出局")
      }else if(is.na(table_row_1$base3) != TRUE && is.na(table_row_1$base2) == TRUE && 
               table_row$away == table_row_1$away || table_row$home == table_row_1$home && 
               table_row$out == table_row_1$out){
        total2_2_2 = total2_2_2 + 1
        total2_2_2_player <- c(total2_2_2_player,table_row$base2)
        total2_2_2_players <- table(total2_2_2_player)
        #print("跑者上三壘")
     
      }else if(is.na(table_row_1$base2) == TRUE && is.na(table_row_1$base3) == TRUE && 
               table_row$away != table_row_1$away || table_row$home != table_row_1$home && 
               table_row$out == table_row_1$out){
        total2_2_3 = total2_2_3 + 1
        total2_2_3_player <- c(total2_2_3_player,table_row$base2)
        total2_2_3_players <- table(total2_2_3_player)
        
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
        total2_3_1_player <- c(total2_3_1_player,table_row$base2)
        total2_3_1_players <- table(total2_3_1_player)
        
        #print("跑者出局")
      }else if(is.na(table_row_1$base3) != TRUE && is.na(table_row_1$base2) == TRUE && 
               table_row$away == table_row_1$away || table_row$home == table_row_1$home && 
               table_row$out == table_row_1$out){
        total2_3_2 = total2_3_2 + 1 
        total2_3_2_player <- c(total2_3_2_player,table_row$base2)
        total2_3_2_players <- table(total2_3_2_player)
         #print("跑者上三壘")
      
      }else if(is.na(table_row_1$base2) == TRUE && is.na(table_row_1$base3) == TRUE && 
               table_row$away != table_row_1$away || table_row$home != table_row_1$home && 
               table_row$out == table_row_1$out){
        total2_3_3 = total2_3_3 + 1
        total2_3_3_player <- c(total2_3_3_player,table_row$base2)
        total2_3_3_players <- table(total2_3_3_player)
        
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
        total2_4_1_player <- c(total2_4_1_player,table_row$base2)
        total2_4_1_players <- table(total2_4_1_player)
        
        #print("跑者出局")
      }else if(is.na(table_row_1$base3) != TRUE && is.na(table_row_1$base2) == TRUE && 
               table_row$away == table_row_1$away || table_row$home == table_row_1$home && 
               table_row$out == table_row_1$out){
        total2_4_2 = total2_4_2 + 1
        total2_4_2_player <- c(total2_4_2_player,table_row$base2)
        total2_4_2players <- table(total2_4_2_player)
       #print("跑者上三壘")
    
      }else if(is.na(table_row_1$base2) == TRUE && is.na(table_row_1$base3) == TRUE && 
               table_row$away != table_row_1$away || table_row$home != table_row_1$home && 
               table_row$out == table_row_1$out){
        total2_4_3 = total2_4_4 + 1
        total2_4_3_player <- c(total2_4_3_player,table_row$base2)
        total2_4_3_players <- table(total2_4_3_player)
        
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
        total3_1_1_player <- c(total3_1_1_player,table_row$base2)
        total3_1_1_players <- table(total3_1_1_player)
        
        # print(table_row$base1)
        #print("跑者出局")
      }else if(is.na(table_row_1$base3) != TRUE && is.na(table_row_1$base2) == TRUE && 
               table_row$away == table_row_1$away || table_row$home == table_row_1$home && 
               table_row$out == table_row_1$out){
        total3_1_2 = total3_1_2 + 1 
        total3_1_2_player <- c(total3_1_2_player,table_row$base2)
        total3_1_2_players <- table(total3_1_2_player)
       #print("跑者上三壘")
      
      }else if(is.na(table_row_1$base2) == TRUE && is.na(table_row_1$base3) == TRUE && 
               table_row$away != table_row_1$away || table_row$home != table_row_1$home && 
               table_row$out == table_row_1$out){
        total3_1_3 = total3_1_3 + 1 
        total3_1_3_player <- c(total3_1_3_player,table_row$base2)
        total3_1_3_players <- table(total3_1_3_player)
        
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
        total3_2_1_player <- c(total3_2_1_player,table_row$base2)
        total3_2_1_players <- table(total3_2_1_player)
        
        #print("跑者出局")
      }else if(is.na(table_row_1$base2) != TRUE && is.na(table_row_1$base3) == TRUE && 
               table_row$away == table_row_1$away || table_row$home == table_row_1$home && 
               table_row$out == table_row_1$out){
        total3_2_2 = total3_2_2 + 1
        total3_2_2_player <- c(total3_2_2_player,table_row$base2)
        total3_2_2_players <- table(total3_2_2_player)
        #print("跑者上三壘")
      
      }else if(is.na(table_row_1$base2) == TRUE && is.na(table_row_1$base3) == TRUE && 
               table_row$away != table_row_1$away || table_row$home != table_row_1$home && 
               table_row$out == table_row_1$out){
        total3_2_3 = total3_2_3 + 1
        total3_2_3_player <- c(total3_2_3_player,table_row$base2)
        total3_2_3_players <- table(total3_2_3_player)
        
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
        total3_3_1_player <- c(total3_3_1_player,table_row$base2)
        total3_3_1_players <- table(total3_3_1_player)
        
        #print("跑者出局")
      }else if(is.na(table_row_1$base3) != TRUE && is.na(table_row_1$base2) == TRUE && 
               table_row$away == table_row_1$away || table_row$home == table_row_1$home && 
               table_row$out == table_row_1$out){
        total3_3_2 = total3_3_2 + 1 
        total3_3_2_player <- c(total3_3_2_player,table_row$base2)
        total3_3_2_players <- table(total3_3_2_player)
        #print("跑者上三壘")
      
      }else if(is.na(table_row_1$base2) == TRUE && is.na(table_row_1$base3) == TRUE && 
               table_row$away != table_row_1$away || table_row$home != table_row_1$home && 
               table_row$out == table_row_1$out){
        total3_3_3 = total3_3_3 + 1
        total3_3_3_player <- c(total3_3_3_player,table_row$base2)
        total3_3_3_players <- table(total3_3_3_player)
        
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
        total3_4_1_player <- c(total3_4_1_player,table_row$base2)
        total3_4_1_players <- table(total3_4_1_player)
        
        #print("跑者出局")
      }else if(is.na(table_row_1$base3) != TRUE && is.na(table_row_1$base2) == TRUE && 
               table_row$away == table_row_1$away || table_row$home == table_row_1$home && 
               table_row$out == table_row_1$out){
        total3_4_2 = total3_4_2 + 1
        total3_4_2_player <- c(total3_4_2_player,table_row$base2)
        total3_4_2players <- table(total3_4_2_player)
        #print("跑者上三壘")
   
      }else if(is.na(table_row_1$base2) == TRUE && is.na(table_row_1$base3) == TRUE && 
               table_row$away != table_row_1$away || table_row$home != table_row_1$home && 
               table_row$out == table_row_1$out){
        total3_4_3 = total3_4_3 + 1
        total3_4_3_player <- c(total3_4_3_player,table_row$base2)
        total3_4_3_players <- table(total3_4_3_player)
        
        #print("跑者回來得分")
      }
    }
    
    
  }
  
}
checkamount = 0
checkamount <- total3_1 +total3_2 +total3_3 +total3_4 + total1_1+  total1_2+  total1_3+  total1_4+total2_1+ total2_2+ total2_3+ total2_4 
#計算status1(零出局)，跑者到兩壘，到三壘，得分，出局的機率

#安打方向(內安)
total1_1_1_prob <-  round(total1_1_1/total1_1,digits = 3)
total1_1_2_prob <-  round(total1_1_2/total1_1,digits = 3)
total1_1_3_prob <-  round(total1_1_3/total1_1,digits = 3)

#安打方向(左安)
total1_2_1_prob <-  round(total1_2_1/total1_2,digits = 3)
total1_2_2_prob <-  round(total1_2_2/total1_2,digits = 3)
total1_2_3_prob <-  round(total1_2_3/total1_2,digits = 3)

#安打方向(中安)
total1_3_1_prob <-  round(total1_3_1/total1_3,digits = 3)
total1_3_2_prob <-  round(total1_3_2/total1_3,digits = 3)
total1_3_3_prob <-  round(total1_3_3/total1_3,digits = 3)

#安打方向(右安)
total1_4_1_prob <-  round(total1_4_1/total1_4,digits = 3)
total1_4_2_prob <-  round(total1_4_2/total1_4,digits = 3)
total1_4_3_prob <-  round(total1_4_3/total1_4,digits = 3)


#計算status1(一出局)，跑者到兩壘，到三壘，得分，出局的機率

#安打方向(內安)
total2_1_1_prob <-  round(total2_1_1/total2_1,digits = 3)
total2_1_2_prob <-  round(total2_1_2/total2_1,digits = 3)
total2_1_3_prob <-  round(total2_1_3/total2_1,digits = 3)

#安打方向(左安)
total2_2_1_prob <-  round(total2_2_1/total2_2,digits = 3)
total2_2_2_prob <-  round(total2_2_2/total2_2,digits = 3)
total2_2_3_prob <-  round(total2_2_3/total2_2,digits = 3)

#安打方向(中安)
total2_3_1_prob <-  round(total2_3_1/total2_3,digits = 3)
total2_3_2_prob <-  round(total2_3_2/total2_3,digits = 3)
total2_3_3_prob <-  round(total2_3_3/total2_3,digits = 3)

#安打方向(右安)
total2_4_1_prob <-  round(total2_4_1/total2_4,digits = 3)
total2_4_2_prob <-  round(total2_4_2/total2_4,digits = 3)
total2_4_3_prob <-  round(total2_4_3/total2_4,digits = 3)


#計算status1(兩出局)，跑者到兩壘，到三壘，得分，出局的機率

#安打方向(內安)
total3_1_1_prob <-  round(total3_1_1/total3_1,digits = 3)
total3_1_2_prob <-  round(total3_1_2/total3_1,digits = 3)
total3_1_3_prob <-  round(total3_1_3/total3_1,digits = 3)

#安打方向(左安)
total3_2_1_prob <-  round(total3_2_1/total3_2,digits = 3)
total3_2_2_prob <-  round(total3_2_2/total3_2,digits = 3)
total3_2_3_prob <-  round(total3_2_3/total3_2,digits = 3)

#安打方向(中安)
total3_3_1_prob <-  round(total3_3_1/total3_3,digits = 3)
total3_3_2_prob <-  round(total3_3_2/total3_3,digits = 3)
total3_3_3_prob <-  round(total3_3_3/total3_3,digits = 3)

#安打方向(右安)
total3_4_1_prob <-  round(total3_4_1/total3_4,digits = 3)
total3_4_2_prob <-  round(total3_4_2/total3_4,digits = 3)
total3_4_3_prob <-  round(total3_4_3/total3_4,digits = 3)





list(Outs_Where ="2-other" ,Opp= total3_1, To3rd =total3_1_2_prob, Score= total3_1_3_prob, OA= total3_1_1_prob)
list(Outs_Where ="2-left" ,Opp= total3_2, To3rd =total3_2_2_prob, Score= total3_2_3_prob, OA= total3_2_1_prob)
list(Outs_Where ="2-center" ,Opp= total3_3, To3rd =total3_3_2_prob, Score= total3_3_3_prob, OA= total3_3_1_prob)
list(Outs_Where ="2-right" ,Opp= total3_4, To3rd =total3_4_2_prob, Score= total3_4_3_prob, OA= total3_4_1_prob)




