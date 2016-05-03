# 我認為他文章裡關於 IRP 的算法有很大的問題，你假如去檢驗一下表格中的 IRP，會發現 IRP 並不是像他文章中所說的，是 Ratio of IR to BRR，而應該是 Ratio of BRR to ExR (BRR/ExR)，事實上 IR 其實就是 Difference betweeb BRR and ExR (BRR-ExR)。
# 
# 因此，我懷疑他上面那個描述 BRR 的算法，也就是算說到三壘的得分期望值減去到二壘的得分期望值，根本就是在算 IR而不是 BRR。
# 
# 我認為 BRR ExR 跟 IR 的算法應該如下：
# 
# 假設得分矩陣如第三篇文章所示
# 那麼無人出局一壘有人時的得分期望值為 0.926
# 狀況：0人出局，一壘有人，打者擊出一壘安打，導致現在1,2壘有人。透過得分舉證取得1.467的得分期望。
# ＩＦ：0人出局，一壘有人，打者擊出一壘安打，原本一壘球員積極跑壘，導致1,3壘有人，取得1.854的期望值。
# 
# 因此，當打者打出一壘安打
# 跑者預期也可以上二壘，那麼他這次跑壘的 ExR 應該是1.467 - 0.926 = 0.541
# 然而由於跑者積極跑壘，跑者實際上到了三壘，那麼他這次跑壘的 BRR 就是 1.854 - 0.926 = 0.928
# 然後因為其跑壘而增加的得分 IR 則是 0.387
# 
# 而跑者全部的 ExR BRR IR，就是以上述方法在所有 opportunities 所計算出的總和，而由於 IR 會受到 opportunities 總數的影響，因此要計算 IRP，也就是 BBR/ExR。
# 
# 我認為這才是合理的，請嘗試以此方向計算，並且告訴我你們認為結果是否合理。


#------------------------------------------------------------------------------------------

library(dplyr)
# off_db <- read.csv(file("C:/Users/Student/Desktop/output_1050503.csv", encoding="big5"), 
#                    header = TRUE, sep = "," ,stringsAsFactors = FALSE)
off_db <- readRDS("C:/Users/Student/Desktop/off_db.RDS")
# 1. 設定
# 1-1. BASIC(全聯盟):  (壘包出局情境為 1~24, 且後續得分為 0~100 的資料列)
odb_rtype <- off_db %>% filter(rem_type %in% c(1:24), follow.up %in% c(0:100), !(Player == ""))

# 1-2. TEAM: (客隊上半局為攻擊、主隊下半局為攻擊)
# team <- "統一"
# or_away <- odb_rtype %>% filter(grepl(pattern = team, x = away, fixed = TRUE), grepl(pattern = "上", x = inning, fixed = TRUE)) 
# or_home <- odb_rtype %>% filter(grepl(pattern = team, x = home, fixed = TRUE), grepl(pattern = "下", x = inning, fixed = TRUE)) 
# odb_rtype <- rbind(or_away, or_home)

# 1-3. PLAYER: (以球員名字為篩選條件)
#odb_rtype <- odb_rtype %>% filter(game.player == "胡金龍")


# 2. Output
# 2-1. 計算 24 種 rem_type 的得分期望值
re_list <- list()
for (i in 1:24) {
  odb_r <- odb_rtype %>% filter(rem_type == i)
  r <- round(sum(odb_r$follow.up) / nrow(odb_r), digits = 3) #得分期望值
  #r <- nrow(odb_r) #次數
  re_list[i] <- r
}

# 2-2. set the martix
re_matrix <- matrix(re_list, nrow = 8, byrow=T)
colnames(re_matrix) <- c("out0", "out1", "out2")
rownames(re_matrix) <- c("empty", "1B", "2B", "3B", "1B_2B", "1B_3B", "2B_3B", "1B_2B_3B")


#------------------------------------------------------------------------------------------
## 個人的opp次數
#,lapply( paste0("total3_",i,"_",j,"_","OA","_player") , get)
player_opp<-NULL

for (i in 0:2) {
  for (j in 1:4) {
    
    player_opp<-c(player_opp,lapply( paste0("total1_",i,"_",j,"_","2nd","_player") , get),
                  lapply( paste0("total1_",i,"_",j,"_","3rd","_player") , get),
                  lapply( paste0("total1_",i,"_",j,"_","score","_player") , get),
                  lapply( paste0("total1_",i,"_",j,"_","OA","_player") , get),
                  lapply( paste0("total2_",i,"_",j,"_","3rd","_player") , get),
                  lapply( paste0("total2_",i,"_",j,"_","score","_player") , get),
                  lapply( paste0("total2_",i,"_",j,"_","OA","_player") , get),
                  lapply( paste0("total3_",i,"_",j,"_","3rd","_player") , get),
                  lapply( paste0("total3_",i,"_",j,"_","score","_player") , get))
    
  }
}
players_opp <- as.data.frame(unlist(player_opp)%>%table())
colnames(players_opp) <- c("name","opp_total")
#------------------------------------------------------------------------------------------
## 個人實際跑到哪壘包
#跑者跑到二壘情況(0出局)
to1_2base_0<-NULL

for (j in 1:4) {
  to1_2base_0<-c(to1_2base_0,lapply( paste0("total1_","0","_",j,"_","2nd","_player") , get))
}

to1_2base_0_players <- as.data.frame(unlist(to1_2base_0)%>%table())
colnames(to1_2base_0_players) <- c("name","opp_total")

#跑者跑到二壘情況(1出局)
to1_2base_1<-NULL

for (j in 1:4) {
  to1_2base_1<-c(to1_2base_1,lapply( paste0("total1_","1","_",j,"_","2nd","_player") , get))
}

to1_2base_1_players <- as.data.frame(unlist(to1_2base_1)%>%table())
colnames(to1_2base_1_players) <- c("name","opp_total")

#跑者跑到二壘情況(2出局)
to1_2base_2<-NULL

for (j in 1:4) {
  to1_2base_2<-c(to1_2base_2,lapply( paste0("total1_","2","_",j,"_","2nd","_player") , get))
}

to1_2base_2_players <- as.data.frame(unlist(to1_2base_2)%>%table())
colnames(to1_2base_2_players) <- c("name","opp_total")

#------------------------------------------------------------------------------------------
#跑者跑到三壘情況(0出局)
to1_3base_0<-NULL
to2_3base_0<-NULL
to3_3base_0<-NULL

for (j in 1:4) {
  to1_3base_0<-c(to1_3base_0,lapply( paste0("total1_","0","_",j,"_","3rd","_player") , get))
  to2_3base_0<-c(to2_3base_0,lapply( paste0("total2_","0","_",j,"_","3rd","_player") , get))
  to3_3base_0<-c(to3_3base_0,lapply( paste0("total3_","0","_",j,"_","3rd","_player") , get))
}

to1_3base_0_players <- as.data.frame(unlist(to1_3base_0)%>%table())
to2_3base_0_players <- as.data.frame(unlist(to2_3base_0)%>%table())
to3_3base_0_players <- as.data.frame(unlist(to3_3base_0)%>%table())
colnames(to1_3base_0_players) <- c("name","opp_total")
colnames(to2_3base_0_players) <- c("name","opp_total")
colnames(to3_3base_0_players) <- c("name","opp_total")

#跑者跑到三壘情況(1出局)
to1_3base_1<-NULL
to2_3base_1<-NULL
to3_3base_1<-NULL

for (j in 1:4) {
  to1_3base_1<-c(to1_3base_1,lapply( paste0("total1_","1","_",j,"_","3rd","_player") , get))
  to2_3base_1<-c(to2_3base_1,lapply( paste0("total2_","1","_",j,"_","3rd","_player") , get))
  to3_3base_1<-c(to3_3base_1,lapply( paste0("total3_","1","_",j,"_","3rd","_player") , get))
}

to1_3base_1_players <- as.data.frame(unlist(to1_3base_1)%>%table())
to2_3base_1_players <- as.data.frame(unlist(to2_3base_1)%>%table())
to3_3base_1_players <- as.data.frame(unlist(to3_3base_1)%>%table())
colnames(to1_3base_1_players) <- c("name","opp_total")
colnames(to2_3base_1_players) <- c("name","opp_total")
colnames(to3_3base_1_players) <- c("name","opp_total")

#跑者跑到三壘情況(2出局)
to1_3base_2<-NULL
to2_3base_2<-NULL
to3_3base_2<-NULL

for (j in 1:4) {
  to1_3base_2<-c(to1_3base_2,lapply( paste0("total1_","2","_",j,"_","3rd","_player") , get))
  to2_3base_2<-c(to2_3base_2,lapply( paste0("total2_","2","_",j,"_","3rd","_player") , get))
  to3_3base_2<-c(to3_3base_2,lapply( paste0("total3_","2","_",j,"_","3rd","_player") , get))
}

to1_3base_2_players <- as.data.frame(unlist(to1_3base_2)%>%table())
to2_3base_2_players <- as.data.frame(unlist(to2_3base_2)%>%table())
to3_3base_2_players <- as.data.frame(unlist(to3_3base_2)%>%table())
colnames(to1_3base_2_players) <- c("name","opp_total")
colnames(to2_3base_2_players) <- c("name","opp_total")
colnames(to3_3base_2_players) <- c("name","opp_total")

#------------------------------------------------------------------------------------------
#跑者跑到本壘情況(0出局)
to1_homebase_0<-NULL
to2_homebase_0<-NULL
to3_homebase_0<-NULL

for (j in 1:4) {
  to1_homebase_0<-c(to1_homebase_0,lapply( paste0("total1_","0","_",j,"_","score","_player") , get))
  to2_homebase_0<-c(to2_homebase_0,lapply( paste0("total2_","0","_",j,"_","score","_player") , get))
  to3_homebase_0<-c(to3_homebase_0,lapply( paste0("total3_","0","_",j,"_","score","_player") , get))
}

to1_homebase_0_players <- as.data.frame(unlist(to1_homebase_0)%>%table())
to2_homebase_0_players <- as.data.frame(unlist(to2_homebase_0)%>%table())
to3_homebase_0_players <- as.data.frame(unlist(to3_homebase_0)%>%table())
colnames(to1_homebase_0_players) <- c("name","opp_total")
colnames(to2_homebase_0_players) <- c("name","opp_total")
colnames(to3_homebase_0_players) <- c("name","opp_total")

#跑者跑到本壘情況(1出局)
to1_homebase_1<-NULL
to2_homebase_1<-NULL
to3_homebase_1<-NULL

for (j in 1:4) {
  to1_homebase_1<-c(to1_homebase_1,lapply( paste0("total1_","1","_",j,"_","score","_player") , get))
  to2_homebase_1<-c(to2_homebase_1,lapply( paste0("total2_","1","_",j,"_","score","_player") , get))
  to3_homebase_1<-c(to3_homebase_1,lapply( paste0("total3_","1","_",j,"_","score","_player") , get))
}

to1_homebase_1_players <- as.data.frame(unlist(to1_homebase_1)%>%table())
to2_homebase_1_players <- as.data.frame(unlist(to2_homebase_1)%>%table())
to3_homebase_1_players <- as.data.frame(unlist(to3_homebase_1)%>%table())
colnames(to1_homebase_1_players) <- c("name","opp_total")
colnames(to2_homebase_1_players) <- c("name","opp_total")
colnames(to3_homebase_1_players) <- c("name","opp_total")

#跑者跑到本壘情況(2出局)
to1_homebase_2<-NULL
to2_homebase_2<-NULL
to3_homebase_2<-NULL

for (j in 1:4) {
  to1_homebase_2<-c(to1_homebase_2,lapply( paste0("total1_","2","_",j,"_","score","_player") , get))
  to2_homebase_2<-c(to2_homebase_2,lapply( paste0("total2_","2","_",j,"_","score","_player") , get))
  to3_homebase_2<-c(to3_homebase_2,lapply( paste0("total3_","2","_",j,"_","score","_player") , get))
}

to1_homebase_2_players <- as.data.frame(unlist(to1_homebase_2)%>%table())
to2_homebase_2_players <- as.data.frame(unlist(to2_homebase_2)%>%table())
to3_homebase_2_players <- as.data.frame(unlist(to3_homebase_2)%>%table())
colnames(to1_homebase_2_players) <- c("name","opp_total")
colnames(to2_homebase_2_players) <- c("name","opp_total")
colnames(to3_homebase_2_players) <- c("name","opp_total")

#------------------------------------------------------------------------------------------
#跑者在跑壘時出局的情況(0出局)
## get2_out_0<-c(get2_out_0,lapply( paste0("total2_","0","_",j,"_","OA","_player") , get))
get1_out_0<-NULL
# get2_out_0<-NULL
# get3_out_0<-NULL
# 
for (j in 1:4) {
  get1_out_0<-c(get1_out_0,lapply( paste0("total1_","0","_",j,"_","OA","_player") , get))

#   get3_out_0<-c(get3_out_0,lapply( paste0("total3_","0","_",j,"_","OA","_player") , get))
  }
# 
get1_out_0_players <- as.data.frame(unlist(get1_out_0)%>%table())
# get2_out_0_players <- as.data.frame(unlist(get2_out_0)%>%table())
# get3_out_0_players <- as.data.frame(unlist(get3_out_0)%>%table())
colnames(get1_out_0_players) <- c("name","opp_total")
# colnames(get2_out_0_players) <- c("name","opp_total")
# colnames(get3_out_0_players) <- c("name","opp_total")

#跑者在跑壘時出局的情況(1出局)
#get3_out_1<-c(get3_out_1,lapply( paste0("total3_","1","_",j,"_","OA","_player") , get))
get1_out_1<-NULL
#get2_out_1<-NULL
#get3_out_1<-NULL

for (j in 1:4) {
  get1_out_1<-c(get1_out_1,lapply( paste0("total1_","1","_",j,"_","OA","_player") , get))
  # get2_out_1<-c(get2_out_1,lapply( paste0("total2_","1","_",j,"_","OA","_player") , get))

  }

get1_out_1_players <- as.data.frame(unlist(get1_out_1)%>%table())
# get2_out_1_players <- as.data.frame(unlist(get2_out_1)%>%table())
#get3_out_1_players <- as.data.frame(unlist(get3_out_1)%>%table())
colnames(get1_out_1_players) <- c("name","opp_total")
# colnames(get2_out_1_players) <- c("name","opp_total")
#colnames(get3_out_1_players) <- c("name","opp_total")

#跑者在跑壘時出局的情況(2出局)

# get1_out_2<-NULL
# get2_out_2<-NULL
# get3_out_2<-NULL
# 
# for (j in 1:4) {
#   get1_out_2<-c(get1_out_2,lapply( paste0("total1_","2","_",j,"_","OA","_player") , get))
#   get2_out_2<-c(get2_out_2,lapply( paste0("total2_","2","_",j,"_","OA","_player") , get))
#   get3_out_2<-c(get3_out_2,lapply( paste0("total3_","2","_",j,"_","OA","_player") , get))
#   }
# 
# get1_out_2_players <- as.data.frame(unlist(get1_out_2)%>%table())
# get2_out_2_players <- as.data.frame(unlist(get2_out_2)%>%table())
# get3_out_2_players <- as.data.frame(unlist(get3_out_2)%>%table())
# colnames(get1_out_2_players) <- c("name","opp_total")
# colnames(get2_out_2_players) <- c("name","opp_total")
# colnames(get3_out_2_players) <- c("name","opp_total")


#------------------------------------------------------------------------------------------
##合併所有計算所需表格
#,get1_out_2_players,  get3_out_0_players, get3_out_1_players, get3_out_2_players 
#,get2_out_0_players, get2_out_2_players,  get2_out_1_players,
#因為沒有人出現，所以還無法放入我的表格內

runner_Run_Expectancy <- Reduce(function(x, y) merge(x, y , by="name" , all = T ), 
                                 list(players_opp, to1_2base_0_players, to1_2base_1_players, 
                                      to1_2base_2_players, to1_3base_0_players, to1_3base_1_players,
                                      to1_3base_2_players, to1_homebase_0_players, to1_homebase_1_players,
                                      to1_homebase_2_players, get1_out_0_players, get1_out_1_players, 
                                      to2_3base_0_players, to2_3base_1_players, to2_3base_2_players, 
                                      to2_homebase_0_players, to2_homebase_1_players, to2_homebase_2_players, 
                                                                                      to3_3base_0_players, 
                                      to3_3base_1_players, to3_3base_2_players, to3_homebase_0_players, 
                                      to3_homebase_1_players, to3_homebase_2_players))

colnames(runner_Run_Expectancy) <- c("name","players_opp", "to1_2base_0_players", "to1_2base_1_players", 
                                      "to1_2base_2_players", "to1_3base_0_players", "to1_3base_1_players",
                                      "to1_3base_2_players", "to1_homebase_0_players" , "to1_homebase_1_players",
                                      "to1_homebase_2_players", "get1_out_0_players" , "get1_out_1_players" , 
                                      "to2_3base_0_players", "to2_3base_1_players", "to2_3base_2_players", 
                                      "to2_homebase_0_players", "to2_homebase_1_players", "to2_homebase_2_players", 
                                                                                          "to3_3base_0_players", 
                                      "to3_3base_1_players", "to3_3base_2_players", "to3_homebase_0_players", 
                                      "to3_homebase_1_players", "to3_homebase_2_players")
##----------------------------------------------------------------------------------------

#把runner_Run_Expectancy表裡NA值變為0
runner_Run_Expectancy[is.na(runner_Run_Expectancy)] <- 0

##----------------------------------------------------------------------------------------

#   name      players_opp     to_2base_0_players to_2base_1_players to_2base_2_players to_3base_0_players
# 王勝偉          11                  4                  3                  2                 NA
# to_3base_1_players to_3base_2_players to_homebase_1_players get_out_0_players get_out_1_players
#     NA                  NA                    NA                 NA                NA
# get_out_2_players
#     2

# BRR ExR UR IRP
BRR = (re_matrix[[5,1]]-re_matrix[[2,1]])*runner_Run_Expectancy$to1_2base_0_players + 
      (re_matrix[[5,2]]-re_matrix[[2,2]])*runner_Run_Expectancy$to1_2base_1_players +
      (re_matrix[[5,3]]-re_matrix[[2,3]])*runner_Run_Expectancy$to1_2base_2_players +
  
      (re_matrix[[6,1]]-re_matrix[[2,1]])*runner_Run_Expectancy$to1_3base_0_players +
      (re_matrix[[6,2]]-re_matrix[[2,2]])*runner_Run_Expectancy$to1_3base_1_players +
      (re_matrix[[6,3]]-re_matrix[[2,3]])*runner_Run_Expectancy$to1_3base_2_players +
  
      1*runner_Run_Expectancy$to1_homebase_0_players +
      1*runner_Run_Expectancy$to1_homebase_1_players +
      1*runner_Run_Expectancy$to1_homebase_2_players +
  
      (re_matrix[[2,2]]-re_matrix[[2,1]])*runner_Run_Expectancy$get1_out_0_players +
      (re_matrix[[2,3]]-re_matrix[[2,2]])*runner_Run_Expectancy$get1_out_1_players +
  
      (re_matrix[[6,1]]-re_matrix[[3,1]])*runner_Run_Expectancy$to2_3base_0_players +
      (re_matrix[[6,2]]-re_matrix[[3,2]])*runner_Run_Expectancy$to2_3base_1_players +
      (re_matrix[[6,3]]-re_matrix[[3,3]])*runner_Run_Expectancy$to2_3base_2_players +
  
      (1+re_matrix[[2,1]] - re_matrix[[3,1]])*runner_Run_Expectancy$to2_homebase_0_players +
      (1+re_matrix[[2,2]] - re_matrix[[3,2]])*runner_Run_Expectancy$to2_homebase_1_players +
      (1+re_matrix[[2,3]] - re_matrix[[3,3]])*runner_Run_Expectancy$to2_homebase_2_players +
  
      #(re_matrix[[2,2]]-re_matrix[[3,1]])*runner_Run_Expectancy$get2_out_0_players +
      # (re_matrix[[2,3]]-re_matrix[[3,2]])*runner_Run_Expectancy$get2_out_1_players +
      #(0-re_matrix[[3,3]])*runner_Run_Expectancy$get2_out_2_players +
  
      (re_matrix[[7,1]]-re_matrix[[2,1]])*runner_Run_Expectancy$to3_3base_0_players +
      (re_matrix[[7,2]]-re_matrix[[2,2]])*runner_Run_Expectancy$to3_3base_1_players +
      (re_matrix[[7,3]]-re_matrix[[2,3]])*runner_Run_Expectancy$to3_3base_2_players +
  
      ((1+re_matrix[[3,1]])-re_matrix[[2,1]])*runner_Run_Expectancy$to3_homebase_0_players +
      ((1+re_matrix[[3,2]])-re_matrix[[2,2]])*runner_Run_Expectancy$to3_homebase_1_players +
      ((1+re_matrix[[3,3]])-re_matrix[[2,3]])*runner_Run_Expectancy$to3_homebase_2_players


ExR = (re_matrix[[5,1]]-re_matrix[[2,1]])*(runner_Run_Expectancy$to1_2base_0_players + runner_Run_Expectancy$to1_3base_0_players + 
                                           runner_Run_Expectancy$to1_homebase_0_players + runner_Run_Expectancy$get1_out_0_players) +
      (re_matrix[[5,2]]-re_matrix[[2,2]])*(runner_Run_Expectancy$to1_2base_1_players + runner_Run_Expectancy$to1_3base_1_players +
                                           runner_Run_Expectancy$to1_homebase_1_players + runner_Run_Expectancy$get1_out_1_players) +
      (re_matrix[[5,3]]-re_matrix[[2,3]])*(runner_Run_Expectancy$to1_2base_2_players + runner_Run_Expectancy$to1_3base_2_players +
                                           runner_Run_Expectancy$to1_homebase_2_players) +
      
      (re_matrix[[6,1]]-re_matrix[[3,1]])*(runner_Run_Expectancy$to2_3base_0_players + runner_Run_Expectancy$to2_homebase_0_players) +
      (re_matrix[[6,2]]-re_matrix[[3,2]])*(runner_Run_Expectancy$to2_3base_1_players + runner_Run_Expectancy$to2_homebase_1_players) +
      (re_matrix[[6,3]]-re_matrix[[3,3]])*(runner_Run_Expectancy$to2_3base_2_players + runner_Run_Expectancy$to2_homebase_2_players) +
  
      (re_matrix[[7,1]]-re_matrix[[2,1]])*(runner_Run_Expectancy$to3_3base_0_players + runner_Run_Expectancy$to3_homebase_0_players) +
      (re_matrix[[7,2]]-re_matrix[[2,2]])*(runner_Run_Expectancy$to3_3base_1_players + runner_Run_Expectancy$to3_homebase_1_players) +
      (re_matrix[[7,3]]-re_matrix[[2,3]])*(runner_Run_Expectancy$to3_3base_2_players + runner_Run_Expectancy$to3_homebase_2_players) 

IR = BRR - ExR

IRP = round(BRR/ExR, digits = 3)

Opp = runner_Run_Expectancy$players_opp

player_Run_Expectancy <- data.frame(runner_Run_Expectancy$name,Opp,BRR,ExR,IR,IRP)
View(player_Run_Expectancy)
