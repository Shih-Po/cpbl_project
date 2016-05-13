#rem_operator (.R檔) 的內容

library(dplyr)
off_db <- readRDS("D:/cpbl_project/temp/off_db.RDS")

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
#由status1_table、status2_table、status3_table、rem_operator (.R檔)
#所算出的值，丟進這script裡，能得到每個球員在整季所獲的的機會次數(Opp)
#及在這些機會次數下跑者實際跑到哪個壘包時的壘上狀況得分機率減去原先跑者在哪個壘包的得分機率(BRR)
#，這些機會次數下預期跑者應該跑到哪個壘包時的壘上狀況得分機率減去原先跑者在哪個壘包的得分機率(ExR)
#(一壘安打就是跑一個壘包，二壘安打就是跑兩個壘包)
#(IR=BRR-ExR)，出局(OA)，(IRP=BRR/ExR)
#------------------------------------------------------------------------------------------
#個人在整季下來所獲得機會次數
#由status1_table、status2_table、status3_table (.R檔)情況總數量
#因為 total3_",i,"_",j,"_","OA","_player 沒有球員發生過
#所以把lapply( paste0("total3_",i,"_",j,"_","OA","_player") , get)拿掉
#但以後有發生就要擺入計算
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
#個人實際跑到哪個壘包
#跑者跑到二壘有誰計次數(0出局)
to1_2base_0<-NULL

for (j in 1:4) {
  to1_2base_0<-c(to1_2base_0,lapply( paste0("total1_","0","_",j,"_","2nd","_player") , get))
}

to1_2base_0_players <- as.data.frame(unlist(to1_2base_0)%>%table())
colnames(to1_2base_0_players) <- c("name","opp_total")

#跑者跑到二壘有誰計次數(1出局)
to1_2base_1<-NULL

for (j in 1:4) {
  to1_2base_1<-c(to1_2base_1,lapply( paste0("total1_","1","_",j,"_","2nd","_player") , get))
}

to1_2base_1_players <- as.data.frame(unlist(to1_2base_1)%>%table())
colnames(to1_2base_1_players) <- c("name","opp_total")

#跑者跑到二壘有誰計次數(2出局)
to1_2base_2<-NULL

for (j in 1:4) {
  to1_2base_2<-c(to1_2base_2,lapply( paste0("total1_","2","_",j,"_","2nd","_player") , get))
}

to1_2base_2_players <- as.data.frame(unlist(to1_2base_2)%>%table())
colnames(to1_2base_2_players) <- c("name","opp_total")

#------------------------------------------------------------------------------------------
#跑者跑到三壘有誰計次數(0出局)
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

#跑者跑到三壘有誰計次數(1出局)
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

#跑者跑到三壘有誰計次數(2出局)
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
#跑者跑到本壘有誰計次數(0出局)
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

#跑者跑到本壘有誰計次數(1出局)
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

#跑者跑到本壘有誰計次數(2出局)
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
#跑者在跑壘時出局有誰計次數(0出局)
#get2_out_0<-c(get2_out_0,lapply( paste0("total2_","0","_",j,"_","OA","_player") , get)) 沒有球員發生過
#get3_out_0<-c(get3_out_0,lapply( paste0("total3_","0","_",j,"_","OA","_player") , get)) 沒有球員發生過

get1_out_0<-NULL
# get2_out_0<-NULL
# get3_out_0<-NULL

for (j in 1:4) {
  get1_out_0<-c(get1_out_0,lapply( paste0("total1_","0","_",j,"_","OA","_player") , get))
}
# 
get1_out_0_players <- as.data.frame(unlist(get1_out_0)%>%table())
# get2_out_0_players <- as.data.frame(unlist(get2_out_0)%>%table())
# get3_out_0_players <- as.data.frame(unlist(get3_out_0)%>%table())
colnames(get1_out_0_players) <- c("name","opp_total")
# colnames(get2_out_0_players) <- c("name","opp_total")
# colnames(get3_out_0_players) <- c("name","opp_total")

#跑者在跑壘時出局有誰計次數(1出局)
#get3_out_1<-c(get3_out_1,lapply( paste0("total3_","1","_",j,"_","OA","_player") , get)) 沒有球員發生過

get1_out_1<-NULL
get2_out_1<-NULL
#get3_out_1<-NULL

for (j in 1:4) {
  get1_out_1<-c(get1_out_1,lapply( paste0("total1_","1","_",j,"_","OA","_player") , get))
  get2_out_1<-c(get2_out_1,lapply( paste0("total2_","1","_",j,"_","OA","_player") , get))
}

get1_out_1_players <- as.data.frame(unlist(get1_out_1)%>%table())
get2_out_1_players <- as.data.frame(unlist(get2_out_1)%>%table())
#get3_out_1_players <- as.data.frame(unlist(get3_out_1)%>%table())
colnames(get1_out_1_players) <- c("name","opp_total")
colnames(get2_out_1_players) <- c("name","opp_total")
#colnames(get3_out_1_players) <- c("name","opp_total")

#跑者在跑壘時出局有誰計次數(2出局)
#沒有球員發生過
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
#合併所有計次數變數
#(,get1_out_2_players,  get3_out_0_players, get3_out_1_players, get3_out_2_players 
#,get2_out_0_players, get2_out_2_players,) 沒有球員發生，所以不放入

runner_Run_Expectancy <- Reduce(function(x, y) merge(x, y , by="name" , all = T ), 
                                list(players_opp, to1_2base_0_players, to1_2base_1_players, 
                                     to1_2base_2_players, to1_3base_0_players, to1_3base_1_players,
                                     to1_3base_2_players, to1_homebase_0_players, to1_homebase_1_players,
                                     to1_homebase_2_players, get1_out_0_players, get1_out_1_players, 
                                     to2_3base_0_players, to2_3base_1_players, to2_3base_2_players, 
                                     to2_homebase_0_players, to2_homebase_1_players, to2_homebase_2_players, 
                                     get2_out_1_players, to3_3base_0_players, to3_3base_1_players, 
                                     to3_3base_2_players, to3_homebase_0_players, to3_homebase_1_players, 
                                     to3_homebase_2_players))

colnames(runner_Run_Expectancy) <- c("name","players_opp", "to1_2base_0_players", "to1_2base_1_players", 
                                     "to1_2base_2_players", "to1_3base_0_players", "to1_3base_1_players",
                                     "to1_3base_2_players", "to1_homebase_0_players" , "to1_homebase_1_players",
                                     "to1_homebase_2_players", "get1_out_0_players" , "get1_out_1_players" , 
                                     "to2_3base_0_players", "to2_3base_1_players", "to2_3base_2_players", 
                                     "to2_homebase_0_players", "to2_homebase_1_players", "to2_homebase_2_players", 
                                     "get2_out_1_players", "to3_3base_0_players", "to3_3base_1_players", 
                                     "to3_3base_2_players", "to3_homebase_0_players", "to3_homebase_1_players",
                                     "to3_homebase_2_players")

#把runner_Run_Expectancy表裡NA值以0取代
runner_Run_Expectancy[is.na(runner_Run_Expectancy)] <- 0

#----------------------------------------------------------------------------------------
#(,get1_out_2_players,  get3_out_0_players, get3_out_1_players, get3_out_2_players 
#,get2_out_0_players, get2_out_2_players,) 沒有球員發生，所以不放入

Name = runner_Run_Expectancy$name

Opp = runner_Run_Expectancy$players_opp

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
  (re_matrix[[2,3]]-re_matrix[[3,2]])*runner_Run_Expectancy$get2_out_1_players +
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
  (re_matrix[[6,2]]-re_matrix[[3,2]])*(runner_Run_Expectancy$to2_3base_1_players + runner_Run_Expectancy$to2_homebase_1_players + 
                                         runner_Run_Expectancy$get2_out_1_players) +
  (re_matrix[[6,3]]-re_matrix[[3,3]])*(runner_Run_Expectancy$to2_3base_2_players + runner_Run_Expectancy$to2_homebase_2_players) +
  
  (re_matrix[[7,1]]-re_matrix[[2,1]])*(runner_Run_Expectancy$to3_3base_0_players + runner_Run_Expectancy$to3_homebase_0_players) +
  (re_matrix[[7,2]]-re_matrix[[2,2]])*(runner_Run_Expectancy$to3_3base_1_players + runner_Run_Expectancy$to3_homebase_1_players) +
  (re_matrix[[7,3]]-re_matrix[[2,3]])*(runner_Run_Expectancy$to3_3base_2_players + runner_Run_Expectancy$to3_homebase_2_players) 

IR = BRR - ExR

IRP = round(BRR/ExR, digits = 3)

player_Run_Expectancy <- data.frame(runner_Run_Expectancy$name,Opp,BRR,ExR,IR,IRP)
View(player_Run_Expectancy)