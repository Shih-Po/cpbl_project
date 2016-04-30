# Runner on first, second not occupied, and the batter singles

# out/where  opp          To2nd              To3rd             Score              OA
# 0 other  total1_1 | total1_1_2_prob | total1_1_3_prob | total1_1_4_prob | total1_1_1_prob
# 0 left   total1_2 | total1_2_2_prob | total1_2_3_prob | total1_2_4_prob | total1_2_1_prob
# 0 center total1_3 | total1_3_2_prob | total1_3_3_prob | total1_3_4_prob | total1_3_1_prob
# 0 right  total1_4 | total1_4_2_prob | total1_4_3_prob | total1_4_4_prob | total1_4_1_prob  
# 
# 1 other  total2_1 | total2_1_2_prob | total2_1_3_prob | total2_1_4_prob | total2_1_1_prob
# 1 left   total2_2 | total2_2_2_prob | total2_2_3_prob | total2_2_4_prob | total2_2_1_prob
# 1 center total2_3 | total2_3_2_prob | total2_3_3_prob | total2_3_4_prob | total2_3_1_prob
# 1 right  total2_4 | total2_4_2_prob | total2_4_3_prob | total2_4_4_prob | total2_4_1_prob
# 
# 2 other  total3_1 | total3_1_2_prob | total3_1_3_prob | total3_1_4_prob | total3_1_1_prob
# 2 left   total3_2 | total3_2_2_prob | total3_2_3_prob | total3_2_4_prob | total3_2_1_prob
# 2 center total3_3 | total3_3_2_prob | total3_3_3_prob | total3_3_4_prob | total3_3_1_prob
# 2 right  total3_4 | total3_4_2_prob | total3_4_3_prob | total3_4_4_prob | total3_4_1_prob
library(dplyr)
##status1 各個opp的EB
#0 other,0 left,0 center,0 right
for (i in 1:4) {
  # opp_EB_0_i <- 1*total1_i_2_prob + 2*total1_i_3_prob + 3*total1_i_4_prob - total1_i_1_prob
  assign(paste0("opp1_EB_0_",i), 1*get(paste0("prob1_","0_",i,"_2nd")) + 
           2*get(paste0("prob1_","0_",i,"_3rd")) +
           3*get(paste0("prob1_","0_",i,"_score")) -
           1*get(paste0("prob1_","0_",i,"_OA")) )
  
  assign(paste0("opp2_EB_0_",i), 1*get(paste0("prob2_","0_",i,"_3rd")) + 
           2*get(paste0("prob2_","0_",i,"_score")) -
           1*get(paste0("prob2_","0_",i,"_OA")) )
  
  assign(paste0("opp3_EB_0_",i), 1*get(paste0("prob3_","0_",i,"_3rd")) + 
           2*get(paste0("prob3_","0_",i,"_score")) -
           1*get(paste0("prob3_","0_",i,"_OA")) )
  
}
#1 other,1 left,1 center,1 right
for (i in 1:4) {
  assign(paste0("opp1_EB_1_",i), 1*get(paste0("prob1_","1_",i,"_2nd")) + 
           2*get(paste0("prob1_","1_",i,"_3rd")) +
           3*get(paste0("prob1_","1_",i,"_score")) -
           1*get(paste0("prob1_","1_",i,"_OA")) )
  
  assign(paste0("opp2_EB_1_",i), 1*get(paste0("prob2_","1_",i,"_3rd")) + 
           2*get(paste0("prob2_","1_",i,"_score")) -
           1*get(paste0("prob2_","1_",i,"_OA")) )
  
  assign(paste0("opp3_EB_1_",i), 1*get(paste0("prob3_","1_",i,"_3rd")) + 
           2*get(paste0("prob3_","1_",i,"_score")) -
           1*get(paste0("prob3_","1_",i,"_OA")) )
}
#2 other,2 left,2 center,2 right
for (i in 1:4) {
  assign(paste0("opp1_EB_2_",i), 1*get(paste0("prob1_","2_",i,"_2nd")) + 
           2*get(paste0("prob1_","2_",i,"_3rd")) +
           3*get(paste0("prob1_","2_",i,"_score")) -
           1*get(paste0("prob1_","2_",i,"_OA")) )
  
  assign(paste0("opp2_EB_2_",i), 1*get(paste0("prob2_","2_",i,"_3rd")) + 
           2*get(paste0("prob2_","2_",i,"_score")) -
           1*get(paste0("prob2_","2_",i,"_OA")) )
  
  assign(paste0("opp3_EB_2_",i), 1*get(paste0("prob3_","2_",i,"_3rd")) + 
           2*get(paste0("prob3_","2_",i,"_score")) -
           1*get(paste0("prob3_","2_",i,"_OA")) )
}
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
## 個人實際跑了多少壘包
#跑者跑到二壘情況(得一壘包)
get_1base<-NULL

for (i in 0:2) {
  for (j in 1:4) {
    get_1base<-c(get_1base,lapply( paste0("total1_",i,"_",j,"_","2nd","_player") , get),
                 lapply( paste0("total2_",i,"_",j,"_","3rd","_player") , get),
                 lapply( paste0("total3_",i,"_",j,"_","3rd","_player") , get))
    }
  }

get_1base_players <- as.data.frame(unlist(get_1base)%>%table())
colnames(get_1base_players) <- c("name","opp_total")

#------------------------------------------------------------------------------------------
#跑者跑到三壘情況(得兩壘包)
get_2base<-NULL

for (i in 0:2) {
  for (j in 1:4) {
    get_2base<-c(get_2base,lapply( paste0("total1_",i,"_",j,"_","3rd","_player") , get),
                 lapply( paste0("total2_",i,"_",j,"_","score","_player") , get),
                 lapply( paste0("total3_",i,"_",j,"_","score","_player") , get))
  }
}

get_2bases_players <- as.data.frame(unlist(get_2base)%>%table())
colnames(get_2bases_players) <- c("name","opp_total")

#------------------------------------------------------------------------------------------
#跑者跑回本壘情況(得三壘包)
get_3base<-NULL

for (i in 0:2) {
  for (j in 1:4) {
    get_3base<-c(get_3base,lapply( paste0("total1_",i,"_",j,"_","score","_player") , get))
  }
}

get_3bases_players <- as.data.frame(unlist(get_3base)%>%table())
colnames(get_3bases_players) <- c("name","opp_total")

#------------------------------------------------------------------------------------------
## 個人預期跑多少壘包
# 0  Other,在這種情況下，每個opp所獲得EB(opp_EB_0_1)
#,lapply( paste0("total3_","0","_","1","_","OA","_player") , get)
get1_0_other<-NULL
get2_0_other<-NULL
get3_0_other<-NULL

  get1_0_other<-c(get1_0_other,lapply( paste0("total1_","0","_","1","_","2nd","_player") , get),
                 lapply( paste0("total1_","0","_","1","_","3rd","_player") , get),
                 lapply( paste0("total1_","0","_","1","_","score","_player") , get),
                 lapply( paste0("total1_","0","_","1","_","OA","_player") , get))
                 
  get2_0_other<-c(get2_0_other,lapply( paste0("total2_","0","_","1","_","3rd","_player") , get),
                 lapply( paste0("total2_","0","_","1","_","score","_player") , get),
                 lapply( paste0("total2_","0","_","1","_","OA","_player") , get))
  
  get3_0_other<-c(get3_0_other,lapply( paste0("total3_","0","_","1","_","3rd","_player") , get),
                 lapply( paste0("total3_","0","_","1","_","score","_player") , get))
  

get1_0_other_EB <- as.data.frame(unlist(get1_0_other)%>%table())
get2_0_other_EB <- as.data.frame(unlist(get2_0_other)%>%table())
get3_0_other_EB <- as.data.frame(unlist(get3_0_other)%>%table())
colnames(get1_0_other_EB) <- c("name","opp_total")
colnames(get2_0_other_EB) <- c("name","opp_total")
colnames(get3_0_other_EB) <- c("name","opp_total")

# 0  left,在這種情況下，每個opp所獲得EB(opp_EB_0_2)
#,lapply( paste0("total3_","0","_","2","_","OA","_player") , get)
get1_0_left<-NULL
get2_0_left<-NULL
get3_0_left<-NULL

  
  get1_0_left<-c(get1_0_left,lapply( paste0("total1_","0","_","2","_","2nd","_player") , get),
                lapply( paste0("total1_","0","_","2","_","3rd","_player") , get),
                lapply( paste0("total1_","0","_","2","_","score","_player") , get),
                lapply( paste0("total1_","0","_","2","_","OA","_player") , get))
  
  get2_0_left<-c(get2_0_left,lapply( paste0("total2_","0","_","2","_","3rd","_player") , get),
                lapply( paste0("total2_","0","_","2","_","score","_player") , get),
                lapply( paste0("total2_","0","_","2","_","OA","_player") , get))
  
  get3_0_left<-c(get3_0_left,lapply( paste0("total3_","0","_","2","_","3rd","_player") , get),
                lapply( paste0("total3_","0","_","2","_","score","_player") , get))
  

get1_0_left_EB <- as.data.frame(unlist(get1_0_left)%>%table())
get2_0_left_EB <- as.data.frame(unlist(get2_0_left)%>%table())
get3_0_left_EB <- as.data.frame(unlist(get3_0_left)%>%table())
colnames(get1_0_left_EB) <- c("name","opp_total")
colnames(get2_0_left_EB) <- c("name","opp_total")
colnames(get3_0_left_EB) <- c("name","opp_total")

# 0  center,在這種情況下，每個opp所獲得EB(opp_EB_0_3)
#,lapply( paste0("total3_","0","_","3","_","OA","_player") , get)
get1_0_center<-NULL
get2_0_center<-NULL
get3_0_center<-NULL

  
  get1_0_center<-c(get1_0_center,lapply( paste0("total1_","0","_","3","_","2nd","_player") , get),
                  lapply( paste0("total1_","0","_","3","_","3rd","_player") , get),
                  lapply( paste0("total1_","0","_","3","_","score","_player") , get),
                  lapply( paste0("total1_","0","_","3","_","OA","_player") , get))
  get2_0_center<-c(get2_0_center,lapply( paste0("total2_","0","_","3","_","3rd","_player") , get),
                  lapply( paste0("total2_","0","_","3","_","score","_player") , get),
                  lapply( paste0("total2_","0","_","3","_","OA","_player") , get))
  get3_0_center<-c(get3_0_center,lapply( paste0("total3_","0","_","3","_","3rd","_player") , get),
                  lapply( paste0("total3_","0","_","3","_","score","_player") , get))
  

get1_0_center_EB <- as.data.frame(unlist(get1_0_center)%>%table())
get2_0_center_EB <- as.data.frame(unlist(get2_0_center)%>%table())
get3_0_center_EB <- as.data.frame(unlist(get3_0_center)%>%table())
colnames(get1_0_center_EB) <- c("name","opp_total")
colnames(get2_0_center_EB) <- c("name","opp_total")
colnames(get3_0_center_EB) <- c("name","opp_total")

# 0  right,在這種情況下，每個opp所獲得EB(opp_EB_0_4)
#, lapply( paste0("total3_","0","_","4","_","OA","_player") , get)
get1_0_right<-NULL
get2_0_right<-NULL
get3_0_right<-NULL

  
  get1_0_right<-c(get1_0_right,lapply( paste0("total1_","0","_","4","_","2nd","_player") , get),
                 lapply( paste0("total1_","0","_","4","_","3rd","_player") , get),
                 lapply( paste0("total1_","0","_","4","_","score","_player") , get),
                 lapply( paste0("total1_","0","_","4","_","OA","_player") , get))
  get2_0_right<-c(get2_0_right,lapply( paste0("total2_","0","_","4","_","3rd","_player") , get),
                 lapply( paste0("total2_","0","_","4","_","score","_player") , get),
                 lapply( paste0("total2_","0","_","4","_","OA","_player") , get))
  get3_0_right<-c(get3_0_right,lapply( paste0("total3_","0","_","4","_","3rd","_player") , get),
                 lapply( paste0("total3_","0","_","4","_","score","_player") , get))
  

get1_0_right_EB <- as.data.frame(unlist(get1_0_right)%>%table())
get2_0_right_EB <- as.data.frame(unlist(get2_0_right)%>%table())
get3_0_right_EB <- as.data.frame(unlist(get3_0_right)%>%table())
colnames(get1_0_right_EB) <- c("name","opp_total")
colnames(get2_0_right_EB) <- c("name","opp_total")
colnames(get3_0_right_EB) <- c("name","opp_total")

#------------------------------------------------------------------------------------------
# 1  Other,在這種情況下，每個opp所獲得EB(opp_EB_1_1)
#,lapply( paste0("total3_","1","_","1","_","OA","_player") , get)
get1_1_other<-NULL
get2_1_other<-NULL
get3_1_other<-NULL

  
  get1_1_other<-c(get1_1_other,lapply( paste0("total1_","1","_","1","_","2nd","_player") , get),
                 lapply( paste0("total1_","1","_","1","_","3rd","_player") , get),
                 lapply( paste0("total1_","1","_","1","_","score","_player") , get),
                 lapply( paste0("total1_","1","_","1","_","OA","_player") , get))
  get2_1_other<-c(get2_1_other,lapply( paste0("total2_","1","_","1","_","3rd","_player") , get),
                 lapply( paste0("total2_","1","_","1","_","score","_player") , get),
                 lapply( paste0("total2_","1","_","1","_","OA","_player") , get))
  get3_1_other<-c(get3_1_other,lapply( paste0("total3_","1","_","1","_","3rd","_player") , get),
                 lapply( paste0("total3_","1","_","1","_","score","_player") , get))
  

get1_1_other_EB <- as.data.frame(unlist(get1_1_other)%>%table())
get2_1_other_EB <- as.data.frame(unlist(get2_1_other)%>%table())
get3_1_other_EB <- as.data.frame(unlist(get3_1_other)%>%table())
colnames(get1_1_other_EB) <- c("name","opp_total")
colnames(get2_1_other_EB) <- c("name","opp_total")
colnames(get3_1_other_EB) <- c("name","opp_total")

# 1  left,在這種情況下，每個opp所獲得EB(opp_EB_1_2)
#,lapply( paste0("total3_","1","_","2","_","OA","_player") , get)
get1_1_left<-NULL
get2_1_left<-NULL
get3_1_left<-NULL
  
  get1_1_left<-c(get1_1_left,lapply( paste0("total1_","1","_","2","_","2nd","_player") , get),
                lapply( paste0("total1_","1","_","2","_","3rd","_player") , get),
                lapply( paste0("total1_","1","_","2","_","score","_player") , get),
                lapply( paste0("total1_","1","_","2","_","OA","_player") , get))
  get2_1_left<-c(get2_1_left,lapply( paste0("total2_","1","_","2","_","3rd","_player") , get),
                lapply( paste0("total2_","1","_","2","_","score","_player") , get),
                lapply( paste0("total2_","1","_","2","_","OA","_player") , get))
  get3_1_left<-c(get3_1_left,lapply( paste0("total3_","1","_","2","_","3rd","_player") , get),
                lapply( paste0("total3_","1","_","2","_","score","_player") , get))
  

get1_1_left_EB <- as.data.frame(unlist(get1_1_left)%>%table())
get2_1_left_EB <- as.data.frame(unlist(get2_1_left)%>%table())
get3_1_left_EB <- as.data.frame(unlist(get3_1_left)%>%table())
colnames(get1_1_left_EB) <- c("name","opp_total")
colnames(get2_1_left_EB) <- c("name","opp_total")
colnames(get3_1_left_EB) <- c("name","opp_total")

# 1  center,在這種情況下，每個opp所獲得EB(opp_EB_1_3)
#,lapply( paste0("total3_","1","_","3","_","OA","_player") , get)
get1_1_center<-NULL
get2_1_center<-NULL
get3_1_center<-NULL

  
  get1_1_center<-c(get1_1_center,lapply( paste0("total1_","1","_","3","_","2nd","_player") , get),
                  lapply( paste0("total1_","1","_","3","_","3rd","_player") , get),
                  lapply( paste0("total1_","1","_","3","_","score","_player") , get),
                  lapply( paste0("total1_","1","_","3","_","OA","_player") , get))
  get2_1_center<-c(get2_1_center,lapply( paste0("total2_","1","_","3","_","3rd","_player") , get),
                  lapply( paste0("total2_","1","_","3","_","score","_player") , get),
                  lapply( paste0("total2_","1","_","3","_","OA","_player") , get))
  get3_1_center<-c(get3_1_center,lapply( paste0("total3_","1","_","3","_","3rd","_player") , get),
                  lapply( paste0("total3_","1","_","3","_","score","_player") , get))
  

get1_1_center_EB <- as.data.frame(unlist(get1_1_center)%>%table())
get2_1_center_EB <- as.data.frame(unlist(get2_1_center)%>%table())
get3_1_center_EB <- as.data.frame(unlist(get3_1_center)%>%table())
colnames(get1_1_center_EB) <- c("name","opp_total")
colnames(get2_1_center_EB) <- c("name","opp_total")
colnames(get3_1_center_EB) <- c("name","opp_total")

# 1  right,在這種情況下，每個opp所獲得EB(opp_EB_1_4)
#,lapply( paste0("total3_","1","_","4","_","OA","_player") , get)
get1_1_right<-NULL
get2_1_right<-NULL
get3_1_right<-NULL

  
  get1_1_right<-c(get1_1_right,lapply( paste0("total1_","1","_","4","_","2nd","_player") , get),
                 lapply( paste0("total1_","1","_","4","_","3rd","_player") , get),
                 lapply( paste0("total1_","1","_","4","_","score","_player") , get),
                 lapply( paste0("total1_","1","_","4","_","OA","_player") , get))
  get2_1_right<-c(get2_1_right,lapply( paste0("total2_","1","_","4","_","3rd","_player") , get),
                 lapply( paste0("total2_","1","_","4","_","score","_player") , get),
                 lapply( paste0("total2_","1","_","4","_","OA","_player") , get))
  get3_1_right<-c(get3_1_right,lapply( paste0("total3_","1","_","4","_","3rd","_player") , get),
                 lapply( paste0("total3_","1","_","4","_","score","_player") , get))


get1_1_right_EB <- as.data.frame(unlist(get1_1_right)%>%table())
get2_1_right_EB <- as.data.frame(unlist(get2_1_right)%>%table())
get3_1_right_EB <- as.data.frame(unlist(get3_1_right)%>%table())
colnames(get1_1_right_EB) <- c("name","opp_total")
colnames(get2_1_right_EB) <- c("name","opp_total")
colnames(get3_1_right_EB) <- c("name","opp_total")

#------------------------------------------------------------------------------------------
# 2  Other,在這種情況下，每個opp所獲得EB(opp_EB_2_1)
#,lapply( paste0("total3_","2","_","1","_","OA","_player") , get)
get1_2_other<-NULL
get2_2_other<-NULL
get3_2_other<-NULL

  
  get1_2_other<-c(get1_2_other,lapply( paste0("total1_","2","_","1","_","2nd","_player") , get),
                 lapply( paste0("total1_","2","_","1","_","3rd","_player") , get),
                 lapply( paste0("total1_","2","_","1","_","score","_player") , get),
                 lapply( paste0("total1_","2","_","1","_","OA","_player") , get))
  get2_2_other<-c(get2_2_other,lapply( paste0("total2_","2","_","1","_","3rd","_player") , get),
                 lapply( paste0("total2_","2","_","1","_","score","_player") , get),
                 lapply( paste0("total2_","2","_","1","_","OA","_player") , get))
  get3_2_other<-c(get3_2_other,lapply( paste0("total3_","2","_","1","_","3rd","_player") , get),
                 lapply( paste0("total3_","2","_","1","_","score","_player") , get))
  

get1_2_other_EB <- as.data.frame(unlist(get1_2_other)%>%table())
get2_2_other_EB <- as.data.frame(unlist(get2_2_other)%>%table())
get3_2_other_EB <- as.data.frame(unlist(get3_2_other)%>%table())
colnames(get1_2_other_EB) <- c("name","opp_total")
colnames(get2_2_other_EB) <- c("name","opp_total")
colnames(get3_2_other_EB) <- c("name","opp_total")

# 2  left,在這種情況下，每個opp所獲得EB(opp_EB_2_2)
#,lapply( paste0("total3_","2","_","2","_","OA","_player") , get)
get1_2_left<-NULL
get2_2_left<-NULL
get3_2_left<-NULL

  
  get1_2_left<-c(get1_1_left,lapply( paste0("total1_","2","_","2","_","2nd","_player") , get),
                lapply( paste0("total1_","2","_","2","_","3rd","_player") , get),
                lapply( paste0("total1_","2","_","2","_","score","_player") , get),
                lapply( paste0("total1_","2","_","2","_","OA","_player") , get))
  get2_2_left<-c(get2_1_left,lapply( paste0("total2_","2","_","2","_","3rd","_player") , get),
                lapply( paste0("total2_","2","_","2","_","score","_player") , get),
                lapply( paste0("total2_","2","_","2","_","OA","_player") , get))
  get3_2_left<-c(get3_1_left,lapply( paste0("total3_","2","_","2","_","3rd","_player") , get),
                lapply( paste0("total3_","2","_","2","_","score","_player") , get))
  

get1_2_left_EB <- as.data.frame(unlist(get1_2_left)%>%table())
get2_2_left_EB <- as.data.frame(unlist(get2_2_left)%>%table())
get3_2_left_EB <- as.data.frame(unlist(get3_2_left)%>%table())
colnames(get1_2_left_EB) <- c("name","opp_total")
colnames(get2_2_left_EB) <- c("name","opp_total")
colnames(get3_2_left_EB) <- c("name","opp_total")

# 2  center,在這種情況下，每個opp所獲得EB(opp_EB_2_3)
#,lapply( paste0("total3_","2","_","3","_","OA","_player") , get)
get1_2_center<-NULL
get2_2_center<-NULL
get3_2_center<-NULL

  
  get1_2_center<-c(get1_2_center,lapply( paste0("total1_","2","_","3","_","2nd","_player") , get),
                  lapply( paste0("total1_","2","_","3","_","3rd","_player") , get),
                  lapply( paste0("total1_","2","_","3","_","score","_player") , get),
                  lapply( paste0("total1_","2","_","3","_","OA","_player") , get))
  get2_2_center<-c(get2_2_center,lapply( paste0("total2_","2","_","3","_","3rd","_player") , get),
                  lapply( paste0("total2_","2","_","3","_","score","_player") , get),
                  lapply( paste0("total2_","2","_","3","_","OA","_player") , get))
  get3_2_center<-c(get3_2_center,lapply( paste0("total3_","2","_","3","_","3rd","_player") , get),
                  lapply( paste0("total3_","2","_","3","_","score","_player") , get))
  

get1_2_center_EB <- as.data.frame(unlist(get1_2_center)%>%table())
get2_2_center_EB <- as.data.frame(unlist(get2_2_center)%>%table())
get3_2_center_EB <- as.data.frame(unlist(get3_2_center)%>%table())
colnames(get1_2_center_EB) <- c("name","opp_total")
colnames(get2_2_center_EB) <- c("name","opp_total")
colnames(get3_2_center_EB) <- c("name","opp_total")

# 2  right,在這種情況下，每個opp所獲得EB(opp_EB_2_4)
#,lapply( paste0("total3_","2","_","4","_","OA","_player") , get)
get1_2_right<-NULL
get2_2_right<-NULL
get3_2_right<-NULL

  
  get1_2_right<-c(get1_2_right,lapply( paste0("total1_","2","_","4","_","2nd","_player") , get),
                 lapply( paste0("total1_","2","_","4","_","3rd","_player") , get),
                 lapply( paste0("total1_","2","_","4","_","score","_player") , get),
                 lapply( paste0("total1_","2","_","4","_","OA","_player") , get))
  get2_2_right<-c(get2_2_right,lapply( paste0("total2_","2","_","4","_","3rd","_player") , get),
                 lapply( paste0("total2_","2","_","4","_","score","_player") , get),
                 lapply( paste0("total2_","2","_","4","_","OA","_player") , get))
  get3_2_right<-c(get3_2_right,lapply( paste0("total3_","2","_","4","_","3rd","_player") , get),
                 lapply( paste0("total3_","2","_","4","_","score","_player") , get))
  

get1_2_right_EB <- as.data.frame(unlist(get1_2_right)%>%table())
get2_2_right_EB <- as.data.frame(unlist(get2_2_right)%>%table())
get3_2_right_EB <- as.data.frame(unlist(get3_2_right)%>%table())
colnames(get1_2_right_EB) <- c("name","opp_total")
colnames(get2_2_right_EB) <- c("name","opp_total")
colnames(get3_2_right_EB) <- c("name","opp_total")

#------------------------------------------------------------------------------------------
## 個人在跑壘時出局的情況次數
#,lapply( paste0("total3_",i,"_",j,"_","OA","_player") , get)
get_out<-NULL

for (i in 0:2) {
  for (j in 1:4) {
    get_out<-c(get_out,lapply( paste0("total1_",i,"_",j,"_","OA","_player") , get),
               lapply( paste0("total2_",i,"_",j,"_","OA","_player") , get))
  }
}

get_out_players <- as.data.frame(unlist(get_out)%>%table())
colnames(get_out_players) <- c("name","opp_total")



##----------------------------------------------------------------------------------------
#合併所有計算所需表格
#get3_0_other_EB, get3_2_other_EB, get3_1_other_EB,
runner_number_of_times <- Reduce(function(x, y) merge(x, y , by="name" , all = T ), 
       list(players_opp, get_1base_players, get_2bases_players, 
            get_3bases_players, get_out_players, get1_0_other_EB,
            get1_0_left_EB, get1_0_center_EB, get1_0_right_EB,
            get1_1_other_EB, get1_1_left_EB, get1_1_center_EB,
            get1_1_right_EB, get1_2_other_EB, get1_2_left_EB,
            get1_2_center_EB, get1_2_right_EB, get2_0_other_EB,
            get2_0_left_EB, get2_0_center_EB, get2_0_right_EB,
            get2_1_other_EB, get2_1_left_EB, get2_1_center_EB,
            get2_1_right_EB, get2_2_other_EB, get2_2_left_EB,
            get2_2_center_EB, get2_2_right_EB, 
            get3_0_left_EB, get3_0_center_EB, get3_0_right_EB,
                            get3_1_left_EB, get3_1_center_EB,
            get3_1_right_EB,                 get3_2_left_EB,
            get3_2_center_EB, get3_2_right_EB))

colnames(runner_number_of_times) <- c("name","players_opp", "get_1base_players", "get_2bases_players", 
                               "get_3bases_players", "get_out_players", "get1_0_other_EB",
                               "get1_0_left_EB", "get1_0_center_EB", "get1_0_right_EB",
                               "get1_1_other_EB", "get1_1_left_EB", "get1_1_center_EB",
                               "get1_1_right_EB", "get1_2_other_EB","get1_2_left_EB",
                               "get1_2_center_EB","get1_2_right_EB", "get2_0_other_EB",
                               "get2_0_left_EB", "get2_0_center_EB", "get2_0_right_EB",
                               "get2_1_other_EB", "get2_1_left_EB", "get2_1_center_EB",
                               "get2_1_right_EB", "get2_2_other_EB", "get2_2_left_EB",
                               "get2_2_center_EB", "get2_2_right_EB", 
                               "get3_0_left_EB", "get3_0_center_EB", "get3_0_right_EB",
                               "get3_1_left_EB", "get3_1_center_EB",
                               "get3_1_right_EB",                 "get3_2_left_EB",
                               "get3_2_center_EB", "get3_2_right_EB")
##----------------------------------------------------------------------------------------

#把runner_number_of_times表裡NA值變為0
runner_number_of_times[is.na(runner_number_of_times)] <- 0

##----------------------------------------------------------------------------------------

#  name        players_opp get_1base_players get_2bases_players get_3bases_players get_out_players get_0_other_EB get_0_left_EB get_0_center_EB get_0_right_EB get_1_other_EB
# 王勝偉          11                 9                 NA                 NA               2              1             1               1              1             NA
# get_1_left_EB get_1_center_EB get_1_right_EB get_2_other_EB get_2_left_EB get_2_center_EB get_2_right_EB
# 1             2              NA              1             NA             2               1              1

Name = runner_number_of_times$name

Opp = runner_number_of_times$players_opp

Bases = runner_number_of_times$get_1base_players + 2 * runner_number_of_times$get_2bases_players  + 3 * runner_number_of_times$get_3bases_players

#opp3_EB_0_1 * runner_number_of_times$get3_0_other_EB +  opp3_EB_1_1 * runner_number_of_times$get3_1_other_EB +
#opp3_EB_2_1 * runner_number_of_times$get3_2_other_EB +

EB =  opp1_EB_0_1 * runner_number_of_times$get1_0_other_EB + opp1_EB_0_2 * runner_number_of_times$get1_0_left_EB + 
      opp1_EB_0_3 * runner_number_of_times$get1_0_center_EB + opp1_EB_0_4 * runner_number_of_times$get1_0_right_EB + 
      opp1_EB_1_1 * runner_number_of_times$get1_1_other_EB + opp1_EB_1_2 * runner_number_of_times$get1_1_left_EB + 
      opp1_EB_1_3 * runner_number_of_times$get1_1_center_EB + opp1_EB_1_4 * runner_number_of_times$get1_1_right_EB + 
      opp1_EB_2_1 * runner_number_of_times$get1_2_other_EB + opp1_EB_2_2 * runner_number_of_times$get1_2_left_EB + 
      opp1_EB_2_3 * runner_number_of_times$get1_2_center_EB + opp1_EB_2_4 * runner_number_of_times$get1_2_right_EB +
      opp2_EB_0_1 * runner_number_of_times$get2_0_other_EB + opp2_EB_0_2 * runner_number_of_times$get2_0_left_EB + 
      opp2_EB_0_3 * runner_number_of_times$get2_0_center_EB + opp2_EB_0_4 * runner_number_of_times$get2_0_right_EB + 
      opp2_EB_1_1 * runner_number_of_times$get2_1_other_EB + opp2_EB_1_2 * runner_number_of_times$get2_1_left_EB + 
      opp2_EB_1_3 * runner_number_of_times$get2_1_center_EB + opp2_EB_1_4 * runner_number_of_times$get2_1_right_EB + 
      opp2_EB_2_1 * runner_number_of_times$get2_2_other_EB + opp2_EB_2_2 * runner_number_of_times$get2_2_left_EB + 
      opp2_EB_2_3 * runner_number_of_times$get2_2_center_EB + opp2_EB_2_4 * runner_number_of_times$get2_2_right_EB +
      opp3_EB_0_2 * runner_number_of_times$get3_0_left_EB + 
      opp3_EB_0_3 * runner_number_of_times$get3_0_center_EB + opp3_EB_0_4 * runner_number_of_times$get3_0_right_EB + 
      opp3_EB_1_2 * runner_number_of_times$get3_1_left_EB + 
      opp3_EB_1_3 * runner_number_of_times$get3_1_center_EB + opp3_EB_1_4 * runner_number_of_times$get3_1_right_EB + 
      opp3_EB_2_2 * runner_number_of_times$get3_2_left_EB + 
      opp3_EB_2_3 * runner_number_of_times$get3_2_center_EB + opp3_EB_2_4 * runner_number_of_times$get3_2_right_EB
  
EB = round(EB,digits=0)

IB = Bases - EB

OA = runner_number_of_times$get_out_players

IBP = round(Bases/EB,digits=3)

player_number_of_times <- data.frame(Name,Opp,Bases,EB,IB,OA,IBP)

View(player_number_of_times)
