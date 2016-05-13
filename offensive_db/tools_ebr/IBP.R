#由status1_table、status2_table、status3_table (.R檔)
#所算出的值，丟進這script裡，能得到每個球員在整季所獲的的機會次數(Opp)
#及在這些機會次數下實際得到幾個壘包(Bases)，預期應該得到幾個壘包(EB)
#比預期多跑幾個壘包(IB=Bases-EB)，出局(OA)，(IBP=Bases/EB)

library(dplyr)

#status1,status2,status3，每當有一次機會次數
#各種不同出局與安打方向組合EB的計算
#0 other,0 left,0 center,0 right (零出局)(內、左、中、右)
#1 other,1 left,1 center,1 right (一出局)(內、左、中、右)
#2 other,2 left,2 center,2 right (二出局)(內、左、中、右)
#status1
#EB = 1 * (To2nd_Probability) + 2 * (To3rd_Probability) + 
#     3 * (Score_Probability) – 1 * (OA_Probability)
#status2
#EB = 1 * (To3rd_Probability) + 2 * (Score_Probability) – 
#     1 * (OA_Probability)
#status3
#EB = 2 * (To3rd_Probability) + 3 * (Score_Probability) – 
#     2 * (OA_Probability)
#prob(第幾狀況)_(幾出局)_(方向)_(狀況)是由status1_table、status2_table、status3_table來的

#0 other,0 left,0 center,0 right
for (i in 1:4) {
  assign(paste0("opp1_EB_0_",i), 1*get(paste0("prob1_","0_",i,"_2nd")) + 
           2*get(paste0("prob1_","0_",i,"_3rd")) +
           3*get(paste0("prob1_","0_",i,"_score")) -
           1*get(paste0("prob1_","0_",i,"_OA")) )
  
  assign(paste0("opp2_EB_0_",i), 1*get(paste0("prob2_","0_",i,"_3rd")) + 
           2*get(paste0("prob2_","0_",i,"_score")) -
           1*get(paste0("prob2_","0_",i,"_OA")) )
  
  assign(paste0("opp3_EB_0_",i), 2*get(paste0("prob3_","0_",i,"_3rd")) + 
           3*get(paste0("prob3_","0_",i,"_score")) -
           2*get(paste0("prob3_","0_",i,"_OA")) )
  
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
  
  assign(paste0("opp3_EB_1_",i), 2*get(paste0("prob3_","1_",i,"_3rd")) + 
           3*get(paste0("prob3_","1_",i,"_score")) -
           2*get(paste0("prob3_","1_",i,"_OA")) )
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
  
  assign(paste0("opp3_EB_2_",i), 2*get(paste0("prob3_","2_",i,"_3rd")) + 
           3*get(paste0("prob3_","2_",i,"_score")) -
           2*get(paste0("prob3_","2_",i,"_OA")) )
}

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
#個人實際跑了多少個壘包
#跑者得一個壘包有誰(計次數)
get_1base<-NULL

for (i in 0:2) {
  for (j in 1:4) {
    get_1base<-c(get_1base,lapply( paste0("total1_",i,"_",j,"_","2nd","_player") , get),
                 lapply( paste0("total2_",i,"_",j,"_","3rd","_player") , get))
  }
}

get_1base_players <- as.data.frame(unlist(get_1base)%>%table())
colnames(get_1base_players) <- c("name","opp_total")

#跑者得兩個壘包有誰(計次數)
get_2base<-NULL

for (i in 0:2) {
  for (j in 1:4) {
    get_2base<-c(get_2base,lapply( paste0("total1_",i,"_",j,"_","3rd","_player") , get),
                 lapply( paste0("total2_",i,"_",j,"_","score","_player") , get),
                 lapply( paste0("total3_",i,"_",j,"_","3rd","_player") , get))
    #lapply( paste0("total3_",i,"_",j,"_","score","_player") , get))
  }
}

get_2bases_players <- as.data.frame(unlist(get_2base)%>%table())
colnames(get_2bases_players) <- c("name","opp_total")

#跑者得三個壘包有誰(計次數)
get_3base<-NULL

for (i in 0:2) {
  for (j in 1:4) {
    get_3base<-c(get_3base,lapply( paste0("total1_",i,"_",j,"_","score","_player") , get),
                 lapply( paste0("total3_",i,"_",j,"_","score","_player") , get))
  }
}

get_3bases_players <- as.data.frame(unlist(get_3base)%>%table())
colnames(get_3bases_players) <- c("name","opp_total")

#------------------------------------------------------------------------------------------
#個人預期應該跑多少個壘包(各種不同出局與安打方向組合，計EB次數)
#status1,status2,status3分開算
#0 Other
#因為 total3_",i,"_",j,"_","OA","_player 沒有球員發生過
#所以,lapply( paste0("total3_","0","_","1","_","OA","_player") , get)拿掉
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

#0 left
#因為 total3_",i,"_",j,"_","OA","_player 沒有球員發生過
#所以,lapply( paste0("total3_","0","_","2","_","OA","_player") , get)拿掉
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

#0 center
#因為 total3_",i,"_",j,"_","OA","_player 沒有球員發生過
#所以,lapply( paste0("total3_","0","_","3","_","OA","_player") , get)拿掉
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

#0 right
#因為 total3_",i,"_",j,"_","OA","_player 沒有球員發生過
#所以,lapply( paste0("total3_","0","_","4","_","OA","_player") , get)拿掉
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

#1 Other
#因為 total3_",i,"_",j,"_","OA","_player 沒有球員發生過
#所以, lapply( paste0("total3_","1","_","1","_","OA","_player") , get)拿掉
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

#1 left
#因為 total3_",i,"_",j,"_","OA","_player 沒有球員發生過
#所以,lapply( paste0("total3_","1","_","2","_","OA","_player") , get)拿掉
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

#1 center
#因為 total3_",i,"_",j,"_","OA","_player 沒有球員發生過
#所以,lapply( paste0("total3_","1","_","3","_","OA","_player") , get)拿掉
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

#1 right
#因為 total3_",i,"_",j,"_","OA","_player 沒有球員發生過
#所以,lapply( paste0("total3_","1","_","4","_","OA","_player") , get)拿掉
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

#2 Other
#因為 total3_",i,"_",j,"_","OA","_player 沒有球員發生過
#所以,lapply( paste0("total3_","2","_","1","_","OA","_player") , get)拿掉
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

#2 left
#因為 total3_",i,"_",j,"_","OA","_player 沒有球員發生過
#所以,lapply( paste0("total3_","2","_","2","_","OA","_player") , get)拿掉
get1_2_left<-NULL
get2_2_left<-NULL
get3_2_left<-NULL

get1_2_left<-c(get1_2_left,lapply( paste0("total1_","2","_","2","_","2nd","_player") , get),
               lapply( paste0("total1_","2","_","2","_","3rd","_player") , get),
               lapply( paste0("total1_","2","_","2","_","score","_player") , get),
               lapply( paste0("total1_","2","_","2","_","OA","_player") , get))
get2_2_left<-c(get2_2_left,lapply( paste0("total2_","2","_","2","_","3rd","_player") , get),
               lapply( paste0("total2_","2","_","2","_","score","_player") , get),
               lapply( paste0("total2_","2","_","2","_","OA","_player") , get))
get3_2_left<-c(get3_2_left,lapply( paste0("total3_","2","_","2","_","3rd","_player") , get),
               lapply( paste0("total3_","2","_","2","_","score","_player") , get))


get1_2_left_EB <- as.data.frame(unlist(get1_2_left)%>%table())
get2_2_left_EB <- as.data.frame(unlist(get2_2_left)%>%table())
get3_2_left_EB <- as.data.frame(unlist(get3_2_left)%>%table())
colnames(get1_2_left_EB) <- c("name","opp_total")
colnames(get2_2_left_EB) <- c("name","opp_total")
colnames(get3_2_left_EB) <- c("name","opp_total")

#2 center
#因為 total3_",i,"_",j,"_","OA","_player 沒有球員發生過
#所以,lapply( paste0("total3_","2","_","3","_","OA","_player") , get)拿掉
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

#2 right
#因為 total3_",i,"_",j,"_","OA","_player 沒有球員發生過
#所以,lapply( paste0("total3_","2","_","4","_","OA","_player") , get)拿掉
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
#跑者在跑壘發生出局有誰(計次數)
#因為 total3_",i,"_",j,"_","OA","_player 沒有球員發生過
#所以,lapply( paste0("total3_",i,"_",j,"_","OA","_player") , get)拿掉
get_out<-NULL

for (i in 0:2) {
  for (j in 1:4) {
    get_out<-c(get_out,lapply( paste0("total1_",i,"_",j,"_","OA","_player") , get),
               lapply( paste0("total2_",i,"_",j,"_","OA","_player") , get))
  }
}

get_out_players <- as.data.frame(unlist(get_out)%>%table())
colnames(get_out_players) <- c("name","opp_total")

#------------------------------------------------------------------------------------------
#跑者跑超過EB(計算次數)
more_than_EB<-NULL

for (i in 0:2) {
  for (j in 1:4) {
    more_than_EB<-c(more_than_EB,lapply( paste0("total1_",i,"_",j,"_","3rd","_player") , get),
                    lapply( paste0("total1_",i,"_",j,"_","score","_player") , get),
                    lapply( paste0("total2_",i,"_",j,"_","score","_player") , get),
                    lapply( paste0("total3_",i,"_",j,"_","score","_player") , get))
  }
}

more_than_EB <- as.data.frame(unlist(more_than_EB)%>%table())
colnames(more_than_EB) <- c("name","more_EB")

#----------------------------------------------------------------------------------------
#合併所有計次數變數
#所得get3_0_other_EB, get3_1_other_EB, get3_2_other_EB 沒有球員發生，所以不放入
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
                                      get3_2_center_EB, get3_2_right_EB,more_than_EB))

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
                                      "get3_2_center_EB", "get3_2_right_EB","more_than_EB")

#把runner_number_of_times表裡NA值以0取代
runner_number_of_times[is.na(runner_number_of_times)] <- 0

#----------------------------------------------------------------------------------------
Name = runner_number_of_times$name

Opp = runner_number_of_times$players_opp

Bases = runner_number_of_times$get_1base_players + 2 * runner_number_of_times$get_2bases_players  + 3 * runner_number_of_times$get_3bases_players

#所得get3_0_other_EB, get3_1_other_EB, get3_2_other_EB 沒有球員發生
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

#EB2與EB的差別在於，EB2就是看打者打幾壘安打，跑者就應該預期得幾個壘包
#一壘安打得一個壘包，二壘安打得兩個壘包
EB2 = 1*(runner_number_of_times$get1_0_other_EB + runner_number_of_times$get1_0_left_EB + 
           runner_number_of_times$get1_0_center_EB + runner_number_of_times$get1_0_right_EB + 
           runner_number_of_times$get1_1_other_EB +  runner_number_of_times$get1_1_left_EB + 
           runner_number_of_times$get1_1_center_EB + runner_number_of_times$get1_1_right_EB + 
           runner_number_of_times$get1_2_other_EB + runner_number_of_times$get1_2_left_EB + 
           runner_number_of_times$get1_2_center_EB +  runner_number_of_times$get1_2_right_EB) +
      1*(runner_number_of_times$get2_0_other_EB + runner_number_of_times$get2_0_left_EB + 
           runner_number_of_times$get2_0_center_EB + runner_number_of_times$get2_0_right_EB + 
           runner_number_of_times$get2_1_other_EB + runner_number_of_times$get2_1_left_EB + 
           runner_number_of_times$get2_1_center_EB + runner_number_of_times$get2_1_right_EB + 
           runner_number_of_times$get2_2_other_EB +  runner_number_of_times$get2_2_left_EB + 
           runner_number_of_times$get2_2_center_EB + runner_number_of_times$get2_2_right_EB) +
      2*(runner_number_of_times$get3_0_left_EB + 
           runner_number_of_times$get3_0_center_EB + runner_number_of_times$get3_0_right_EB + 
           runner_number_of_times$get3_1_left_EB + 
           runner_number_of_times$get3_1_center_EB + runner_number_of_times$get3_1_right_EB + 
           runner_number_of_times$get3_2_left_EB + 
           runner_number_of_times$get3_2_center_EB + runner_number_of_times$get3_2_right_EB)

IB = Bases - EB

IB2 = Bases - EB2

OA = runner_number_of_times$get_out_players

IBP = Bases/EB

IBP2 = Bases/EB2

more_EB = runner_number_of_times$more_than_EB

player_number_of_times <- data.frame(Name,Opp,Bases,EB,EB2,IB,IB2,OA,IBP,IBP2,more_EB)

View(player_number_of_times)