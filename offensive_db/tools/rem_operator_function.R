library(dplyr)
off_db <- readRDS("D:/cpbl_project/temp/off_db.RDS")

# t = 1  算得分期望值, t = 2 算次數
rem_operator = function(t) {
  odb_rtype <- off_db %>% filter(rem_type %in% c(1:24), follow.up %in% c(0:100), !(Player == ""))
  odb_rtype <- odb_rtype %>% select(numforgame, rowforgame, rowforextra, 
                                    inning, away, home, rem_type, base1, base2, base3, out, 
                                    id, Player, direction, result, follow.up)
  re_list <- list()
  for (i in 1:24) {
    odb_r <- odb_rtype %>% filter(rem_type == i)
    # different type
    if (t == 1) {
      r <- round(sum(odb_r$follow.up) / nrow(odb_r), digits = 3) #得分期望值
    } else {
      r <- nrow(odb_r) #次數
    }
    re_list[i] <- r
  }
  re_matrix <- matrix(re_list, nrow = 8, byrow=T)
  colnames(re_matrix) <- c("out0", "out1", "out2")
  rownames(re_matrix) <- c("empty", "1B", "2B", "3B", "1B_2B", "1B_3B", "2B_3B", "1B_2B_3B")
  return(re_matrix)
}

# team 參數輸入隊名：統一、兄弟、義大、桃猿
rem_operator_team = function(t, team) {
  odb_rtype <- off_db %>% filter(rem_type %in% c(1:24), follow.up %in% c(0:100), !(Player == ""))
  odb_rtype <- odb_rtype %>% select(numforgame, rowforgame, rowforextra, 
                                    inning, away, home, rem_type, base1, base2, base3, out, 
                                    id, Player, direction, result, follow.up)
  # team <- "兄弟"
  or_away <- odb_rtype %>% filter(grepl(pattern = team, x = away, fixed = TRUE), grepl(pattern = "上", x = inning, fixed = TRUE)) 
  or_home <- odb_rtype %>% filter(grepl(pattern = team, x = home, fixed = TRUE), grepl(pattern = "下", x = inning, fixed = TRUE)) 
  odb_rtype <- rbind(or_away, or_home)
  
  re_list <- list()
  for (i in 1:24) {
    odb_r <- odb_rtype %>% filter(rem_type == i)
    # different type
    if (t == 1) {
      r <- round(sum(odb_r$follow.up) / nrow(odb_r), digits = 3) #得分期望值
    } else {
      r <- nrow(odb_r) #次數
    }
    re_list[i] <- r
  }
  re_matrix <- matrix(re_list, nrow = 8, byrow=T)
  colnames(re_matrix) <- c("out0", "out1", "out2")
  rownames(re_matrix) <- c("empty", "1B", "2B", "3B", "1B_2B", "1B_3B", "2B_3B", "1B_2B_3B")
  return(re_matrix)
}

# 參數 p 輸入球員名字
rem_operator_p = function(t, p) {
  odb_rtype <- off_db %>% filter(rem_type %in% c(1:24), follow.up %in% c(0:100), !(Player == ""))
  odb_rtype <- odb_rtype %>% select(numforgame, rowforgame, rowforextra, 
                                    inning, away, home, rem_type, base1, base2, base3, out, 
                                    id, Player, direction, result, follow.up)
  # p <- "彭政閔"
  odb_rtype <- odb_rtype %>% filter(Player == p)
  
  re_list <- list()
  for (i in 1:24) {
    odb_r <- odb_rtype %>% filter(rem_type == i)
    # different type
    if (t == 1) {
      r <- round(sum(odb_r$follow.up) / nrow(odb_r), digits = 3) #得分期望值
    } else {
      r <- nrow(odb_r) #次數
    }
    re_list[i] <- r
  }
  re_matrix <- matrix(re_list, nrow = 8, byrow=T)
  colnames(re_matrix) <- c("out0", "out1", "out2")
  rownames(re_matrix) <- c("empty", "1B", "2B", "3B", "1B_2B", "1B_3B", "2B_3B", "1B_2B_3B")
  return(re_matrix)
}

# ------------------------------------------------------------------------------------------
# t = 1  算得分期望值, t = 2 算次數
rem_operator(1)
rem_operator(2)
# 參數 team 輸入隊名：統一、兄弟、義大、桃猿
rem_team <- lapply(c("統一", "兄弟", "義大", "桃猿"), function(team) { rem_operator_team(1, team) })
names(rem_team) <- c("統一", "兄弟", "義大", "桃猿")
ram_team <- lapply(c("統一", "兄弟", "義大", "桃猿"), function(team) { rem_operator_team(2, team) })
names(ram_team) <- c("統一", "兄弟", "義大", "桃猿")
rem_team %>% View()

write.csv(rem_team, file = "D:/cpbl_project/output/2014/rem_team.csv")

# 參數 p 輸入球員名字
rem_operator_p(1, p = "彭政閔")

