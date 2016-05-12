library(dplyr)
<<<<<<< Updated upstream
# off_db <- read.csv(file("D:/cpbl_project/temp/output042205.csv", encoding="big5"), 
#                      header = TRUE, sep = "," ,stringsAsFactors = FALSE)
off_db <- readRDS("D:/cpbl_project/temp/off_db.RDS")
=======
off_db_path <- "/Users/shipo/Documents/cpbl_project/temp/stone_1_24_042303_utf8.csv" 
off_db <- read.csv(off_db_path)
>>>>>>> Stashed changes

# 1. 設定
# 1-1. BASIC(全聯盟):  (壘包出局情境為 1~24, 且後續得分為 0~100 的資料列)
odb_rtype <- off_db %>% filter(rem_type %in% c(1:24), follow.up %in% c(0:100), !(Player == ""))
odb_rtype <- odb_rtype %>% select(numforgame, rowforgame, rowforextra, 
                     inning, away, home, rem_type, base1, base2, base3, out, 
                     id, Player, direction, result, follow.up)

# 1-2. TEAM: (客隊上半局為攻擊、主隊下半局為攻擊)
# team <- "兄弟"
# or_away <- odb_rtype %>% filter(grepl(pattern = team, x = away, fixed = TRUE), grepl(pattern = "上", x = inning, fixed = TRUE)) 
# or_home <- odb_rtype %>% filter(grepl(pattern = team, x = home, fixed = TRUE), grepl(pattern = "下", x = inning, fixed = TRUE)) 
# odb_rtype <- rbind(or_away, or_home)

# 1-3. PLAYER: (以球員名字為篩選條件)
# odb_rtype <- odb_rtype %>% filter(Player == "彭政閔")

# 1-3-1. 球員出場次數
# rem_player <- odb_rtype$Player %>% table() %>% as.data.frame()
# colnames(rem_player) <- c("Name", "Freq")
# rem_player[order(rem_player$Freq, decreasing = TRUE), ] %>% head(20) %>% View()

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

View(re_matrix)
r1 <- re_matrix

# 2014 mlb REM
rem_mlb <- re_matrix
rem_mlb[1, ] <- c(0.45, 0.23, 0.09)
rem_mlb[2, ] <- c(1.37, 0.94, 0.34)
rem_mlb[3, ] <- c(1.07, 0.62, 0.31)
rem_mlb[4, ] <- c(0.82, 0.48, 0.20)
rem_mlb[5, ] <- c(1.88, 1.35, 0.50)
rem_mlb[6, ] <- c(1.78, 1.10, 0.41)
rem_mlb[7, ] <- c(1.38, 0.83, 0.39)
rem_mlb[8, ] <- c(2.35, 1.50, 0.64)

# 2-3. output REM and Warning amount
View(re_matrix)
<<<<<<< Updated upstream
View(rem_mlb)
=======
summary(odb_rtype$special)
>>>>>>> Stashed changes
