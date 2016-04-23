library(dplyr)
off_db_file <- "/Users/shipo/Documents/cpbl_project/temp/output042205_utf8.csv" 
off_db <- read.csv(off_db_file)

# 1. 設定
# 1-1. BASIC: (壘包出局情境為 1~24, 且後續得分為 0~100 的資料列)
odb_rtype <- off_db %>% filter(rem_type %in% c(1:24), follow.up %in% c(0:100))

# 1-2. TEAM: (客隊上半局為攻擊、主隊下半局為攻擊)
# or_away <- odb_rtype %>% filter(grepl(pattern = "統一", x = away, fixed = TRUE), grepl(pattern = "上", x = inning, fixed = TRUE)) 
# or_home <- odb_rtype %>% filter(grepl(pattern = "統一", x = home, fixed = TRUE), grepl(pattern = "下", x = inning, fixed = TRUE)) 
# odb_rtype <- rbind(or_away, or_home)

# 1-3. PLAYER: (以球員名字為篩選條件)
# odb_rtype <- odb_rtype %>% filter(Player == "胡金龍")


# 2. Output
# 2-1. 計算 24 種 rem_type 的得分期望值
re_list <- list()
for (i in 1:24) {
  odb_r <- odb_rtype %>% filter(rem_type == i)
  r <- sum(odb_r$follow.up) / nrow(odb_r)
  re_list[i] <- r
}
# 2-2. set the martix
re_matrix <- matrix(re_list, nrow = 8, byrow=T)
colnames(re_matrix) <- c("out0", "out1", "out2")
rownames(re_matrix) <- c("empty", "1B", "2B", "3B", "1B_2B", "1B_3B", "2B_3B", "1B_2B_3B")

# 2-3. output REM and Warning amount
re_matrix
summary(odb_rtype$special)