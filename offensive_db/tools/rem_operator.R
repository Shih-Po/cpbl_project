library(dplyr)
off_db_file <- "/Users/shipo/Documents/cpbl_project/temp/output042100_utf8.csv" 
off_db <- read.csv(off_db_file)

# 找壘包出局情境為 1~24, 且後續得分為 0~100 的資料列
odb_rtype <- off_db %>% filter(rem_type %in% c(1:24), follow.up %in% c(0:100))

# B. 找球隊 (客隊上半局為攻擊、主隊下半局為攻擊)
# or_away <- odb_rtype %>% filter(grepl(pattern = "兄弟", x = away, fixed = TRUE), grepl(pattern = "上", x = inning, fixed = TRUE)) 
# or_home <- odb_rtype %>% filter(grepl(pattern = "兄弟", x = home, fixed = TRUE), grepl(pattern = "下", x = inning, fixed = TRUE)) 
# odb_rtype <- rbind(odb_rtype_away, odb_rtype_home)

# C. 找球員
# odb_rtype <- odb_rtype %>% filter(Player == "胡金龍")
re_matrix <- list()

# 計算24種 rem_type 的得分期望值
for (i in 1:24) {
  odb_r <- odb_rtype %>% filter(rem_type == i)
  r <- sum(odb_r$follow.up) / nrow(odb_r)
  re_matrix[i] <- r
}

# output a list
re_matrix
