# test file
path <- "D:/dataset_cpbl/test/20160328_scoreMatrix/2014_001_1上.txt"
logs <- readLines(file(path, encoding = "UTF-8"))

# create a dummy offensive condition matrix
off_cond <- list(bases = 0, outs = 0)

# 開局情境
start.ining <- function(log_row) {
  # 能否使用 key value?
  i.index <- c("1上", "1下", "2上", "2下",
                   "3上", "3下", "4上", "4下",
                   "5上", "5下", "6上", "6下",
                   "7上", "7下", "8上", "8下", "9上", "9下")
  for(i in 1:length(i.index)){
    if (grepl(i.index[i],log_row)) {
      print("catch opening condition!")
      off_cond["bases"] <- 0
      off_cond["outs"] <- 0
    }
  }
}

# 判斷出局
catch.outs <- function(log_row) { 
  if (grepl("三出局", log_row)) {
    print("3 outs!")
    off_cond["outs"] <- 3
  } 
  else if (grepl("兩出局", log_row)) {
    print("2 outs!")
    off_cond["outs"] <- 2
  } 
  else if (grepl("一出局", log_row)) {
    print("1 out!")  
    off_cond["outs"] <- 1
  }
}

# 判斷安打

?grepl

catch.hits <- function(log_row) {
  if (grepl("一壘安打", log_row)) {
    print("1 base hit!")
    # off_cond["bases"] <- 1
  }
}

# 判斷盜壘
catch.stealing <- function(log_row) {
  if (grepl("盜壘成功", log_row)) {
    if (grepl("上到二壘", log_row)) {
      print("stolen 2 base!")
    }
  }
}