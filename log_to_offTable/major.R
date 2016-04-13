num_logfile <- 1
# log_path <- paste0("/Users/shipo/Documents/cpbl_project/20160314/例行賽", as.character(num_logfile), "(2014org).txt")
log_path <- paste0("D:/dataset_cpbl/test/20160314/例行賽", as.character(num_logfile), "(2014org).txt")
log_file <- readLines( log_path, encoding = "UTF-8")

# 紀錄得分矩陣共24種情境、行動球員名、壘包上球員名, check with this link
# https://docs.google.com/spreadsheets/d/1fTBCX7Cgo3JINW0_SKA1qA-GD1GrAtZ1XqllJl6fqDw/edit#gid=1827881277
dummy_list <- list(
  num_logfile = num_logfile, num_logrow = 1, inning = "NA",
  rem_type = "NA", base1 = "NA", base2 = "NA", base3 = "NA", 
  player = "NA", 
  to_check = 0
)

# set the column vector
c_numlogfile <- "NA"
c_numlogrow <- "NA"
c_inning <- "NA"
c_rem_type <- "NA"
c_base1 <- "NA"
c_base2 <- "NA"
c_base3 <- "NA"
c_player <- "NA"
c_tocheck <- "NA"

# load by log_row
# log_row <- log_file[3]
for ( i in 1:length(log_file))  {
  log_row <- log_file[i]
  c_numlogfile[i] <- num_logfile
  c_numlogrow[i] <- i
  c_rem_type[i] <- dummy_list$rem_type
  c_base1[i] <- dummy_list$base1
  c_base2[i] <- dummy_list$base2
  c_base3[i] <- dummy_list$base3
  
  # renew the current player
  dummy_list$player <- "NA"
  
  # call the functions
  dummy_list <- inningStart_function(dummy_list, log_row)
  dummy_list <- hit_function(dummy_list, log_row)
  dummy_list <- outs_function(dummy_list, log_row)
  
  
  c_player[i] <- dummy_list$player
  c_tocheck[i] <- dummy_list$to_check
  c_inning[i] <- dummy_list$inning
  
  # human check
  if (nchar(log_row) > 40) {
    c_tocheck[i] <- 1
  }
    
}

# output
off_table <- data.frame(
  num_logfile = c_numlogfile, num_logrow = c_numlogrow, inning = c_inning,
  rem_type = c_rem_type, 
  base1 = c_base1, base2 = c_base2, base3 = c_base3, 
  player = c_player,
  to_check = c_tocheck
  # ,log_row = log_file
)
print(off_table)
