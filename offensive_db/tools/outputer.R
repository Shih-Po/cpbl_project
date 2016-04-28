
# num_logfile <- 43
# log_path <- paste0("D:/cpbl_project/logs/2014/例行賽", as.character(num_logfile), "(2014org).txt")
# log_file <- readLines(log_path, encoding = "UTF-8")
# log_file <- normalize_log_function(log_file)


setwd("D:/cpbl_project/offensive_db/tools")
write.csv(offensive_db, "offensive_db.csv", row.names = FALSE)
