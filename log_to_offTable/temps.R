# temp the trickys

# load files
num_logfile <- 1
# mac, linux
# log_path <- paste0("/Users/shipo/Documents/cpbl_project/20160314/例行賽", as.character(num_logfile), "(2014org).txt")
# windows
log_path <- paste0("D:/dataset_cpbl/test/20160314/例行賽", as.character(num_logfile), "(2014org).txt")
log_file <- readLines( log_path, encoding = "UTF-8")


#
c_nlog <- nchar(log_file)
summary(c_nlog)
