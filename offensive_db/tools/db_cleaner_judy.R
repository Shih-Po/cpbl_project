library(dplyr)

off_db <- read.csv(file("D:/cpbl_project/temp/output_1050503.csv", encoding="big5"), 
                   header = TRUE, sep = "," ,stringsAsFactors = FALSE)

for(i in 1: nrow(off_db)){
  # complete.cases() <->  is.na()
  if(is.na(off_db$out[i])){
    off_db$out[i]=as.factor("零出局")
  }
  #b1 b2 b3 out
  #NA NA NA NA
  if(
    is.na(off_db$base1[i]) & 
    is.na(off_db$base2[i]) & 
    is.na(off_db$base3[i]) & 
    is.na(off_db$out[i])){
    off_db$rem_type[i]=NA
  }
  #b1 b2 b3 out
  #0  0 0   0
  if(
    is.na(off_db$base1[i]) & 
    is.na(off_db$base2[i]) & 
    is.na(off_db$base3[i]) & 
    off_db$out[i]=="零出局"){
    off_db$rem_type[i]=1
  }
  #b1 b2 b3 out
  #1  0 0   0
  if(
    complete.cases(off_db$base1[i]) & 
    is.na(off_db$base2[i]) & 
    is.na(off_db$base3[i]) & 
    off_db$out[i]=="零出局"){
    off_db$rem_type[i]=4
  }
  
  #b1 b2 b3 out
  #0  1 0   0
  if(
    is.na(off_db$base1[i]) & 
    complete.cases(off_db$base2[i]) & 
    is.na(off_db$base3[i]) & 
    off_db$out[i]=="零出局"){
    off_db$rem_type[i]=7
  }
  
  #b1 b2 b3 out
  #0  0  1   0
  if(
    is.na(off_db$base1[i]) & 
    is.na(off_db$base2[i]) & 
    complete.cases(off_db$base3[i]) & 
    off_db$out[i]=="零出局"){
    off_db$rem_type[i]=10
  }
  
  #b1 b2 b3 out
  #1  1  0   0
  if(
    complete.cases(off_db$base1[i]) & 
    complete.cases(off_db$base2[i]) & 
    is.na(off_db$base3[i]) & 
    off_db$out[i]=="零出局"){
    off_db$rem_type[i]=13
  }
  
  #b1 b2 b3 out
  #1  0  1   0
  if(
    complete.cases(off_db$base1[i]) & 
    is.na(off_db$base2[i]) & 
    complete.cases(off_db$base3[i]) & 
    off_db$out[i]=="零出局"){
    off_db$rem_type[i]=16
  }
  
  #b1 b2 b3 out
  #0  1  1   0
  if(
    is.na(off_db$base1[i]) & 
    complete.cases(off_db$base2[i]) & 
    complete.cases(off_db$base3[i]) & 
    off_db$out[i]=="零出局"){
    off_db$rem_type[i]=19
  }
  
  #b1 b2 b3 out
  #1  1  1   0
  if(
    complete.cases(off_db$base1[i]) & 
    complete.cases(off_db$base2[i]) & 
    complete.cases(off_db$base3[i]) & 
    off_db$out[i]=="零出局"){
    off_db$rem_type[i]=22
  }
  
  
  
  #b1 b2 b3 out
  #0  0 0   1
  if(
    is.na(off_db$base1[i]) & 
    is.na(off_db$base2[i]) & 
    is.na(off_db$base3[i]) & 
    off_db$out[i]=="一出局"){
    off_db$rem_type[i]=2
  }
  
  #b1 b2 b3 out
  #1  0 0   1
  if(
    complete.cases(off_db$base1[i]) & 
    is.na(off_db$base2[i]) & 
    is.na(off_db$base3[i]) & 
    off_db$out[i]=="一出局"){
    off_db$rem_type[i]=5
  }
  
  #b1 b2 b3 out
  #0  1 0   1
  if(
    is.na(off_db$base1[i]) & 
    complete.cases(off_db$base2[i]) & 
    is.na(off_db$base3[i]) & 
    off_db$out[i]=="一出局"){
    off_db$rem_type[i]=8
  }
  
  #b1 b2 b3 out
  #0  0  1   1
  if(
    is.na(off_db$base1[i]) & 
    is.na(off_db$base2[i]) & 
    complete.cases(off_db$base3[i]) & 
    off_db$out[i]=="一出局"){
    off_db$rem_type[i]=11
  }
  
  
  #b1 b2 b3 out
  #1  1 0   1
  if(
    complete.cases(off_db$base1[i]) & 
    complete.cases(off_db$base2[i]) & 
    is.na(off_db$base3[i]) & 
    off_db$out[i]=="一出局"){
    off_db$rem_type[i]=14
  }
  
  #b1 b2 b3 out
  #1  0  1   1
  if(
    complete.cases(off_db$base1[i]) & 
    is.na(off_db$base2[i]) & 
    complete.cases(off_db$base3[i]) & 
    off_db$out[i]=="一出局"){
    off_db$rem_type[i]=17
  }
  
  #b1 b2 b3 out
  #0  1  1   1
  if(
    is.na(off_db$base1[i]) & 
    complete.cases(off_db$base2[i]) & 
    complete.cases(off_db$base3[i]) & 
    off_db$out[i]=="一出局"){
    off_db$rem_type[i]=20
  }
  
  #b1 b2 b3 out
  #1  1  1   1
  if(
    complete.cases(off_db$base1[i]) & 
    complete.cases(off_db$base2[i]) & 
    complete.cases(off_db$base3[i]) & 
    off_db$out[i]=="一出局"){
    off_db$rem_type[i]=23
  }
  
  #b1 b2 b3 out
  #0  0 0   2
  if(
    is.na(off_db$base1[i]) & 
    is.na(off_db$base2[i]) & 
    is.na(off_db$base3[i]) & 
    off_db$out[i]=="二出局"){
    off_db$rem_type[i]=3
  }
  
  #b1 b2 b3 out
  #1  0  0   2
  if(
    complete.cases(off_db$base1[i]) & 
    is.na(off_db$base2[i]) & 
    is.na(off_db$base3[i]) & 
    off_db$out[i]=="二出局"){
    off_db$rem_type[i]=6
  }
  
  #b1 b2 b3 out
  #0  1  0   2
  if(
    is.na(off_db$base1[i]) & 
    complete.cases(off_db$base2[i]) & 
    is.na(off_db$base3[i]) & 
    off_db$out[i]=="二出局"){
    off_db$rem_type[i]=9
  }
  
  #b1 b2 b3 out
  #0  0  1   2
  if(
    is.na(off_db$base1[i]) & 
    is.na(off_db$base2[i]) & 
    complete.cases(off_db$base3[i]) & 
    off_db$out[i]=="二出局"){
    off_db$rem_type[i]=12
  }
  
  #b1 b2 b3 out
  #1  1  0   2
  if(
    complete.cases(off_db$base1[i]) & 
    complete.cases(off_db$base2[i]) & 
    is.na(off_db$base3[i]) & 
    off_db$out[i]=="二出局"){
    off_db$rem_type[i]=15
  }
  
  #b1 b2 b3 out
  #1  0  1   2
  if(
    complete.cases(off_db$base1[i]) & 
    is.na(off_db$base2[i]) & 
    complete.cases(off_db$base3[i]) & 
    off_db$out[i]=="二出局"){
    off_db$rem_type[i]=18
  }
  
  #b1 b2 b3 out
  #0  1  1   2
  if(
    is.na(off_db$base1[i]) & 
    complete.cases(off_db$base2[i]) & 
    complete.cases(off_db$base3[i]) & 
    off_db$out[i]=="二出局"){
    off_db$rem_type[i]=21
  }
  
  #b1 b2 b3 out
  #1  1  1   2
  if(
    complete.cases(off_db$base1[i]) & 
    complete.cases(off_db$base2[i]) & 
    complete.cases(off_db$base3[i]) & 
    off_db$out[i]=="二出局"){
    off_db$rem_type[i]=24
  }
}

saveRDS(off_db,"D:/cpbl_project/temp/off_db.RDS")
