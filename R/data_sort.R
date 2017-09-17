PATH_TO_FILE = "./Advanced_Designs"
PATH_TO_DIR = "./Algo"
PATH_TO_NEW = "./Improvement"
CRIT_TYPE = "CL2_"
dir.create(file.path(PATH_TO_DIR), showWarnings = TRUE)


# first change old designs to identical format with new design:
tmp_nn = list.files(path = PATH_TO_FILE,pattern="*.csv")
total_files = length(tmp_nn)
for(file in tmp_nn){
  tryCatch({
  tmp_name = file
  cur_design = as.matrix(read.csv(file,header=FALSE))
  write.csv(cur_design,file = file.path(PATH_TO_FILE,tmp_name))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

# Then move new designs to merge with old designs, 
# which form advanced designs together:
new_nn = list.files(path = PATH_TO_NEW,pattern="*.csv")
for(file in new_nn){
  tmp_name = sub("[a-zA-Z]*_* ","",file)
  file_path = file.path(PATH_TO_NEW,file)
  cur_design = as.matrix(read.csv(file_path,header=TRUE))[,c(-1)]
  write.csv(cur_design,file = file.path(PATH_TO_FILE,tmp_name))
}


#sort Designs by runs and levels, used in algorithm. 
for(file in tmp_nn){
  tryCatch({
  tmp_name = sub(".csv","", sub("_[0-9]*","_s", sub(CRIT_TYPE,"",file) ) )
  file_path = file.path(PATH_TO_FILE,file)
  dir_path = file.path(PATH_TO_DIR,tmp_name)
  save_path = file.path(dir_path,file)
  cur_design = as.matrix(read.csv(file_path,header=TRUE))[,c(-1)]
  dir.create(dir_path, showWarnings = TRUE)
  write.csv(cur_design,file = save_path)
  }, error=function(e){cat("ERROR :",conditionMessage(e)," file : ",file,"\n")})
}
