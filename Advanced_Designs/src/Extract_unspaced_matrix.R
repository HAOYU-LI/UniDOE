file_list = list.files(pattern = "*.txt")

for(file_name in file_list ){
  cat("at",file_name,"\n")
  save_name = paste("./LT68_matrix/",file_name,".csv")
  mystring <- paste(readLines(file_name), collapse=" ")
  numberstring_split <- strsplit(mystring, "")[[1]]
  
  M_list = c()
  for( number in numberstring_split){
    if(number != " "){
      M_list = c(M_list,(as.integer(number)+1))  
    }
  }
  M = matrix(M_list,ncol = 27,nrow = 13, byrow = TRUE)
  write.csv(M,save_name)
}
