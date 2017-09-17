library(UniDOE)

PATH_TO_FILE = "./Advanced_Designs"
PATH_TO_DIR = "./Algo"
PATH_TO_NEW = "./Improvement"
#######################################Need to be set for different crit################################
CRIT_TYPE = "CL2"                             
BEGIN_ROW = 27
########################################################################################################
dir.create(file.path(PATH_TO_DIR), showWarnings = TRUE)
dir_lists = list.files(path = PATH_TO_DIR)
total_nq_sets = length(dir_lists)

fb <- function(n,s,q,init_design,iter)
{
  # Find best(fb) augmented design in given iterations
  best_list = c()
  cur_value = 1e13
  maxiter = 300*n
  for(i in 1:iter)
  {
    cur_list = UDC(n=n,s=s,q=q,init = "orth",initX = init_design,maxiter = maxiter,crit = CRIT_TYPE)
    if(cur_list$obj < cur_value)
    {
      best_list = cur_list
    }
  }
  return(best_list)
}

fb2 <- function(n,s,q,iter)
{
  # Find best design with random input in given iteration:
  best_list = c()
  cur_value = 1e13
  maxiter = 300*n
  for(i in 1:iter)
  {
    cur_list = UDC(n=n,s=s,q=q,init = "rand",maxiter=maxiter,crit = CRIT_TYPE)
    if(cur_list$obj < cur_value )
    {
      best_list = cur_list
    }
  }
  return(best_list)
}

create_hash <- function(csv_lists)
{
  # Create a hash between file names and their columns: 
  csv_length = length(csv_lists)
  hash_list = c()
  for(i in 1:(csv_length+1)){
    hash_list[i] = ""
  }
  
  for(j in 1:csv_length){
    file = csv_lists[j]
    index = as.integer(sub("_[0-9]*","",sub("n_[0-9]*_","",sub(CRIT_TYPE,"n",sub(".csv","",file)))))
    hash_list[index] = file 
  }
  return(hash_list)
}


pos_list = c()
for(i in 1:7){
  #1:6 represents previous 1~6 columns and 7 represents total count.
  pos_list[i] = 0
}



#Traverse each directory:
for(dir in dir_lists){
  # tmp_name = sub("[a-zA-Z]*_* ","",file)
  cat("-----------------------------------------------------------------------\n")
  cat("At directory ",dir," :\n")
  
  dir_pos_list = c()
  for(i in 1:7){
    #1:6 represents previous 1~6 columns and 7 represents total count.
    dir_pos_list[i] = 0
  }
  
  tryCatch({
    file_path = file.path(PATH_TO_DIR,dir)
    csv_lists = list.files(path = file_path)
    csv_length = length(csv_lists)
    hash_list = create_hash(csv_lists = csv_lists)
    hash_length = length(hash_list)
    #Traverse each csv files:
    for(index in 2:hash_length){
      cat("At csv file ",hash_list[index]," :\n")
      csv_path = file.path(file_path,hash_list[index])
      cur_design = as.matrix(read.csv(csv_path,header=TRUE))[,c(-1)]
      row = nrow(cur_design)
      col = ncol(cur_design)
      level = max(cur_design) - min(cur_design) + 1
      #replace advanced design with manually generated, random design:( For research purpose )
      tmp_list = fb2(n=row,s=col,q=level,iter=3)
      cur_design =  tmp_list$UniDOE_Matrix
      write.csv(cur_design,file = csv_path)
      
      #only interested in large-size designs:
      if(row >= BEGIN_ROW && col >=8 )
      {
        cur_value = Eval(cur_design,crit = CRIT_TYPE)
        i = index-1
        crit_list = c(cur_value) # cur_value is put into first position
        count = 0
        #Use all designs with nb. of columns smaller than that of cur_design to do augmentation:
        while(i>1 && count<6){
          init_path = file.path(file_path,hash_list[i])
          init_design = as.matrix(read.csv(init_path,header=TRUE))[,c(-1)]
          #use augmented method to find best design, set iters.
          prev_list = fb(row,col,level,init_design,iter=3)
          crit_list = c(crit_list,prev_list$obj)
          #TO be done
          i = i-1
          count = count + 1
        }
        write.csv(crit_list,file = file.path(file_path,paste("crit_",hash_list[index],sep="")))
        minId = which.min(crit_list)
        if(minId != 1)
        {
          dir_pos_list[minId-1] = dir_pos_list[minId-1] + 1
          pos_list[minId-1] = pos_list[minId-1] + 1
          cat("cur_value = ",cur_value, "crit_list[minId] = ",crit_list[minId],"dir_pos_list[",minId-1,"]=",dir_pos_list[minId-1]," pos_list[",minId-1,"] = ",pos_list[minId-1],"\n")  
        }
        pos_list[7] = pos_list[7] + 1
        dir_pos_list[7] = dir_pos_list[7] + 1
      }
    }
  }, error=function(e){cat("ERROR :",conditionMessage(e)," file_path : ",file_path,"\n")})
  if(row >= BEGIN_ROW){write.csv(dir_pos_list,file = file.path(file_path,"dir_count_result.csv"))}
}

write.csv(pos_list,file = file.path(PATH_TO_DIR,"Count_result.csv"))

