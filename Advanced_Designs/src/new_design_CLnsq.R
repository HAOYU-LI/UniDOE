library(UniDOE)


########move file from->to:
my.file.rename <- function(from, to) {
  todir <- dirname(to)
  if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
  file.rename(from = from,  to = to)
}
#########


#set work directory to CL2_nq directory. Then create Improvement_newalg and Discrepancy_newalg directory if not existed.
PATH_TO_FILE = "./"
dir.create(file.path(PATH_TO_FILE, "Improvement_newalg"), showWarnings = TRUE)
dir.create(file.path(PATH_TO_FILE, "Discrepancy_newalg"), showWarnings = TRUE)
tmp_CL2_nq = list.files(path = PATH_TO_FILE,pattern="*.csv")
total_CL2_files = length(tmp_CL2_nq)
number_improved = 0
process = 1
crit="CL2"
ptm <- proc.time()
for(file in tmp_CL2_nq){
  tryCatch({
    cur_design = as.matrix(read.csv(file,header=T)[,c(-1)])
    if(length(cur_design)==0){total_CL2_files = total_CL2_files-1}
    else{
      run = nrow(cur_design)
      factor = ncol(cur_design)
      level = max(cur_design) - min(cur_design) + 1
      cur_value = Eval(X0 = cur_design, crit = "CL2")
      best_design = cur_design
      best_value = cur_value
      for(i in 1:3){
        ITER = max(1e5,min(3e5,10000*run))
        
        #####random initialized imput#######
        impro_list = UDC(n=run,s=factor,q=level,init="rand",crit="CL2",maxiter = ITER)
        test_OA = LP(impro_list$UniDOE_Matrix,crit=crit,vis=FALSE,maxiter = 100000)
        impro_list = test_OA
        impro_value = impro_list$obj
        
        if(impro_value < best_value){
          best_design = impro_list$UniDOE_Matrix
          best_value = impro_value
        }
        #####random initialized imput#######
        
        
        #####best_design as input###########
        impro_list2 = UDC(n=run,s=factor,q=level,init="input",initX = best_design,crit="CL2",maxiter = ITER)
        test_OA2 = LP(impro_list2$UniDOE_Matrix,crit=crit,vis=FALSE,maxiter = 100000)
        impro_list2 = test_OA2
        impro_value2 = impro_list2$obj
        
        if(impro_value2 < best_value){
          best_design = impro_list2$UniDOE_Matrix
          best_value = impro_value2
        }
        #####best_design as input###########
        
        
        #####old_design as input###########
        impro_list3 = UDC(n=run,s=factor,q=level,init="input",initX = cur_design,crit="CL2",maxiter = ITER)
        test_OA3 = LP(impro_list3$UniDOE_Matrix,crit=crit,vis=FALSE,maxiter = 100000)
        impro_list3 = test_OA3
        impro_value3 = impro_list3$obj
        
        if(impro_value3 < best_value){
          best_design = impro_list3$UniDOE_Matrix
          best_value = impro_value3
        }
        #####old_design as input###########
        
      }
      if(best_value < cur_value - 1e-10){
        number_improved = number_improved+1
        ### save new design in Improvement_newalg directory:
        write.csv(best_design,file = paste("./Improvement_newalg/new_",file,sep=""))
        tmpl = list()
        tmpl$prev_dis = cur_value
        tmpl$new_dis = best_value
        ### save Discrepancy_newalg of both old and new designs in Discrepancy_newalg directory:
        write.table(do.call(rbind, tmpl), file = paste("./Discrepancy_newalg/Dis_",file,sep=""))
      }
      
    }}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  cat("at process : ",process,", file = ",file,", time used: ",(proc.time()-ptm)[3],", number_improved = ",number_improved,", prev_dis = ",cur_value,", new_dis = ",best_value,"\n")
  process = process+1
  my.file.rename(from = file,
                 to = paste("./already_done/",file,sep = ""))
}
improving_ratio = number_improved/total_CL2_files
