library(UniDOE)

#set work directory to MD2_nq directory. Then create Improvement and Discrepancy directory if not existed.
PATH_TO_FILE = "./"
dir.create(file.path(PATH_TO_FILE, "Improvement"), showWarnings = TRUE)
dir.create(file.path(PATH_TO_FILE, "Discrepancy"), showWarnings = TRUE)
tmp_MD2_nq = list.files(path = PATH_TO_FILE,pattern="*.csv")
total_MD2_files = length(tmp_MD2_nq)
number_improved = 0
process = 1
ptm <- proc.time()
for(file in tmp_MD2_nq){
  tryCatch({
    cur_design = as.matrix(read.csv(file,header=FALSE))
    if(length(cur_design)==0){total_MD2_files = total_MD2_files-1}
    else{
      run = nrow(cur_design)
      factor = ncol(cur_design)
      level = max(cur_design) - min(cur_design) + 1
      cur_value = Eval(X0 = cur_design, crit = "MD2")
      best_design = cur_design
      best_value = cur_value
      for(i in 1:3){
        impro_list = UDC(init = "input",initX = best_design,crit = "MD2")
        impro_value = impro_list$obj
        
        impro_list2 = UDC(n=run,s=factor,q=level,init="rand",crit="MD2")
        impro_value2 = impro_list2$obj
        
        if(impro_value < best_value){
          best_design = impro_list$UniDOE_Matrix
          best_value = impro_value
        }
        
        if(impro_value2 < best_value){
          best_design = impro_list2$UniDOE_Matrix
          best_value = impro_value2
        }
        
      }
      if(best_value < cur_value - 1e-12){
        number_improved = number_improved+1
        ### save new design in Improvement directory
        name = paste("./Improvement/new_",file)
        write.csv(best_design,file = name)
        tmpl = list()
        tmpl$prev_dis = cur_value
        tmpl$new_dis = best_value
        ### save discrepancy of both old and new designs in Discrepancy directory
        write.table(do.call(rbind, tmpl), file = paste("./Discrepancy/Dis_",file))
      }
    }}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  cat("at process : ",process,", file = ",file,", time used: ",(proc.time()-ptm)[3],", number_improved = ",number_improved,", prev_dis = ",cur_value,", new_dis = ",best_value,"\n")
  process = process+1
}

improving_ratio = number_improved/total_MD2_files
