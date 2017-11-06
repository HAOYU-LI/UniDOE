library(UniDOE)
setwd("I:/Online_design_data")
#initM = read.csv("CL2_27_13_3.csv",header=TRUE)[,c(-1)]
initM = read.csv("./OA_27_13_3.csv",header=FALSE)
initM = initM + 1
initM = as.matrix(initM)
crit = "MD2"

#===================================================================================================================
#Infinite loop:
flag = TRUE
col_list = c(1,2,3,4,5,6,7,8,9,10,11,12,13)
min_obj = 65
best_design = matrix(0)
best_initM = matrix(0)
best_index = -1
best_cols = c(3,4,5,6,7,8,9,10,11,12)
MD_list = c()
while(flag){
  ran_num = sample(c(5,6),1)
  #fixed_col = sample(col_list,ran_num)
  fixed_col = c(3,5,7,12,13)
  testM = initM[,fixed_col]
  test_list = UDC(n=27,s=13,q=3,init="orth",initX = testM, crit="MD2",vis = FALSE, maxiter = 20000)
  min_test_OA = 65
  for(j in 1:20){
    test_OA = LP(test_list$UniDOE_Matrix,crit=crit,vis=FALSE)
    if(test_OA$obj < min_test_OA){
      min_test_OA = test_OA$obj
    }
    if(test_OA$obj < min_obj){
      min_obj = test_OA$obj
      best_design = test_OA$UniDOE_Matrix
      best_initM = testM
      best_cols = fixed_col
    }
  }
  MD_list = c(MD_list,min_test_OA)
  cat("test_list$obj = ",test_list$obj,"; minimum value at this round = ",min_test_OA,"; fixed_col = ",fixed_col,"; current min crit value = ",min_obj,"\n")
  if(min_obj <= 62.79874){
    flag = FALSE
    cat("Goal reached!!!!!!!!!\n")
    file_name = paste("crit_27_3_13=",min_obj,".csv")
    write.csv(best_design,file = file_name)
    write.csv(best_cols,file = paste("fixed_col=",best_cols,".csv"))
  }
}
cat("Best value is : ",min_obj,"!!!!!!!!!\n")
best_design
#===================================================================================================================




#=============================================68 non-isomorphic designs=============================================
library(UniDOE)
setwd("I:/Online_design_data/LT96_68_designs/LT68_matrix/")
crit = "MD2"
for (file in list.files()){
    cat("\n\n================================At file : ",file,"==========================\n")
    start_time <- proc.time()
    initM = t(as.matrix(read.csv(file,header=TRUE)[,c(-1)]))
    if(Eval(initM,crit=crit) < 63){
    #60 loop:
    # flag = TRUE
    col_list = c(1,2,3,4,5,6,7,8,9,10,11,12,13)
    min_obj = 65
    maxiter = 1e5
    best_design = matrix(0)
    best_initM = matrix(0)
    best_index = -1
    best_cols = c(6,7,8,9,10)
    MD_list = c()
      for(m in 1:60){
        ran_num = sample(c(4,5),1)
        fixed_col = sample(col_list,ran_num)
        testM = initM[,c(fixed_col)]
        test_list = UDC(n=27,s=13,q=3,init="orth",initX = testM, crit="MD2",vis = FALSE, maxiter = 300000)
        min_test_OA = 65
        test_OA = LP(test_list$UniDOE_Matrix,crit=crit,maxiter = maxiter,vis=FALSE)
        MD_list = c(MD_list,test_OA$obj)
        if(test_OA$obj < min_test_OA){
          min_test_OA = test_OA$obj
        }
        if(test_OA$obj < min_obj){
          min_obj = test_OA$obj
          best_design = test_OA$UniDOE_Matrix
          best_initM = testM
          best_cols = fixed_col
        }
        cat("At iter ",m,"; Time used: ",(proc.time() - start_time)[3],"; test_list$obj = ",test_list$obj,"; minimum value at this round = ",min_test_OA,"; fixed_col = ",fixed_col,"; current min crit value = ",min_obj,"\n")
        if(min_obj <= 62.79874){
          # flag = FALSE
          cat("Goal reached!!!!!!!!!\n")
          file_name = paste("./result/initD=",sub(".txt .csv","",file),"_minMD2=",min_obj,".csv")
          details = c()
          details$index = file
          details$fixedCol = best_cols
          details$final_obj = min_obj
          details$time =  (proc.time() - start_time)[3]
          details$DesignFile = file_name
          write.csv(details,file = paste("./result/minMD2=",min_obj,"_initD=_",sub(".txt .csv","",file),"_Details.txt"))
          write.csv(best_design,file = file_name)
          write.csv(best_cols,file = paste("./result/minMD2=",min_obj,"_fixed_col=",best_cols,".csv"))
        }
      }
    cat("Best value is : ",min_obj,"!!!!!!!!!\n")
    #best_design
    #boxplot(MD_list,main="100 iterations of LP with 5 randomly fixed columns",ylab = "Discrepency value",xlab = paste("level permutation :",maxiter))
    write.csv(MD_list,file = paste("./result/SUSPENDAT_",sub(".txt .csv","",file),".txt"))
    cat("===================================================================================================================\n")
  }
}
#===================================================================================================================





#==================================================Adaptive element-wise exchange algorithm============================================================
library(UniDOE)
setwd("I:/Online_design_data")
#initM = read.csv("CL2_27_13_3.csv",header=TRUE)[,c(-1)]
initM = read.csv("./OA_27_13_3.csv",header=FALSE)
initM = initM + 1
initM = as.matrix(initM)
crit = "MD2"
col_list = c(1,2,3,4,5,6,7,8,9,10,11,12,13)
min_obj = 65
best_design = matrix(0)
best_initM = matrix(0)
best_index = -1
#best_cols = c(3,4,5,6,7,8,9,10,11,12)
best_cols = c(4,5,6)
crit_lists_col = data.frame()
for(c_col in best_cols){
  MD_list = c()
  for(m in 1:100){
    fixed_col = sample(col_list,c_col)
    testM = initM[,c(fixed_col)]
    test_list = UDC(n=27,s=13,q=3,init="orth",initX = testM, crit="MD2",vis = FALSE, maxiter = 20000)
    min_test_OA = 65
    for(j in 1:10){
      test_OA = LP(test_list$UniDOE_Matrix,crit=crit,vis=FALSE)
      if(test_OA$obj < min_test_OA){
        min_test_OA = test_OA$obj
      }
      if(test_OA$obj < min_obj){
        min_obj = test_OA$obj
        best_design = test_OA$UniDOE_Matrix
        best_initM = testM
        best_cols = fixed_col
      }
    }
    MD_list = c(MD_list,min_test_OA)
    cat("At iter ",m,"; c_col = ",c_col,"; test_list$obj = ",test_list$obj,"; minimum value at this round = ",min_test_OA,"; fixed_col = ",fixed_col,"; current min crit value = ",min_obj,"\n")
  }

  if(c_col == 4){
    list_frame = data.frame(MD_list)
    colnames(list_frame) <- c(c_col)
  }else{
    list_frame$c_col = MD_list
    colnames(list_frame) <- c(4:c_col)
  }
  print(list_frame)
}


colnames(list_frame) <- c("s-9","s-8","s-7")
list_frame_new = read.csv("./100Rounds_crit_c(4,5,6)_new.csv",header = TRUE)[,c(-1)]
colnames(list_frame_new) <- c("s-9","s-8","s-7")

X = c("s-8","s-8","s-7","s-7")
Y = cbind(list_frame_new$`s-9`,list_frame_new$`s-8`,list_frame_new$`s-7`)
Y_old = cbind(list_frame$`s-9`,list_frame$`s-8`,list_frame$`s-7`)

boxplot(Y_old[,2],Y[,2],Y_old[,3],Y[,3],
        names = X,horizontal = FALSE,col = c(2,4,2,4),plot = TRUE,range=100,
        main = "100 rounds Comparison between fixed and adaptive element-exchange algorithm",
        Xlab = "columns",
        ylab = "Mixture L2 discrepancy value")
legend("topright",
       c("Fixed element-wise exchange","adaptive element-wise exchange"),
       lty=c(1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5),col=c("red","blue"))

dev.copy(jpeg,filename="30Round_crit_plot_comparison.jpg");
dev.off()
write.csv(list_frame[,c(-1)],file = "100Rounds_crit_c(5,6)_old.csv")
write.csv(list_frame_new[,c(-1)],file = "100Rounds_crit_c(5,6)_new.csv")
#===================================================================================================================


#==============================================visualization========================================================
setwd("I:/Online_design_data")
list_frame = read.csv("./paper/UniDOE/30Rounds_crit_list.csv",header = TRUE)[,c(-1)]
colnames(list_frame) <- c("s-10","s-9","s-8","s-7","s-6","s-5","s-4","s-3","s-2","s-1")
X = c("s-10","s-9","s-8","s-7","s-6","s-5","s-4","s-3","s-2","s-1")
Y = cbind(list_frame$`s-10`,list_frame$`s-9`,list_frame$`s-8`,list_frame$`s-7`
          ,list_frame$`s-6`,list_frame$`s-5`,list_frame$`s-4`
          ,list_frame$`s-3`,list_frame$`s-2`,list_frame$`s-1`)

boxplot(Y,
        names = X,horizontal = FALSE,col = c(2,2,2,2,2,2,2,2,2,2),plot = TRUE,range=100,
        main = "MD discrepancy for 30 rounds s-1 to s-10 warm start design",
        Xlab = "Randomly fixed column number",
        ylab = "Mixture L2 discrepancy value")

#===================================================================================================================





