################################## Adaptive D(27_3_13) ##########################################

library(UniDOE)
setwd("I:/Online_design_data")
crits_UniDOE_adaptive = c()
time_UniDOE_adaptive =c()
n = 27
s = 13
q = 3
for(i in 1:2){
  startTime =  proc.time()
  Xopt_UniDOE <- UDC(n = n,s=s,q = q,crit = "MD2",maxiter = 100000,vis=TRUE)
  min_crit = Xopt_UniDOE$obj
  opt_list = Xopt_UniDOE
  for(j in 1:10){
    LP_Xopt = LP(Xopt_UniDOE$UniDOE_Matrix,crit="CL2",vis=FALSE)
    if(LP_Xopt$obj < min_crit){
      min_crit = LP_Xopt$obj
      opt_list = LP_Xopt
    }
  }
  Xopt_UniDOE = opt_list
  crits_UniDOE_adaptive = c(crits_UniDOE_adaptive,Xopt_UniDOE$obj)
  cur_time = (proc.time()-startTime)[3]
  cat("UniDOE, itr = ",i,"; crit = ",Xopt_UniDOE$obj,"; time = ",cur_time,"\n")
  time_UniDOE_adaptive = c(time_UniDOE_adaptive,cur_time)
}

write.csv(crits_UniDOE_adaptive,file = "30Rounds_crit_MD27_3^13_newAlgo_200000_noLP.csv")
write.csv(time_UniDOE_adaptive,file = "30Rounds_time_MD27_3^13_newAlgo_200000_noLP.csv")

################################## Adaptive D(27_3_13) ##########################################



################################## Fixed D(27_3_13) ##########################################

library(UniDOE)
setwd("I:/Online_design_data")
crits_UniDOE_fixed = c()
time_UniDOE_fixed =c()
init = matrix(0)
n=27
s=13
q = 3
for(i in 1:10){
  startTime =  proc.time()
  Xopt_UniDOE <- UDC(n = n,s=s,q = q,crit = "MD2",maxiter = 100000)
  # if(Xopt_UniDOE$obj<62.88){
  #   init = Xopt_UniDOE$Init_Matrix
  #   write.csv(init,file = "./init_design.csv")
  #   write.csv(Xopt_UniDOE$UniDOE_Matrix,file = "./best_design.csv")
  # }

  # min_crit = Xopt_UniDOE$obj
  # opt_list = Xopt_UniDOE
  # for(j in 1:10){
  #   LP_Xopt = LP(Xopt_UniDOE$UniDOE_Matrix,crit="MD2",vis=FALSE)
  #   if(LP_Xopt$obj < min_crit){
  #     min_crit = LP_Xopt$obj
  #     opt_list = LP_Xopt
  #   }
  # }
  # Xopt_UniDOE = opt_list
  crits_UniDOE_fixed = c(crits_UniDOE_fixed,Xopt_UniDOE$obj)
  cur_time = (proc.time()-startTime)[3]
  time_UniDOE_fixed = c(time_UniDOE_fixed,cur_time)
  cat("UniDOE, itr = ",i,"; crit = ",Xopt_UniDOE$obj,"; time = ",cur_time,"\n")
}

# write.csv(crits_UniDOE_fixed,file = "10Rounds_crit_MD27_3^13_fixed_200000_noLP.csv")
# write.csv(time_UniDOE_fixed,file = "10Rounds_time_MD27_3^13_fixed_200000_noLP.csv")

write.csv(crits_UniDOE_fixed,file = "10Rounds_crit_CL30_30^15_fixed_200000_noLP.csv")
write.csv(time_UniDOE_fixed,file = "10Rounds_time_CL30_30^15_fixed_200000_noLP.csv")

################################## Fixed D(27_3_13) ##########################################


crits_UniDOE_adaptive = read.csv("./30Rounds_crit_MD27_3^13_adaptive_200000.csv",header=TRUE)[,c(-1)]
crits_UniDOE_fixed = read.csv("./30Rounds_crit_MD27_3^13_fixed_200000.csv",header=TRUE)[,c(-1)]

boxplot(crits_UniDOE_fixed,crits_UniDOE_adaptive,
        names = c("fixed_J","adaptive_J"),horizontal = FALSE,col = c(2,4),plot = TRUE,range=100,
        main = "30 rounds Comparison between fixed and adaptive element-exchange on MD_27(3^13)",
        Xlab = "columns",
        ylab = "Mixture L2 discrepancy value")



library(UniDOE)
init = matrix(0)
n=30
s=15
q = 30
for(i in 1:20){
  startTime =  proc.time()
  Xopt_UniDOE <- UDC(n = n,s=s,q = q,crit = "CL2",maxiter = 30000)
  min_crit = Xopt_UniDOE$obj
  opt_list = Xopt_UniDOE
  for(j in 1:10){
    LP_Xopt = LP(Xopt_UniDOE$UniDOE_Matrix,crit="CL2",vis=FALSE)
    if(LP_Xopt$obj < min_crit){
      min_crit = LP_Xopt$obj
      opt_list = LP_Xopt
    }
  }
  Xopt_UniDOE = opt_list
  cur_time = (proc.time()-startTime)[3]
  cat("UniDOE, itr = ",i,"; crit = ",Xopt_UniDOE$obj,"; time = ",cur_time,"\n")
}


