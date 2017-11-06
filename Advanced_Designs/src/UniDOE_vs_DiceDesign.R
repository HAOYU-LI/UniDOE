install.packages("DiceDesign")
library(UniDOE)
library(DiceDesign)

# Comparison One:
setwd("I:/Online_design_data/")
crit_UniDOE = "CL2"
crit_dice = "C2"

dimension = 15
n = 30

################################DiceDesign_ESE################################
startTime =  proc.time()
crits_dice_ESE = c()
time_dice_ESE = c()
for(i in 1:30){
  startTime =  proc.time()
  X = lhsDesign(n,dimension,randomized = FALSE)$design
  Xopt_dice <-  discrepESE_LHS(X,T0=0.005*discrepancyCriteria(X,type='C2')[[1]],inner_it=100,J=50,it=50)
  crits_dice_ESE = c(crits_dice_ESE,Eval(Xopt_dice$design*30+0.5,crit = "CL2"))
  cat("dice, itr = ",i,"; crit = ",Eval(Xopt_dice$design*30+0.5,crit = "CL2"),"; time = ",(proc.time()-startTime)[3],"\n")
  time_dice_ESE = c(time_dice_ESE,(proc.time()-startTime)[3])
}
##############################################################################



################################DiceDesign_SA################################
startTime =  proc.time()
crits_dice_SA = c()
time_dice_SA = c()
for(i in 1:30){
  startTime =  proc.time()
  X = lhsDesign(n,dimension,randomized = FALSE)$design
  Xopt_dice <-  discrepSA_LHS(X,T0=10,c=0.99,it=50000,criterion="C2")
  crits_dice_SA = c(crits_dice_SA,Eval(Xopt_dice$design*30+0.5,crit = "CL2"))
  cat("dice, itr = ",i,"; crit = ",Eval(Xopt_dice$design*30+0.5,crit = "CL2"),"; time = ",(proc.time()-startTime)[3],"\n")
  time_dice_SA = c(time_dice_SA,(proc.time()-startTime)[3])
}
#############################################################################



################################UniDOE################################
crits_UniDOE = c()
time_UniDOE = c()
for(i in 1:30){
  startTime =  proc.time()
  Xopt_UniDOE <- UDC(n = n,s=dimension,q = n,crit = "CL2",maxiter = 5000)
  crits_UniDOE = c(crits_UniDOE,Xopt_UniDOE$obj)
  cat("UniDOE, itr = ",i,"; crit = ",Xopt_UniDOE$obj,"; time = ",(proc.time()-startTime)[3],"\n")
  time_UniDOE = c(time_UniDOE,(proc.time()-startTime)[3])
}
#####################################################################



################################Write data################################
df = data.frame(crits_dice_ESE,crits_dice_SA,crits_UniDOE)
write.csv(df,file = "UniDOE_dice_crit_5000it_n30s15.csv")
df_time = data.frame(time_dice_ESE,time_dice_SA,time_UniDOE)
write.csv(df_time,file = "UniDOE_dice_time_5000it_n30s15.csv")
##########################################################################


################################save and draw  criterion ################################
#png(filename = "UniDOE_dice_crit_5000it_n30s15")
crits_dice_ESE = crit_lst[,c(1)]
crits_dice_SA = crit_lst[,c(2)]
crits_UniDOE = crit_lst[,c(3)]
boxplot(crits_dice_ESE,crits_dice_SA,crits_UniDOE,
        names = c(paste("DiceDesign_ESE : time(s)=",floor(sum(time_dice_ESE))),paste("DiceDesign_SA : time(s)=",floor(sum(time_dice_SA))),paste("UniDOE : time(s)=",floor(sum(time_UniDOE)))),
        horizontal = FALSE,col = 2,plot = TRUE,range=0.000001,
        main = "Criterion comparison between DiceDesign and UniDOE",
        Xlab = "packages",
        ylab = "Centralized L2 Discrepancy")
dev.copy(jpeg,filename="UniDOE_dice_crit_5000it_n30s15.jpg");
dev.off()
########################################################################################



#############################save and draw  time########################################
setwd("I:/Online_design_data/src/")
time_lst = read.csv("../UniDOEvsDiceDesign/UniDOE_dice_time_5000it_n30s15.csv",header = TRUE)[,c(-1)]
crit_lst = read.csv("../UniDOEvsDiceDesign/UniDOE_dice_crit_5000it_n30s15.csv",header = TRUE)[,c(-1)]
time_dice_ESE = time_lst[,c(1)]
time_dice_SA = time_lst[,c(2)]
time_UniDOE = time_lst[,c(3)]
Rounds = c(1:30)
plot(Rounds,time_dice_ESE,col = 2,plot = TRUE,type = "l",
     main = "Time comparison between DiceDesign and UniDOE",
     Xlab = "packages",
     ylab = "Time used(s) for one round",
     ylim=c(0,300))
par(new=T)
plot(Rounds,time_dice_SA,col = 3,plot = TRUE,ylim=c(0,300),type = "l",
      ylab = "Time used(s) for one round")
par(new=T)
plot(Rounds,time_UniDOE,col = 4,plot = TRUE,ylim=c(0,300),type = "l",
      ylab = "Time used(s) for one round")
legend("topright",
       c("DiceDesign_ESE", "DiceDesign_SA","UniDOE"),
       lty=c(1,1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5,2.5),col=c("red","green","blue"))
dev.copy(jpeg,filename="UniDOE_dice_crit_5000it_n30s15.jpg");
dev.off()
################################################################################




##########################Comparison between fixed and adaptive element-wise exchange algorithm##################
############################################Based on CL_30(30^15)################################################

############Adaptive###########
library(UniDOE)
setwd("I:/Online_design_data")
crits_UniDOE_adaptive = c()
time_UniDOE_adaptive = c()
n=30
dimension=15
for(i in 1:20){
  startTime =  proc.time()
  Xopt_UniDOE <- UDC(n = n,s=dimension,q = n,crit = "CL2",maxiter = 100000)
  crits_UniDOE_adaptive = c(crits_UniDOE_adaptive,Xopt_UniDOE$obj)
  cat("UniDOE, itr = ",i,"; crit = ",Xopt_UniDOE$obj,"; time = ",(proc.time()-startTime)[3],"\n")
  time_UniDOE_adaptive = c(time_UniDOE_adaptive,(proc.time()-startTime)[3])
}
#write.csv(crits_UniDOE_adaptive,file = "30Rounds_crit_CL30_30^15_adaptive_5000.csv")
write.csv(time_UniDOE_adaptive,file = "20Rounds_time_CL30_30^15_adaptive_100000.csv")
############Adaptive###########


############Fixed##############
library(UniDOE)
setwd("I:/Online_design_data")
crits_UniDOE_fixed = c()
time_UniDOE_fixed =c()
n=30
dimension=15
for(i in 1:20){
  startTime =  proc.time()
  Xopt_UniDOE <- UDC(n = n,s=dimension,q = n,crit = "CL2",maxiter = 100000)
  crits_UniDOE_fixed = c(crits_UniDOE_fixed,Xopt_UniDOE$obj)
  cat("UniDOE, itr = ",i,"; crit = ",Xopt_UniDOE$obj,"; time = ",(proc.time()-startTime)[3],"\n")
  time_UniDOE_fixed = c(time_UniDOE_fixed,(proc.time()-startTime)[3])
}
#write.csv(crits_UniDOE_fixed,file = "20Rounds_crit_CL30_30^15_fixed.csv")
write.csv(time_UniDOE_fixed,file = "20Rounds_time_CL30_30^15_fixed_100000.csv")
############Fixed##############


##########################visualization and comparison##########################
crits_UniDOE_adaptive = read.csv("./20Rounds_crit_CL30_30^15_adaptive.csv",header = TRUE)[,c(-1)]

boxplot(crits_UniDOE_fixed,crits_UniDOE_adaptive,
        names = c("fixed_J","adaptive_J"),horizontal = FALSE,col = c(2,4),plot = TRUE,range=100,
        main = "20 rounds Comparison between fixed and adaptive element-exchange on CL_30(30^15)",
        Xlab = "columns",
        ylab = "Centered L2 discrepancy value")
abline(h =  0.384186, col = 3)
legend("topright",
       c("Fixed element-wise exchange","adaptive element-wise exchange","old state-of-art"),
       lty=c(1,1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5,1),col=c("red","blue","green"))
dev.copy(jpeg,filename="20Round_crit_plot_comparison.jpg");
dev.off()

##########################visualization and comparison###########################

##########################visualization for fixed-adaptive###########################
time_UniDOE_fixed = read.csv("../Online_design_data/20Rounds_time_CL30_30^15_fixed_100000.csv",header = TRUE)[,c(-1)]
time_UniDOE_adaptive = read.csv("../Online_design_data/20Rounds_time_CL30_30^15_adaptive_100000.csv",header = TRUE)[,c(-1)]
x = c(1:20)

matplot(x,cbind(time_UniDOE_fixed,time_UniDOE_adaptive),
        type="l",col=c("red","blue"),
        main = "20 rounds time for fixed and adaptive element-exchange on CL_30(30^15)",
        xlab = "Rounds",
        ylab = "Time(s) used for each round",
        lty=c(1,1),
        lwd = c(2,2))

legend("topright",
       c("Fixed element-wise exchange","adaptive element-wise exchange"),
       lty=c(1,1), # gives the legend appropriate symbols (lines)
       lwd=c(2.5,2.5),col=c("red","blue"))
##########################visualization for fixed-adaptive###########################



