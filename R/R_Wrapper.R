library(Rcpp)

UDC <- function(n,s,q,init="rand",initX=matrix(0),crit="MD2",maxiter=100000,hits_ratio = 0.1,vis=FALSE)
{
  if(init=="input"){
    n = nrow(initX)
    s = ncol(initX)
    q = max(initX) - min(initX) + 1
  }
  #restrictions for arguments:
  if(is.numeric(n)&&is.numeric(s)&&is.numeric(q) == FALSE){stop("Wrong types of n,s,q.")}
  else if(n%%q != 0){stop("n should be multiple of q. ")}
  else if(s<=1 || n<=2 ){stop(("\n The size of design should be larger than 2*2."))}
  else if((init == "orth" || init == "input") && length(initX)==1){stop(("\n when init=Orth or input, input of initX is needed."))}
  else if((init == "orth" || init == "input") && (nrow(initX) != n || range(initX)[2] != q)){
  stop(("\n The size of Orthogonal matrix mismatches with n,s,q ."))
  }else if((init == "orth") && ncol(initX) == s){stop(("\n If s = ncol(initX), then no update is proceeded."))}

  if(crit == "CL2"){crit=2}
  else if(crit == "maximin"){crit=1}
  else{crit=4}

  #recall Rcpp compiled StoUDC function:
  list <- StoUDC(n,s,q,init,initX,crit,maxiter,hits_ratio)
  if(vis == TRUE){
    plot(list$obj_list,type="l")
    bst_score = list$obj
    min_index = which.min(list$obj_list)[1]
    abline(v = min_index,col=2)
    abline(h =list$obj_list[min_index],col=4 )
    title(main = c("best_score = ",bst_score))
  }
  return (list)
}


AUDC <- function (X0,n,crit="MD2",maxiter=100000,hits_ratio = 0.1,vis=FALSE)
{

  s = ncol(X0)
  q = max(X0) - min(X0) + 1
  #restrictions for arguments:
  if(n<=1 || s<=0 || q <=1 ){stop("Error: Please input valid X0.")}
  else if(is.matrix(X0)==FALSE){stop("Error: Please input X0 to do the augmented searching. End of program...")}
  else if(is.numeric(n) == FALSE){stop("Error: Wrong types of n. End of program...")}
  else if(n%%q != 0){stop("Error: n should multiple of level. End of program...")}

 
  if(crit == "CL2"){crit=2}
  else if(crit == "maximin"){crit=1}
  else{crit=4}

  #recall Rcpp compiled StoAUDC function:
  list <- StoAUDC(X0,n,s,q,init="rand",initX=matrix(0),crit,maxiter,hits_ratio)
  if(vis == TRUE){
    plot(list$obj_list,type="l")
    bst_score = list$obj
    min_index = which.min(list$obj_list)[1]
    abline(v = min_index,col=2)
    abline(h =list$obj_list[min_index],col=4 )
    title(main = c("best_score = ",bst_score))
  }
  return(list)
}


LP<- function(X0=matrix(0),crit="MD2",maxiter=10000,hits_ratio = 0.1,vis=FALSE)
{
  if(crit == "CL2"){crit=2}
  else if(crit == "maximin"){crit=1}
  else{crit=4}

  q = max(X0) - min(X0) + 1
  list <- StoLP(X0,q,crit,maxiter,hits_ratio)
  if(vis == TRUE){
    plot(list$obj_list,type="l")
    bst_score = list$obj
    min_index = which.min(list$obj_list)[1]
    abline(v = min_index,col=2)
    abline(h =list$obj_list[min_index],col=4 )
    title(main = c("best_score = ",bst_score))
  }
  return (list)
}


Eval<-function(X0 = matrix(0), crit="MD2")
{
  q = max(X0) - min(X0) + 1
  if(crit == "MD2"){crit=0}
  else{crit=1}

  return(StoEval(X0,q,crit))
}

RUDC <- function (X0,n,s,q,init="rand",initX=matrix(0),crit="MD2",J=10,maxiter=2000,
                  hits_ratio = c(0.3,0.2,0.15,0.1,0.05,0.01),vis=FALSE)
{

  s = ncol(X0)
  q = max(X0) - min(X0) + 1

  #restrictions for arguments:
  if(is.numeric(n)&&is.numeric(s)&&is.numeric(q) == FALSE){stop("Wrong types of n,s,q. End of program...")}
  else if(is.matrix(X0)==FALSE){stop("Please input X0 to do the augmented searching. End of program...")}
  else if(n<=1 || s<=0 || q <=1 ){stop("Please input valid X0.")}
  else if(n%%q != 0){stop("n should multiple of q. End of program...")}
  else if(ncol(X0) != s){stop("Inconsistency in X0 columns and s. End of program...")}

  if(crit == "CL2"){crit=2}
  else if(crit == "maximin"){crit=1}
  else{crit=4}

  #recall Rcpp compiled StoAUDC function:
  #list0 <- StoAUDC(X0,n,s,q,init,initX,crit,maxiter,hits_ratio[1])
  I = length(hits_ratio)
  start_time = proc.time()
  list_new <- UDC(n,s,q,init,initX,crit,maxiter,hits_ratio[1])
  D1 = list_new$UniDOE_Matrix
  D_global = rbind(X0,list_new$UniDOE_Matrix)
  Dis_glob = Eval(D_global,crit=crit)
  Dis_list = c()
  Dis_list_cur = c()
  for(i in 1:I)
  {
    for(j in 1:J)
    {
      D_tmp_list = UDC(n,s,q,init="input",initX=as.matrix(D1),crit,maxiter,hits_ratio[i])
      D_tmp_1 = D_tmp_list$UniDOE_Matrix
      D_tmp_cur = rbind(X0,D_tmp_1)
      Dis_cur = Eval(D_tmp_cur,crit=crit)
      Dis_list_cur = c(Dis_list_cur,Dis_cur)
      if( Dis_cur < Dis_glob)
      {
        Dis_glob = Dis_cur
        D1 = D_tmp_1
        D_global = D_tmp_cur
      }
    }
    Dis_list = c(Dis_list,Dis_list_cur)
    Dis_list_cur = c()
  }
  return_list = list(D1,D_global,Dis_glob,Dis_list,(proc.time() - start_time)[3])
  names(return_list) = c(Aug_Matrix,Objective_Matrix,obj,obj_list,time_consumed)

  if(vis == TRUE){
    plot(return_list$obj_list,type="l")
    bst_score = return_list$obj
    min_index = which.min(return_list$obj_list)[1]
    abline(v = min_index,col=2)
    abline(h =return_list$obj_list[min_index],col=4 )
    title(main = c("best_score = ",bst_score))
  }
  return(return_list)
}

