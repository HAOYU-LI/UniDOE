\name{UDC}
\alias{UDC}
\title{Uniform Design Construction}
\usage{
UDC(n,s,q,init,initX,crit,maxiter,tol,th0,vis)
}
\description{
  This function takes n,s,q and other arguments to output a list(described below).
}
\arguments{
\item{n}{an integer R object}

\item{s}{an integer R object}

\item{q}{an integer R object}

\item{init}{an string vector R object: "rand"(default), "orth" or "input"}

\item{initX}{an user-defined integer matrix R object, which is fixed in search. Needed when init="orth" or init="input"}

\item{crit}{an character R object. Type of criterion to use.

         "maximin" -- maximin Discrepancy    ;

         "CL2"  --Centered L2 Discrepancy ;

         "MD2"  --Mixture L2  Discrepancy ;}

\item{maxiter}{a positive integer  R  object}

\item{tol}{an integer R  object}

\item{th0}{an integer R  object}

\item{vis}{an boolean R object}

}
\value{
  A list that contains Initial design matrix(Init_Matrix),optimal design matrix(UniDOE_Matrix), initial criterion value(critobj0), final criterion value(critobj) and criterion list in update process.
}
\examples{
##e.g.1
n=12#(must be multiples of q)
s=3
q=4
crit = "MD2"#(Mixture L2 criteria)
list1=UDC(n,s,q,crit=crit)

##e.g.2
n=10 #(must be multiples of q)
s=3
q=5
init = "rand"
crit = "MD2" #(Mixture L2 criteria)
vis=TRUE
list1=UDC(n,s,q,init=init,crit=crit,vis=vis)

##e.g.3
#If init = "orth", algorithm will fix columns of initX and expand total number of columns to s for final design.
n=6
s=5
q=3
init = "orth"
initX = matrix(c(1,1,2,2,3,3,3,3,1,1,2,2),ncol=2)# initX should be orthogonal design with nrow=n&level=q
list = UDC(n,s,q,init,initX)

##e.g.4
#If init="input", algorithm will search for better a better design with same size as initX.
init = "input"
initX = matrix(c(1,1,2,2,3,3,3,3,1,1,2,2),ncol=2)# initX should be orthogonal design with nrow=n&level=q
list = UDC(init=init,initX = initX)

}
