\name{AUDC}
\alias{AUDC}
\title{Augmented Uniform Design Construction}
\usage{
AUDC(X0,n,s,q,init,initX,crit,maxiter,hits_ratio,vis)
}
\description{
    This function takes n,s,q; a unchanged initial design and other arguments to output a list(described below).
}
\arguments{
\item{X0}{an integer matrix R object}

\item{n}{an integer R object}

\item{s}{an integer R object}

\item{q}{an integer R object}

\item{init}{an string vector R object: "rand"(default) or "input"}

\item{initX}{an user-defined integer matrix R object, which is fixed in search. Needed when init="orth" or init="input"}

\item{crit}{an character R object. Type of criterion to use.

      "maximin" -- maximin Discrepancy    ;

      "CL2"     --Centered L2 Discrepancy ;

      "MD2"     --Mixture L2  Discrepancy ;}

\item{maxiter}{a positive integer  R  object}

\item{hits_ratio}{an float R object. Default value is 0.1, which is the ratio to accept changes of design in inner for loop. Details can be checked in (Zhang, A. and Li, H. (2017). UniDOE: an R package for uniform design construction via stochastic optimization.)}

\item{vis}{an boolean R object}
}
\value{
  A list that contains optimal design matrix(UniDOE_Matrix), initial criterion value(critobj0), final criterion value(critobj) and criterion list in update process.
}
\examples{

#e.g.1.
#Set a fixed initial matrix:
n=12#(must be multiples of q)
s=3
q=4
mat0 = matrix(c(1,1,1,2,2,2,3,3,3),ncol=3,byrow=TRUE)# nb. of columns=s
crit = "MD2"#(Mixture L2 criteria)
list1=AUDC(X0=mat0,n,s,q,crit=crit)

#e.g.2.
#Set a fixed initial matrix with visualization:
n=8#(must be multiples of q)
s=3
q=4
mat0 = matrix(c(1,1,1,2,2,2,3,3,3),ncol=3,byrow=TRUE)# nb. of columns=s
init = "rand"
crit = "MD2"#(Mixture L2 criteria)
vis= TRUE
list1=AUDC(X0=mat0,n,s,q,init=init,crit=crit,vis=vis)

}
