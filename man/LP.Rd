\name{LP}
\alias{LP}
\title{Improve design with level permutation}
\usage{
  LP( X0, crit, maxiter, tol, th0, vis)
}
\description{
  This function aims at improving current design using exhaustive level permutation.
}
\arguments{
  \item{X0}{Current design }

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

  n=60#(must be multiples of q)
  s=10
  q=30
  crit = "MD2"#(Mixture L2 criteria)
  list1=UDC(n,s,q,crit=crit)
  LP_list = LP(X0=list1$UniDOE_Matrix,crit = crit,vis=TRUE)
}
