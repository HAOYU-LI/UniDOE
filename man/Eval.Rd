\name{Eval}
\alias{Eval}
\title{Evaluate discrepency of matrix}
\usage{
Eval(x0,crit)
}
\description{
  This function takes matrix X0,q and crit to output the criterion value.
}
\arguments{
\item{x0}{an integer matrix object}

\item{crit}{an integer object, criterion to choose:

              "MD2" -- MD2

              "CL2" -- CL2

              Default: "MD2"}
}
\value{
 criterion value.
}
\examples{

  x0 = matrix(c(1,1,1,2,2,2,3,3,3),nrow=3,byrow=TRUE)
  crit="MD2"
  value = Eval(x0,crit)

}
