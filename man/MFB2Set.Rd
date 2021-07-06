\name{MFB2Set}
\alias{MFB2Set}
\title{Convert the output of the MFB function into a vector
}
\description{
Secondary function useful for manipulating the result of the \code{\link{MFB}} function.
}
\usage{
MFB2Set(sExpr="")
}
\arguments{
  \item{sExpr}{the output of the \code{\link{MFB}} function}
}
\value{
 \item{set}{a set}
}

\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}
}

\seealso{
\code{\link{MFB}},
\code{\link{Set2expr}}
}

\examples{
 
# Run MFB(c(3),1) to generate f[3]g[1]^3 + 3f[2]g[1]g[2] + f[1]g[3]
# Convert the output of the MFB(c(3),1) into a vector using
# MFB2Set(MFB(c(3),1)). The result is the following:
# "1" "1" "f" "3" "1"
# "1" "1" "g" "1" "3"
# "2" "3" "f" "2" "1"
# "2" "1" "g" "1" "1"
# "2" "1" "g" "2" "1"
# "3" "1" "f" "1" "1"
# "3" "1" "g" "3" "1"
MFB2Set(MFB(c(3),1))
}


% R KEYWORDS
\keyword{ multivariate }
\keyword{ univar }
\keyword{ list }
\keyword{ symbolmath }

