\name{MFB2Set}
\alias{MFB2Set}
\title{Convert the output of the MFB function to a vector
}
\description{
Secondary function useful for manipulating the result of the MFB function
}
\usage{
MFB2Set(sExpr)
}
\arguments{
  \item{sExpr}{ MFB output }
}
\value{
 \item{set}{output as set}
}

\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}
}

\seealso{
\code{\link{MFB}}
\code{\link{Set2expr}}
}

\examples{
 
# MFB(c(3),1) generate f[3]g[1]^3 + 3f[2]g[1]g[2] + f[1]g[3]
# Convert the output of the MFB(c(3),1) to a vector using
# MFB2Set(MFB(c(5),1)). It generates the following result:
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

