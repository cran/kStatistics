\name{powS}
\alias{powS}
\title{Power sums}
\description{The function returns the value of the power sum symmetric 
polynomial, with fixed degrees and in one or more sets of variables, when the 
variables are substituted with the input lists of numerical values.}

\usage{
powS(vn = NULL, lvd = NULL) 
}

\arguments{

\item{vn}{vector of integers (the powers of the indeterminates)}
\item{lvd}{list of numerical values in place of the variables}
}

\details{Given the lists of numerical values \code{(x[1],x[2],...), (y[1],y[2],...), (z[1],z[2],...), ...} 
in the input parameter \code{lvd} and the integers \code{(n,m,j,...)} in the input parameter \code{vn}, 
the \code{\link{powS}} function returns the value of 
\code{(x[1]^n)*(y[1]^m)*(z[1]^j)*...+(x[2]^n)*(y[2]^m)*(z[2]^j)*+...}. 
}

\value{
 \item{integer}{the value of the polynomial}
}

\references{

E. Di Nardo, G. Guarino, D. Senato (2008) Symbolic computation of moments of sampling distributions. 
Comp. Stat. Data Analysis. 52(11), 4909-4922. (download from \url{https://arxiv.org/abs/0806.0129}) 

}

\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}
}

\note{
Called by the  \code{\link{nKS}}, \code{\link{nKM}}, \code{\link{nPS}} and \code{\link{nPM}} functions 
in the \code{kStatistics} package.
}


\examples{

# Return 1^3 + 2^3 + 3^3 = 36
powS(c(3), list(c(1),c(2),c(3))) 

# Return (1^3 * 4^2) + (2^3 * 5^2) + (3^3 * 6^2) = 1188
powS(c(3,2),list(c(1,4),c(2,5),c(3,6))) 

}


% R KEYWORDS
\keyword{ multivariate }
\keyword{ univar }
\keyword{ list }
\keyword{ symbolmath }
