\name{powS}
\alias{powS}
\title{Power sums}
\description{Given one or more lists of numbers, computes the value of the power sum symmetric polynomial with 
fixed degrees in one or more sets of variables replaced by a given list of numbers.}

\usage{
powS(vn,lvd) 
}

\arguments{

\item{vn}{vector of powers (degrees)}
\item{lvd}{list of numbers}
}

\details{
Given the lists (x[1],x[2],...), (y[1],y[2],...), (z[1],z[2],...), ... in L and the integers (n,m,j,...) in vn, the
function powS( vn ,L ) returns (x[1]^n)*(y[1]^m)*(z[1]^j)*...+(x[2]^n)*(y[2]^m)*(z[2]^j)*+...See also the examples.
}

\value{
 \item{integer}{value of the power sum symmetric polynomial}
}

\references{

E. Di Nardo, G. Guarino, D. Senato (2008) Symbolic computation of moments of sampling distributions. 
Comp. Stat. Data Analysis. 52(11), 4909-4922. (download from \url{http://arxiv.org/abs/0806.0129}) 

}

\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}
}

\note{
Called by the functions \code{\link{nKS}}, \code{\link{nKM}}, \code{\link{nPS}}, \code{\link{nPM}} in the 
package \code{kStatistics}.
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
