\name{powS}
\alias{powS}
\title{Power sums}
\description{Given one or more lists of numbers, computes the value of the power 
sum symmetric polynomial with fixed degrees in one or more sets of variables plugged by the given numbers .}

\usage{
powS(vn,lvd) 
}

\arguments{

\item{vn}{array of powers (degrees)}
\item{lvd}{list of numbers}
}

\details{
Given the lists (x[1],x[2],...), (y[1],y[2],...), (z[1],z[2],...), ... in L and the integers (n,m,j,...) in vn powS( vn ,L ) 
returns (x[1]^n)*(y[1]^m)*(z[1]^j)*...+(x[2]^n)*(y[2]^m)*(z[2]^j)*+...See also the examples.
}

\value{
 \item{integer}{value of the power sum symmetric polynomial}
}

\references{
Di Nardo E., G. Guarino, D. Senato (2008) Symbolic computation of moments of sampling distributions. 
Comp. Stat. Data Analysis Vol. 52, no. 11, 4909--4922, (download from \url{http://www.elviradinardo.it/lavori1.html}) 
}

\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}
}

\note{
Called by the functions \code{\link{nKS}}, \code{\link{nKM}}, \code{\link{nPS}}, \code{\link{nPM}} in the 
package \code{kStatistics}
}


\examples{

powS(c(3), list(c(1),c(2),c(3))) 
# computes 1^3 + 2^3 + 3^3 = 36
    
powS(c(3,2),list(c(1,4),c(2,5),c(3,6))) 
# computes (1^3 * 4^2) + (2^3 * 5^2) + (3^3 * 6^2) = 1188

}


% R KEYWORDS
\keyword{ multivariate }
\keyword{ univar }
\keyword{ list }
\keyword{ symbolmath }
