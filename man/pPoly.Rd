\name{pPoly}
\alias{pPoly}
\title{Product of Polynomials}
\description{The function returns the product between polynomials without constant term.
}

\usage{
pPoly( L ) 
}

\arguments{

\item{L}{lists of coefficients of the polynomials}
}

\value{
 \item{vector}{coefficients of the polynomial output of the product}
}

\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}
}

\note{
Called by the functions \code{\link{nKS}}, \code{\link{nKM}} in the package \code{kStatistics}.
}

\examples{

# c(1,-3) are the coefficients of (x-3x^2), c(2) is the coefficient of 2x
# generate c(0, 2,-6), coefficients of 2x^2-6x^3 =(x-3x^2)*(2x) 
pPoly(list(c(1,-3), c(2)))

# c(0,3,-2) are the coefficients of 3x^2-2x^3, c(0,2,-1) are the coefficients of (2x^2-x^3)
# generate c(0,0,0,6,-7,2), coefficients of 6x^4-7x^5+2x^6=(3x^2-2x^3)*(2x^2-x^3)
pPoly(list(c(0,3,-2),c(0,2,-1)))                       

}


% R KEYWORDS
\keyword{ multivariate }
\keyword{ univar }
\keyword{ list }
\keyword{ symbolmath }
