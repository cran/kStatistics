\name{oBellPol}
\alias{oBellPol}
\title{
Ordinary Bell polynomials
}
\description{The function generates a complete or a partial ordinary Bell polynomial.}

\usage{
oBellPol(n = 1, m = 0)
}

\arguments{
  \item{n}{integer, the degree of the polynomial}
  \item{m}{integer, the fixed degree of each monomial in the polynomial}
}

\details{Faa di Bruno's formula gives the coefficients of the exponential formal power series obtained 
from the composition \code{f[g()]} of the exponential formal power series \code{f} with \code{g}. The 
partial ordinary Bell polynomials \code{B[n,m]} can be expressed 
in the terms of the partial exponential Bell polynomials \code{B(n,m)(y[1],...,y[n-m+1])}  using the 
following formula:  
\tabular{l}{\code{B[n,m](y[1],...,y[n-m+1])=k!/n!B(n,m)(y[1],...,y[n-m+1]).}}  
The complete ordinary Bell polynomials are given by \code{B[n]=B[n,1]+B[n,2]+...B[n,n]}, where 
\code{B[n,m]} is the partial ordinary  Bell polynomial of order \code{(n,m)} for \code{m} from \code{1}
to \code{n}.}

\value{
 \item{string}{the expression of the polynomial}
}

\references{

C.A. Charalambides (2002) Enumerative Combinatoris, Chapman & Haii/CRC. 
 
E. Di Nardo, G. Guarino, D. Senato (2011) A new algorithm for computing the multivariate Faa di Bruno's formula. 
Appl. Math. Comp. 217, 6286-6295. (download from \url{https://arxiv.org/abs/1012.6008})

}

\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}
}

\section{Warning}{The value of the first parameter is the same as the \code{\link{MFB}} function in the univariate with 
univariate composition.}

\note{This function calls the \code{\link{MFB}} function in the \code{kStatistics} package.}


\seealso{
\code{\link{MFB}}
}


\examples{

# Return the complete ordinary Bell Polynomial for n=5, that is 
# (y1^5) + 20(y1^3)(y2) + 30(y1)(y2^2) + 60(y1^2)(y3) + 120(y2)(y3) + 120(y1)(y4) + 120(y5)
oBellPol(5)
#
# OR (same output)
#
oBellPol(5,0)

# Return the partial ordinary Bell polynomial for n=5 and m=3, that is 
# 30(y1)(y2^2) + 60(y1^2)(y3)
oBellPol(5,3)

}

% R KEYWORDS
\keyword{ univar }
\keyword{ list }
\keyword{ symbolmath }
\keyword{ combinatorics }