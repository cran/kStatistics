\name{gpPart}
\alias{gpPart}
\title{General partition polynomial}
\description{The function returns a general partition polynomial.}  

\usage{
gpPart(n = 0)
}

\arguments{
  \item{n}{integer}
}
\details{Faa di Bruno's formula gives the coefficients of the exponential formal power series composition 
\code{f[g()]} obtained from the composition of the exponential formal power series \code{f} with \code{g}. 
General partition polynomials in the variables \code{y[1],...,y[n]} are recovered from the Faa di Bruno's 
formula (output of the \code{\link{MFB}} function) in the case "composition of univariate 
\code{f} with univariate \code{g}" by setting \code{f[i]=ai} and \code{g[i]=y[i]}, for \code{i} 
from \code{1} to \code{n}.}

\value{
\item{string}{the expression of the polynomial}
}

\references{ 

C.A. Charalambides (2002) Enumerative Combinatoris, Chapman & Haii/CRC. 

E. Di Nardo, G. Guarino, D. Senato (2011) A new algorithm for computing the multivariate Faa di Bruno's formula. 
Appl. Math. Comp. 217, 6286--6295. (download from \url{https://arxiv.org/abs/1012.6008})

}

\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}
}

\section{Warning}{The value of the first parameter is the same as the \code{\link{MFB}} function 
in the univariate with univariate composition.}

\note{
This function calls the \code{\link{MFB}} function in the \code{kStatistics} package.}


\seealso{
\code{\link{MFB}}
}


\examples{

# Return the general partition polynomial G[a1,a2; y1,y2], that is a2(y1^2) + a1(y2)
gpPart(2)

# Return the general partition polynomial G[a1,a2,a3,a4,a5; y1,y2,y3,y4,y5], that is 
# a5(y1^5) + 10a4(y1^3)(y2) + 15a3(y1)(y2^2) + 10a3(y1^2)(y3) + 10a2(y2)(y3) + 5a2(y1)(y4) 
# + a1(y5)
gpPart(5)

}

% R KEYWORDS
\keyword{multivariate}
\keyword{univar}
\keyword{list}
\keyword{symbolmath}
\keyword{combinatorics}