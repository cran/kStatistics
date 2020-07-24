\name{gpPart}
\alias{gpPart}
\title{General partition polynomial}
\description{The function returns general partition polynomials.}

\usage{
gpPart(n = 0)
}

\arguments{
  \item{n}{integer}
}
\details{Faa di Bruno's formula gives the coefficients of the exponential formal power series obtained from the composition f(g()) 
of exponential formal power series. General partition polynomials in the variables y1,...,yn are recovered from Faa di Bruno's 
formula obtained from the MFB function in the case "composition of univariate f with univariate g" by setting f[i]=ai and g[i]=yi, 
for each i from 1 to n.}

\value{
\item{string}{expression of the general partition polynomial}
}

\references{ 

C.A. Charalambides (2002) Enumerative Combinatoris, Chapman & Haii/CRC. 

E. Di Nardo, G. Guarino, D. Senato (2011) A new algorithm for computing the multivariate Faa di Bruno's formula. 
Appl. Math. Comp. 217, 6286--6295. (download from \url{http://arxiv.org/abs/1012.6008})

}

\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}
}

\section{Warning}{The value of the first parameter is the same as the MFB function in the univariate with univariate 
case composition.}

\note{
This function calls the function \code{\link{MFB}} in the package \code{kStatistics}}


\seealso{
\code{\link{MFB}}
}


\examples{

# Return the general partition polynomial G[2], that is a2(y1^2) + a1(y2)
gpPart(2)

# Return the general partition polynomial G[5], that is a5(y1^5) + 10a4(y1^3)(y2) + 15a3(y1)(y2^2) 
# + 10a3(y1^2)(y3) + 10a2(y2)(y3) + 5a2(y1)(y4) + a1(y5)
gpPart(5)

}

% R KEYWORDS
\keyword{multivariate}
\keyword{univar}
\keyword{list}
\keyword{symbolmath}
\keyword{combinatorics}