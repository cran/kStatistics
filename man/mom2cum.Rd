\name{mom2cum}
\alias{mom2cum}
\title{Moments in terms of cumulants 
}

\description{Compute simple and multivariate moments in terms of simple and multivariate cumulants.}

\usage{
mom2cum(n = 0)
}
\arguments{
  \item{n}{integer or vector of integers}
}

\details{Faa di Bruno's formula (the MFB function) gives the coefficients of the exponential formal power series obtained 
from the composition f(g()) of exponential formal power series. Simple moments are expressed in terms of cumulants using the Faa 
di Bruno's formula obtained from the MFB function in the case "composition of univariate f with univariate g" with f[i]=1, 
g[i]=k[i] for each i from 1 to n and k[i] cumulants. Multivariate moments are expressed in terms of multivariate cumulants 
using the Faa di Bruno's formula obtained from the MFB function in the case "composition of univariate f with multivariate 
g" whose coefficients are the multivariate cumulants.
}

\value{\item{string}{the expression of moments in terms of cumulants}}

\references{

E. Di Nardo, G. Guarino, D. Senato (2008) An unifying framework for k-statistics, polykays and their generalizations. 
Bernoulli. 14(2), 440-468. (download from \url{http://arxiv.org/pdf/math/0607623.pdf}) 

E. Di Nardo E., G. Guarino, D. Senato (2011) A new algorithm for computing the multivariate Faa di Bruno's formula. 
Appl. Math. Comp. 217, 6286--6295. (download from \url{http://arxiv.org/abs/1012.6008})

P. McCullagh, J. Kolassa (2009), Scholarpedia, 4(3):4699. \url{http://www.scholarpedia.org/article/Cumulants}


}

\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}
}

\section{Warning}{The value of the first parameter is the same as the MFB function 
in the univariate with univariate case composition and in the univariate with 
multivariate case composition.}

\note{
This function calls the \code{\link{MFB}} function in the package \code{kStatistics}.
}



\seealso{
\code{\link{MFB}}
}

\examples{
# Simple moment m[5] in terms of the cumulants k[1],...,k[5].
mom2cum(5)

# Multivariate moment m[3,1] in terms of the multivariate cumulants k[i,j] for 
# i=0,1,2,3 and j=0,1.
mom2cum(c(3,1))
}

% R KEYWORDS
\keyword{ multivariate }
\keyword{ univar }
\keyword{ list }
\keyword{ symbolmath }