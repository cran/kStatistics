\name{cum2mom}
\alias{cum2mom}
\title{Cumulants in terms of moments
}
\description{The function computes a simple or a multivariate cumulant in terms of simple or multivariate moments.}

\usage{
cum2mom(n = 1)
}

\arguments{
  \item{n}{integer or vector of integers}
}

\details{Faa di Bruno's formula (the  \code{\link{MFB}} function) gives the coefficients of the exponential formal power series 
\code{f[g()]} where \code{f} and \code{g} are exponential formal power series too. Simple cumulants 
are expressed in terms of simple moments using the Faa di Bruno's formula obtained from the  \code{\link{MFB}} function in the case 
"composition of univariate \code{f} with univariate \code{g}" with \code{f[i]=(-1)^(i-1)*(i-1)!, g[i]=m[i]} 
for \code{i} from 1 to \code{n} and \code{m[i]} moments. Multivariate cumulants are expressed in terms 
of multivariate moments using the Faa di Bruno's formula obtained from the  \code{\link{MFB}} function in the case "composition 
of univariate \code{f} with multivariate \code{g}". In such a case the coefficients of \code{g} are the multivariate moments.}

\value{
\item{string}{the expression of the cumulant in terms of moments}
}

\references{

E. Di Nardo, G. Guarino, D. Senato (2008) An unifying framework for k-statistics, polykays and their generalizations. 
Bernoulli. 14(2), 440-468. (download from \url{https://arxiv.org/pdf/math/0607623.pdf}) 

E. Di Nardo E., G. Guarino, D. Senato (2011) A new algorithm for computing the multivariate Faa di Bruno's formula. 
Appl. Math. Comp. 217, 6286--6295. (download from \url{https://arxiv.org/abs/1012.6008})

P. McCullagh, J. Kolassa (2009) Scholarpedia, 4(3):4699. \url{http://www.scholarpedia.org/article/Cumulants}

}
\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}
}

\section{Warning}{The value of the first parameter is the same as the \code{\link{MFB}} function in the univariate with 
univariate case composition and in the univariate with multivariate case composition.}

\note{
This function calls the \code{\link{MFB}} function in the \code{kStatistics} package.
}



\seealso{
\code{\link{MFB}}
}


\examples{
# Return the simple cumulant k[5] in terms of the simple moments m[1],..., m[5].
cum2mom(5)

# Return the multivariate cumulant k[3,1] in terms of the multivariate moments m[i,j] for 
# i=0,1,2,3 and j=0,1.
cum2mom(c(3,1))
}

% R KEYWORDS
\keyword{ multivariate }
\keyword{ univar }
\keyword{ symbolmath }