\name{e_eBellPol}
\alias{e_eBellPol}
\title{
Evaluation of exponential Bell polynomials
}
\description{The function evaluates a complete or a partial exponential Bell polynomial 
(output of the \code{\link{eBellPol}} function) when its variables are substituted with 
numerical values.}

\usage{
e_eBellPol(n=1,m=0,v=c(rep(1,n)))
}

\arguments{
  \item{n}{integer, the degree of the polynomial}
  \item{m}{integer, the fixed degree of each monomial in the polynomial}
  \item{v}{vector,  the numerical values in place of the variables of the polynomial}
}

\details{ The \code{\link{eBellPol}} function generates a complete or a partial exponential Bell polynomial
in the variables \code{y[1],..., y[n-m+1]}. The \code{\link{e_eBellPol}} function computes the value assumed 
by this polynomial when its variables are substituted with numerical values.}

\value{
 \item{numerical value}{the value assumed by the polynomial.}
}

\references{

C.A. Charalambides (2002) Enumerative Combinatoris, Chapman & Haii/CRC. 

E. Di Nardo, G. Guarino, D. Senato (2008) An unifying framework for k-statistics, polykays and their generalizations. 
Bernoulli. 14(2), 440-468. (download from \url{https://arxiv.org/pdf/math/0607623.pdf}) 
 
E. Di Nardo, G. Guarino, D. Senato (2008) Symbolic computation of moments of sampling distributions. 
Comp. Stat. Data Analysis. 52(11), 4909-4922. (download from \url{https://arxiv.org/abs/0806.0129}) 

E. Di Nardo, G. Guarino, D. Senato (2011) A new algorithm for computing the multivariate Faa di Bruno's formula. 
Appl. Math. Comp. 217, 6286--6295. (download from \url{https://arxiv.org/abs/1012.6008})

}

\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}
}

\section{Warning}{By default, the function returns the Stirling numbers of second kind.}

\note{This function calls the \code{\link{eBellPol}} function in the \code{kStatistics} package.}


\seealso{
\code{\link{eBellPol}}
}


\examples{

# Return S(5,3) = 25 (where S=Stirling number of second kind)  
e_eBellPol(5,3) 
#
# OR (same output) 
#
e_eBellPol(5,3,c(1,1,1,1,1))

# Return  B5=52 (where B5 is the 5-th Bell number)   
e_eBellPol(5) 
#
# OR (same output) 
#  
e_eBellPol(5,0) 
#
# OR (same output) 
# 
e_eBellPol(5,0,c(1,1,1,1,1))

# Return  s(5,3) = 35 (where s=Stirling number of first kind)  
e_eBellPol(5,3,c(1,-1,2,-6,24))

}

% R KEYWORDS
\keyword{ univar }
\keyword{ list }
\keyword{ symbolmath }
\keyword{ combinatorics }