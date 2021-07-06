\name{nPS}
\alias{nPS}
\title{Simple Polykays}
\description{Given a data sample, the function returns an estimate of a product of cumulants with fixed orders.} 

\usage{
nPS( v = NULL, V = NULL) 
}

\arguments{

\item{v}{vector of integers}
\item{V}{vector of a data sample}
}

\details{Simple polykays or generalized k-statistics are unbiased estimators of cumulant products
with minimum variance.
See the referred papers to read more about these estimators. Simple polykays are usually expressed 
in terms of power sum symmetric polynomials in the i.i.d. random variables of the sample. Thus, 
for the input sample \code{data}, running \code{nPS(c(i,j,...),data)} returns an estimate of 
the product \code{k[i]*k[j]*...} with \code{k[i], k[j], ...} the cumulants of the population 
distribution and \code{v=(i,j,...)} their fixed orders.
}

\value{
 \item{float}{the estimate of the polykay}
}

\references{

E. Di Nardo, G. Guarino, D. Senato (2008) An unifying framework for k-statistics, polykays and their generalizations. 
Bernoulli. 14(2), 440-468. (download from \url{https://arxiv.org/pdf/math/0607623.pdf}) 
 
E. Di Nardo, G. Guarino, D. Senato (2008) Symbolic computation of moments of sampling distributions. 
Comp. Stat. Data Analysis. 52(11), 4909-4922. (download from \url{https://arxiv.org/abs/0806.0129}) 

E. Di Nardo, G. Guarino, D. Senato (2009) A new method for fast computing unbiased estimators of cumulants. 
Statistics and Computing, 19, 155-165.  (download from \url{https://arxiv.org/abs/0807.5008}) 

P. McCullagh, J. Kolassa (2009), Scholarpedia, 4(3):4699. \url{http://www.scholarpedia.org/article/Cumulants}
}


\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}
}

\note{
Called by the master \code{\link{nPolyk}} function in the \code{kStatistics} package. 
}


\seealso{
\code{\link{nPolyk}},
\code{\link{nKS}},
\code{\link{nKM}},
\code{\link{nPM}}
}


\examples{

# Data assignment
data<-c(16.34, 10.76, 11.84, 13.55, 15.85, 18.20, 7.51, 10.22, 12.52, 14.68, 16.08, 
19.43,8.12, 11.20, 12.95, 14.77, 16.83, 19.80, 8.55, 11.58, 12.10, 15.02, 16.83, 
16.98, 19.92, 9.47, 11.68, 13.41, 15.35, 19.11)

# Return an estimate of the product k[2]*k[1], where k[1] and k[2] are the mean and 
# the variance of the population distribution respectively
nPS(c(2,1), data) 

}


% R KEYWORDS
\keyword{ multivariate }
\keyword{ univar }
\keyword{ list }
\keyword{ symbolmath }


