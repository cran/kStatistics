\name{nKS}
\alias{nKS}
\title{Simple K-Statistics}
\description{Given a data sample, the function returns an estimate of a cumulant with a fixed order.}

\usage{
nKS( v = NULL, V = NULL) 
}

\arguments{

\item{v}{integer or one-dimensional vector}
\item{V}{vector of a data sample}
}

\details{
For a sample of i.i.d. random variables, k-statistics are unbiased estimators with minimum variance of the population 
cumulants and are expressed in terms of power sum symmetric polynomials in the random variables 
of the sample. See the referred papers to read more about these estimators. Thus, for the input 
sample \code{data}, running \code{nKS(v,data)} or \code{nKS(c(v),data)} 
returns an estimate of the \code{v}-th cumulant of the population distribution.}

\value{
 \item{float}{the value of the k-statistics}
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
\code{\link{nKM}},
\code{\link{nPS}},
\code{\link{nPM}}
}

\examples{

# Data assignment
data<-c(16.34, 10.76, 11.84, 13.55, 15.85, 18.20, 7.51, 10.22, 12.52, 14.68, 16.08, 
19.43,8.12, 11.20, 12.95, 14.77, 16.83, 19.80, 8.55, 11.58, 12.10, 15.02, 16.83, 
16.98, 19.92, 9.47, 11.68, 13.41, 15.35, 19.11)

# Return an estimate of the cumulant of order 7
nKS(7, data) 

# Return an estimate of the cumulant of order 1, that is the mean (R command: mean(data))
nKS(1, data) 

# Return an estimate of the cumulant of order 2, that is the variance (R command: var(data))
nKS(2, data) 

# Return an estimate of the skewness (R command: skewnes(data) in the library "moments")
nKS(3, data)/sqrt(nKS(2, data))^3 

# Return an estimate of the kurtosis (R command: kurtosis(data) in the library "moments")
nKS(4, data)/nKS(2, data)^2 + 3 

}


% R KEYWORDS
\keyword{ multivariate }
\keyword{ univar }
\keyword{ list }
\keyword{ symbolmath }

