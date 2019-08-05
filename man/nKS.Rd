\name{nKS}
\alias{nKS}
\title{Simple K-Statistics}
\description{Given a sample data, compute an estimate of the cumulant of a given order for the population distribution.}

\usage{
nKS( v, V ) 
}

\arguments{

\item{v}{integer or one-dimensional array}
\item{V}{array of sample data}
}

\details{
For a sample of i.i.d. random variables, kstatistics are unbiased estimators of the population cumulants and are
expressed in terms of the power sum symmetric polynomials in the random variables of the sample. Thus, for the 
given sample data, nKS(n,data) or nKS(c(n),data) computes an estimate of the n-th cumulant of the
population distribution.}

\value{
 \item{float}{the value of the kStatistics of order v}
}

\references{

E. Di Nardo, G. Guarino, D. Senato (2008) An unifying framework for k-statistics, polykays and their generalizations. 
Bernoulli. Vol. 14(2), 440-468. (download from \url{http://www.elviradinardo.it/lavori1.html}) 
 
E. Di Nardo, G. Guarino, D. Senato (2008) Symbolic computation of moments of sampling distributions. 
Comp. Stat. Data Analysis Vol. 52(11), 4909-4922, (download from \url{http://www.elviradinardo.it/lavori1.html}) 

E. Di Nardo, G. Guarino, D. Senato (2009) A new method for fast computing unbiased estimators of cumulants. 
Statistics and Computing, Vol. 19, 155-165.  (download from \url{http://www.elviradinardo.it/lavori1.html}) 

P. McCullagh, J. Kolassa (2009), Scholarpedia, 4(3):4699. \url{http://www.scholarpedia.org/article/Cumulants}
}


\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}
}

\note{
Called by the master function \code{\link{nPolyk}} in the package \code{kStatistics}
}



\seealso{
\code{\link{nPolyk}},
\code{\link{nKM}},
\code{\link{nPS}},
\code{\link{nPM}}
}

\examples{

data<-c(16.34, 10.76, 11.84, 13.55, 15.85, 18.20, 7.51, 10.22, 12.52, 14.68, 16.08, 
19.43,8.12, 11.20, 12.95, 14.77, 16.83, 19.80, 8.55, 11.58, 12.10, 15.02, 16.83, 
16.98, 19.92, 9.47, 11.68, 13.41, 15.35, 19.11)

nKS(7, data) 
# generate an estimate of the cumulant of order 7

nKS(1, data) 
# generate an estimate of the cumulant of order 1, that is the mean (R command: mean(data))

nKS(2, data) 
# generate an estimate of the cumulant of order 2, that is the variance (R command: var(data))

nKS(3, data)/sqrt(nKS(2, data))^3 
# generate an estimate of the skewness (R command: skewnes(data) in the library "moments")

nKS(4, data)/nKS(2, data)^2 + 3 
# generate an estimate of the kurtosis (R command: kurtosis(data) in the library "moments")
}


% R KEYWORDS
\keyword{ multivariate }
\keyword{ univar }
\keyword{ list }
\keyword{ symbolmath }

