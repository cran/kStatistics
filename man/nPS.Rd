\name{nPS}
\alias{nPS}
\title{Simple Polykays}
\description{Given a sample data, compute an estimate of a product of population cumulants using
simple Polykays (generalized k-statistics).} 

\usage{
nPS( v, V ) 
}

\arguments{

\item{v}{array of integers}
\item{V}{array of sample data}
}

\details{Products of kstatistics are known as polykays. They are unbiased estimators of products of cumulants of 
a distribution and are expressed in terms of the power sum symmetric polynomials in the random variables of the 
sample. Thus, for the given sample data, nPS(c(i,j,...),data) computes an estimate of the product c_i*c_j*... 
where c_i, c_j, ... are cumulants of the population distribution.
}

\value{
 \item{float}{estimate of the polykay of order v}
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
\code{\link{nKS}},
\code{\link{nKM}},
\code{\link{nPM}}
}


\examples{

data<-c(16.34, 10.76, 11.84, 13.55, 15.85, 18.20, 7.51, 10.22, 12.52, 14.68, 16.08, 
19.43,8.12, 11.20, 12.95, 14.77, 16.83, 19.80, 8.55, 11.58, 12.10, 15.02, 16.83, 
16.98, 19.92, 9.47, 11.68, 13.41, 15.35, 19.11)
nPS(c(2,1), data) 
# generate an estimate of the product c_2*c_1, where c_1 and c_2 are respectively 
# the mean and the variance of the population distribution  

}


% R KEYWORDS
\keyword{ multivariate }
\keyword{ univar }
\keyword{ list }
\keyword{ symbolmath }


