\name{nKM}
\alias{nKM}
\title{Multivariate K-Statistics}
\description{Given a sample data, compute an estimate of the joint (multivariate) cumulant of a given
order for the population distribution.
}

\usage{
nKM( v, V) 
}

\arguments{

\item{v}{vector of integers}
\item{V}{vector of sample data}
}

\details{
For a sample of i.i.d. random vectors, multivariate kstatistics are unbiased estimators of 
the population joint cumulants and are expressed in terms of the power sum symmetric polynomials
in the random vectors of the sample. Thus, for the given multivariate sample data, nKM( c(r, s, ...), data) computes 
an estimate of the joint cumulant k[r, s, ...] of the population distribution.                  

}

\value{
 \item{float}{the value of the multivariate kstatistics of order v}
}

\references{

E. Di Nardo, G. Guarino, D. Senato (2008) An unifying framework for k-statistics, polykays and their generalizations. 
Bernoulli. 14(2), 440-468. (download from \url{http://arxiv.org/pdf/math/0607623.pdf}) 
 
E. Di Nardo, G. Guarino, D. Senato (2008) Symbolic computation of moments of sampling distributions. 
Comp. Stat. Data Analysis. 52(11), 4909-4922. (download from \url{http://arxiv.org/abs/0806.0129}) 

E. Di Nardo, G. Guarino, D. Senato (2009) A new method for fast computing unbiased estimators of cumulants. 
Statistics and Computing, 19, 155-165.  (download from \url{https://arxiv.org/abs/0807.5008}) 

P. McCullagh, J. Kolassa (2009), Scholarpedia, 4(3):4699. \url{http://www.scholarpedia.org/article/Cumulants}
}


\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}
}

\section{Warning}{The size of each value of the data vector must coincide with the length of the first parameter}

\note{
Called by the master function \code{\link{nPolyk}} in the package \code{kStatistics}
}



\seealso{
%\code{\link{nPolyk}},
\code{\link{nKS}},
\code{\link{nPS}},
\code{\link{nPM}}
}

\examples{

# Data assignment
data1<-list(c(5.31,11.16),c(3.26,3.26),c(2.35,2.35),c(8.32,14.34),c(13.48,49.45),
c(6.25,15.05),c(7.01,7.01),c(8.52,8.52),c(0.45,0.45),c(12.08,12.08),c(19.39,10.42))

# Return an estimate of the joint cumulant k[2,1]
nKM(c(2,1),data1)

# Data assignment
data2<-list(c(5.31,11.16,4.23),c(3.26,3.26,4.10),c(2.35,2.35,2.27),
c(4.31,10.16,6.45),c(3.1,2.3,3.2),c(3.20, 2.31, 7.3))

# Return an estimate of the joint cumulant k[2,2,2]
nKM(c(2,2,2),data2)
  
# Data assignment
data3<-list(c(5.31,11.16,4.23,4.22),c(3.26,3.26,4.10,4.9),c(2.35,2.35,2.27,2.26),
c(4.31,10.16,6.45,6.44),c(3.1,2.3,3.2,3.1),c(3.20, 2.31, 7.3,7.2))

# Return an estimate of the joint cumulant k[2,1,1,1]
nKM(c(2,1,1,1),data3)
 
}


% R KEYWORDS
\keyword{multivariate}
\keyword{univar}
\keyword{list}
\keyword{symbolmath}
