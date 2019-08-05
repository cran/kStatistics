\name{nPM}
\alias{nPM}
\title{Multivariate Polykays}
\description{Given a multivariate sample data, compute an estimate of a product of population joint cumulants using multivariate polykays.}

\usage{
nPM( v, V ) 
}

\arguments{

\item{v}{array of integers}
\item{V}{array of multivariate sample data}
          
}

\details{Multivariate polykays are symmetric statistics which generalize polykays. They are unbiased estimators of 
products of joint cumulants of a distribution and are expressed in terms of the power sum symmetric polynomials 
in the random vectors of the sample. Thus, for the given multivariate sample data, 
nPM( list( c(r1, s1, ...), c(r1, s2, ...),..  ), data) computes an estimate of the product 
c_(r1, s1,....)*c_(r2, s2, ...)... where c_(r1, s1,....),c_(r2, s2, ...), ... are 
joint cumulants of the population distribution. 
}

\value{
 \item{float}{estimate of the multivariate polykay of order the multi-index v}
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

\section{Warning}{The size of each value of the data array must coincide with the length of the first parameter.\cr
          The arrays contained in the list must have the same length.}

\note{
Called by the master function \code{\link{nPolyk}} in the package \code{kStatistics}
}


\seealso{
\code{\link{nPolyk}},
\code{\link{nKS}},
\code{\link{nKM}},
\code{\link{nPS}}
}

\examples{

data1<-list(c(5.31,11.16),c(3.26,3.26),c(2.35,2.35),c(8.32,14.34),c(13.48,49.45),
c(6.25,15.05),c(7.01,7.01),c(8.52,8.52),c(0.45,0.45),c(12.08,12.08),c(19.39,10.42));
nPM( list( c(2,1), c(1,0) ), data1)
# generate an estimate of the product c_(2,1)*c_(1,0)  where c_(2,1) and c_(1,0) are respectively 
# the cross-correlation of order (2,1) and the marginal mean of the population distribution 


data2<-list(c(5.31,11.16,4.23),c(3.26,3.26,4.10),c(2.35,2.35,2.27),
c(4.31,10.16,6.45),c(3.1,2.3,3.2),c(3.20, 2.31, 7.3));
nPM( list( c(2,0,1), c(1,1,0) ), data2) 
# generate an estimate of the product c_(2,0,1)*c(1,1,0) where c_(2,0,1) and c(1,1,0) 
# are joint cumulants of the population distribution

}


% R KEYWORDS
\keyword{ multivariate }
\keyword{ univar }
\keyword{ list }
\keyword{ symbolmath }

