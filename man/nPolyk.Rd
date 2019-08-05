\name{nPolyk}
\alias{nPolyk}
\title{K-Statistics Master function}
\description{The master function executes one of functions to generate simple kstatistics (nKS), multivariate 
kstatistics (nKM), simple polykays (nPS), multivariate polykays (nPM) of given orders.}


\usage{
nPolyk( L, data, bhelp=NULL ) 
}

\arguments{

\item{L}{array of orders}
\item{data}{array of (multivariate) sample data}
\item{bhelp}{T=true or F=false}
}

\details{
The master function analizes the first and the second parameters and recall one of the functions
\code{nKS}, \code{nKM}, \code{nPS}, \code{nPM} in the package \code{kStatistics}. \cr \cr 
Given a sample data:\cr\cr
1) simple       k-statistics are computed using nPolyk(c(r), data)) or nPolyk(list(c(r)), data))\cr\cr
2) multivariate k-statistics are computed using nPolyk(c(r, s), data)) or nPolyk( list(c(r, s)), data))\cr\cr
3) simple       polykays     are computed using nPolyk(list(c(r),c(s)...),data))\cr\cr
4) multivariate polykays     are computed using nPolyk(list(c(r1, r2,...),c(s1, s2,...),...),data)) 
                   

}

\value{
 \item{float}{estimate of (joint) cumulant (product)}
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
The sample data array dimension depends on the first parameter.
}

\seealso{
\code{\link{nKS}},
\code{\link{nKM}},
\code{\link{nPS}},
\code{\link{nPM}}
}


\examples{

data1<-c(16.34, 10.76, 11.84, 13.55, 15.85, 18.20, 7.51, 10.22, 12.52, 14.68, 16.08,
19.43,8.12, 11.20, 12.95, 14.77, 16.83, 19.80, 8.55, 11.58, 12.10, 15.02, 16.83, 
16.98, 19.92, 9.47, 11.68, 13.41, 15.35, 19.11)

nPolyk(c(3),data1, TRUE) 
# display "KS" which indicates the type of subfunction (nKS) called by the master function nPolyk.
# KS:[1] -1.44706
 
nPolyk(c(3),data1, FALSE)
# produce (without indication of the employed subfunction)
# [1] -1.44706 

nPolyk( list(  c(2), c(1) ),data1,TRUE)
# generate the estimate of c_2*c_1 with the indication of the employed subfunction
# PS:[1] 177.4233

data2<-list(c(5.31,11.16),c(3.26,3.26),c(2.35,2.35),c(8.32,14.34),c(13.48,49.45),
c(6.25,15.05),c(7.01,7.01),c(8.52,8.52),c(0.45,0.45),c(12.08,12.08),c(19.39,10.42))

nPolyk(c(2,1),data2,TRUE)
# generate the estimate of c_(2,1) with the indication of the employed subfunction
# KM:[1] -23.7379

nPolyk( list(  c(2,1), c(1,0) ),data2,TRUE)
# generate the estimate of c_(2,1)*c_(1,0) with the indication of the employed subfunction
# PM:[1] 48.43243
}


% R KEYWORDS
\keyword{ multivariate }
\keyword{ univar }
\keyword{ list }
\keyword{ symbolmath }
