\name{kStatistics-package}
\alias{kStatistics-package}
\alias{kStatistics}
\alias{kstatistics}
\alias{k-Statistics}
\alias{k-statistics}
\docType{package}
\title{
\packageTitle{kStatistics}
}
\description{kStatistics is a package allowing to estimate (joint)cumulants and products of (joint)cumulants of a given 
population distribution using (multivariate) k-statistics and (multivariate) polykays, which are symmetric unbiased 
estimators. The procedures rely on a symbolic method arising from the classical umbral calculus 
and described in the referred papers. In the package, a set of combinatorial devices are given such 
as integer partitions, set partitions and multiset subdivisions, pairing and merging of multisets, useful in the 
construction of these estimators.}

\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}


\packageAuthor{kStatistics}

Maintainer: \packageMaintainer{kStatistics}
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

\keyword{ multivariate }
\keyword{ univar }
\keyword{ list }
\keyword{ symbolmath }

\examples{
# Some of the most important functions:

data1<-c(16.34, 10.76, 11.84, 13.55, 15.85, 18.20, 7.51, 10.22, 12.52, 14.68, 
16.08, 19.43,8.12, 11.20, 12.95, 14.77, 16.83, 19.80, 8.55, 11.58, 12.10, 
15.02, 16.83, 16.98, 19.92, 9.47, 11.68, 13.41, 15.35, 19.11)

data2<-list(c(5.31,11.16),c(3.26,3.26),c(2.35,2.35),c(8.32,14.34),c(13.48,49.45),
c(6.25,15.05),c(7.01,7.01),c(8.52,8.52),c(0.45,0.45),c(12.08,12.08),c(19.39,10.42))

nPolyk(c(3), data1, TRUE)
# generate an estimate of the third cumulant of the population distribution with the indication 
# of which function has been employed
# KS:[1] -1.44706

nPolyk( list(  c(2), c(1) ), data1, TRUE)
# generate an estimate of the product of the mean and the variance of the population distribution 
# with the indication of which function has been employed 
# PS:[1] 177.4233

nPolyk(c(2,1), data2, TRUE);
# generate an estimate of the joint cumulant c_(2,1) of the population distribution  with the 
# indication of which function has been employed
# KM:[1] -23.7379

nPolyk( list(  c(2,1), c(1,0) ), data2, TRUE)
# generate an estimate of the product of joint cumulants c_(2,1)*c(1,0) of the population 
# distribution with the indication of which function has been employed
# PM:[1] 48.43243

mkmSet(3)
# generate all subdivisions of a multiset with only one element of multiplicity 3
 
mkmSet(c(2,1)) 
# generate all subdivisions of a multiset with two elements, 
# having multiplicity respectively 2 and 1
 
}
