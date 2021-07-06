\name{nPolyk}
\alias{nPolyk}
\title{K-Statistics Master function}
\description{The master function executes one of the functions to compute simple k-statistics \code{(nKS)}, 
multivariate k-statistics \code{(nKM)}, simple polykays \code{(nPS)} or multivariate polykays \code{(nPM)}.}


\usage{
nPolyk( L = NULL, data = NULL, bhelp=NULL ) 
}

\arguments{

\item{L}{vector of orders}
\item{data}{vector of a (univariate or multivariate) sample data}
\item{bhelp}{\code{T=true} or \code{F=false}}
}

\details{
The master function analizes the first two input variables and recalls one of the \code{nKS}, \code{nKM}, 
\code{nPS} or \code{nPM}  functions in the \code{kStatistics} package. \cr \cr 
Given a sample data:
\enumerate{
\item simple       k-statistics are computed using \code{nPolyk(c(r), data))} or \code{nPolyk(list(c(r)), data))} 
\cr
\item multivariate k-statistics are computed using \code{nPolyk(c(r, s), data))} or \code{nPolyk( list(c(r, s)), 
data))} \cr
\item simple       polykays     are computed using \code{nPolyk(list(c(r),c(s)...),data))} \cr
\item multivariate polykays     are computed using \code{nPolyk(list(c(r1, r2,...),c(s1, s2,...),...),data))}}          
}

\value{
 \item{float}{the estimate of the (joint) cumulant or of the (joint) cumulant product}
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
The dimension of the vector with the sample data depends on the first parameter.
}

\seealso{
\code{\link{nKS}},
\code{\link{nKM}},
\code{\link{nPS}},
\code{\link{nPM}}
}


\examples{

# Data assignment
data1<-c(16.34, 10.76, 11.84, 13.55, 15.85, 18.20, 7.51, 10.22, 12.52, 14.68, 16.08,
19.43,8.12, 11.20, 12.95, 14.77, 16.83, 19.80, 8.55, 11.58, 12.10, 15.02, 16.83, 
16.98, 19.92, 9.47, 11.68, 13.41, 15.35, 19.11)

# Display "KS:[1] -1.44706" which indicates the type of subfunction (nKS) called by 
# the master function nPolyk and gives the estimate of the third cumulant 
nPolyk(c(3),data1, TRUE) 
 
# Display "[1] -1.44706" (without the indication of the employed subfunction)
nPolyk(c(3),data1, FALSE)

# Display "PS:[1] 177.4233" which indicates the type of subfunction (nPS) called by 
# the master function nPolyk and gives the estimate of the product between the 
# variance k[2] and the mean k[1] 
nPolyk( list(  c(2), c(1) ),data1,TRUE)

# Data assignment
data2<-list(c(5.31,11.16),c(3.26,3.26),c(2.35,2.35),c(8.32,14.34),c(13.48,49.45),
c(6.25,15.05),c(7.01,7.01),c(8.52,8.52),c(0.45,0.45),c(12.08,12.08),c(19.39,10.42))

# Display "KM:[1] -23.7379" which indicates the type of subfunction (nKM) called by 
# the master function nPolyk and gives the estimate of k[2,1]  
nPolyk(c(2,1),data2,TRUE)

# Display "PM:[1] 48.43243" which indicates the type of subfunction (nPM) called by 
# the master function nPolyk and gives the estimate of k[2,1]*k[1,0]  
nPolyk( list(  c(2,1), c(1,0) ),data2,TRUE)

}


% R KEYWORDS
\keyword{ multivariate }
\keyword{ univar }
\keyword{ list }
\keyword{ symbolmath }
