\name{mpCart}
\alias{mpCart}
\title{Pairing two multisets}
\description{Given two multisets, generates a new multiset whose elements are the pairing of the corresponding ones
in the input multisets.}

\usage{
mpCart( M1,M2 ) 
}

\arguments{

\item{M1}{list of arrays}
\item{M2}{list of arrays}
}

\details{Two lists of two arrays are given in input: the multiset is in the first array, in the second there is a 
multiplicative factor (for example its multiplicity if the multiset is a subdivision). The function generates a 
new multiset whose elements are the pairing of the corresponding ones in two input multisets with a 
multiplicative factor which is just the product of the corresponding multiplicative factors.}

\value{
 \item{list}{list with the paired multiset}
}

\references{

E. Di Nardo, G. Guarino, D. Senato (2008) An unifying framework for k-statistics, polykays and their generalizations. 
Bernoulli. Vol. 14(2), 440-468. (download from \url{http://www.elviradinardo.it/lavori1.html}) 
 
E. Di Nardo, G. Guarino, D. Senato (2008) Symbolic computation of moments of sampling distributions. 
Comp. Stat. Data Analysis Vol. 52(11), 4909-4922, (download from \url{http://www.elviradinardo.it/lavori1.html}) 

E. Di Nardo, G. Guarino, D. Senato (2009) A new method for fast computing unbiased estimators of cumulants. 
Statistics and Computing, Vol. 19, 155-165.  (download from \url{http://www.elviradinardo.it/lavori1.html}) 

}

\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}

}

\note{
Called by the function \code{\link{nPM}} in the package \code{kStatistics}
}

\seealso{
\code{\link{pCart}}
}

\examples{

A <- list(  list( list(c(1),c(2) ),c(-1)), list(list(c(3)),c(1))  )
# where -1 is the multiplicative factor of the first array and 1 is the 
# multiplicative factor of the second array
B<-list( list(c(5),c(7)))
# where 7 is the multiplicative factor of the first array  
mpCart(A,B) 
# generates [[[1],[2],[5]], -7] , [[[3],[5]], 7]

A <- list( list( list( c(1,0),c(1,0) ), c(-1)),  list( list( c(2,0)), c(1) ))
B <- list( list( list( c(1,0)), c(1)) )  
mpCart(A,B)
# generates  [[[1,0],[1,0],[1,0]], -1], [[[2,0],[1,0]],1]
}

% R KEYWORDS
\keyword{ multivariate }
\keyword{ univar }
\keyword{ list }
\keyword{ symbolmath }
