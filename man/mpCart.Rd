\name{mpCart}
\alias{mpCart}
\title{Pairing two multisets}
\description{Given two multisets, the function generates a new multiset whose elements are the pairing of the corresponding ones
in the input multisets.}

\usage{
mpCart( M1,M2 ) 
}

\arguments{

\item{M1}{list of vectors}
\item{M2}{list of vectors}
}

\details{Two lists are given in input. Each list might contains more lists of two vectors: the first vector is a multiset,
the second vector is a multiplicative factor (for example its multiplicity if the multiset is a subdivision). The function 
generates a new multiset whose elements are the pairing of the corresponding ones in the two input lists, with a 
multiplicative factor which is just the product of the corresponding multiplicative factors.}

\value{
 \item{list}{list with the paired multiset}
}

\references{

E. Di Nardo, G. Guarino, D. Senato (2008) An unifying framework for k-statistics, polykays and their generalizations. 
Bernoulli. 14(2), 440-468. (download from \url{http://arxiv.org/pdf/math/0607623.pdf}) 
 
E. Di Nardo, G. Guarino, D. Senato (2008) Symbolic computation of moments of sampling distributions. 
Comp. Stat. Data Analysis. 52(11), 4909-4922. (download from \url{http://arxiv.org/abs/0806.0129}) 

E. Di Nardo, G. Guarino, D. Senato (2009) A new method for fast computing unbiased estimators of cumulants. 
Statistics and Computing, 19, 155-165.  (download from \url{https://arxiv.org/abs/0807.5008}) 

}

\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}

}

\note{Called by the function \code{\link{nPM}} in the package \code{kStatistics}.
}

\seealso{
\code{\link{pCart}}
}

\examples{

A <- list(  list( list(c(1),c(2) ),c(-1)), list(list(c(3)),c(1))  )
# where -1 is the multiplicative factor of list(c(1),c(2) ) and 1 is the 
# multiplicative factor of list(c(3))
B<-list( list( list(c(5)),c(7)))
# where 7 is the multiplicative factor of list(c(5))
 
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
