\name{mpCart}
\alias{mpCart}
\title{Join two lists}
\description{Given two lists with elements of the same type, the function returns a new list whose elements are the joining of 
the two original lists, except for the last elements, which are multiplied.}

\usage{
mpCart( M1 = NULL, M2 = NULL ) 
}

\arguments{

\item{M1}{list of vectors}
\item{M2}{list of vectors}
}

\details{The input of the \code{\link{mpCart}} function are two lists. Each list might contain multiple lists 
of two vectors: the first vector contains multisets whose elements are of the same type (integers or vectors
with the same lenght), the second vector is a number (for example a multiplicity if the multiset is a 
subdivision). The \code{\link{mpCart}} function generates a new list of two vectors: the first is 
obtained by joining the first vectors in the two input lists, the second is just the product 
of the numbers in the second vectors. See the examples.}

\value{
 \item{list}{the list with the joined input lists}
}

\references{

E. Di Nardo, G. Guarino, D. Senato (2008) An unifying framework for k-statistics, polykays and their generalizations. 
Bernoulli. 14(2), 440-468. (download from \url{https://arxiv.org/pdf/math/0607623.pdf}) 
 
E. Di Nardo, G. Guarino, D. Senato (2008) Symbolic computation of moments of sampling distributions. 
Comp. Stat. Data Analysis. 52(11), 4909-4922. (download from \url{https://arxiv.org/abs/0806.0129}) 

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
# where 
# -1 is the multiplicative factor of list(c(1),c(2) ) 
#  1 is the multiplicative factor of list(c(3))
B <- list( list( list(c(5)),c(7)))
# where 7 is the multiplicative factor of list(c(5))
 
# Return [[[1],[2],[5]], -7] , [[[3],[5]], 7]
mpCart(A,B) 

A <- list( list( list( c(1,0),c(1,0) ), c(-1)),  list( list( c(2,0)), c(1) ))
# where
# - 1 is the multiplicative factor of list( c(1,0),c(1,0) )
#   1 is the multiplicative factor of list( c(2,0) )
B <- list( list( list( c(1,0)), c(1)) )
# where 1 is the multiplicative factor of list( c(1,0))

# Return [[[1,0],[1,0],[1,0]], -1], [[[2,0],[1,0]],1]
mpCart(A,B)
 

}

% R KEYWORDS
\keyword{ multivariate }
\keyword{ univar }
\keyword{ list }
\keyword{ symbolmath }
