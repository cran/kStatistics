\name{pCart}
\alias{pCart}
\title{Cartesian product}
\description{The function returns the cartesian product between vectors.}

\usage{
pCart( L ) 
}

\arguments{

\item{L}{vectors in a list}
}

\details{The function pairs any element of the first vector with any element of the second vector, iteratively 
if there are more than two vectors in input. Repetitions are allowed. See examples. 
}

\value{
 \item{list}{list with the cartesian product}
}

\references{
Knuth, Donald E. (1998). The Art of Computer Programming. (3rd ed.). Addison Wesley.
}

\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}
}

\note{
Called by the function \code{\link{nPS}} in the package \code{kStatistics}.
}


\seealso{
\code{\link{mpCart}}
}

\examples{

A <- c(1,2)
B <- c(3,4,5) 
# Return the cartesian product [[1,3],[1,4],[1,5],[2,3],[2,4],[2,5]]
pCart( list( A, B) ) 
 

L1<-list( c(1,1), c(2)) 
L2<-list( c(5,5), c(7) )
# Return the Cartesian product [[1,1],[5,5]], [[1,1],[7]], [[2],[5,5]], [[2],[7]]
# assign the value to L3
L3<-pCart ( list(L1, L2) )
 
# Return the Cartesian product between L3 and [7]
# The result will be [[1,1],[5,5],[7]], [[1,1],[7],[7]], [[2],[5,5],[7]], [[2],[7],[7]]
pCart ( list(L3, c(7)) ) 

}


% R KEYWORDS
\keyword{ multivariate }
\keyword{ univar }
\keyword{ list }
\keyword{ symbolmath }

