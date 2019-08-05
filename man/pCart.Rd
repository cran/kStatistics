\name{pCart}
\alias{pCart}
\title{Cartesian product}
\description{Generates the Cartesian product between arrays}

\usage{
pCart( L ) 
}

\arguments{

\item{L}{arrays in a list}
}

\details{The function pairs any element of the first array with any element of the second array, iteratively 
if there are more than two arrays in input. Repetitions are allowed. See examples. 
}

\value{
 \item{list}{list with the Cartesian product}
}

\references{
Knuth, Donald E. (1998). The Art of Computer Programming. (3rd ed.). Addison Wesley.
}

\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}
}

\note{
Called by the function \code{\link{nPS}} in the package \code{kStatistics}
}


\seealso{
\code{\link{mpCart}}
}

\examples{

A <- c(1,2)
B <- c(3,4,5) 
pCart( list( A, B) ) 
# generate the Cartesian product [[1,3],[1,4],[1,5],[2,3],[2,4],[2,5]] 

L1<-list( c(1,1), c(2)) 
L2<-list( c(5,5), c(7) )
L3<-pCart ( list(L1, L2) )
# generate the Cartesian product [[1,1],[5,5]], [[1,1],[7]], [[2],[5,5]], [[2],[7]] 

pCart ( list(L3, c(7)) ) 
# generate the Cartesian product [[1,1],[5,5],[7]], [[1,1],[7],[7]], [[2],[5,5],[7]], [[2],[7],[7]] 

}


% R KEYWORDS
\keyword{ multivariate }
\keyword{ univar }
\keyword{ list }
\keyword{ symbolmath }

