\name{list2m}
\alias{list2m}
\title{List To Multiset}
\description{The function returns a multiset representation of a vector or a list, in increasing order.}

\usage{
list2m( v ) 
}

\arguments{

\item{v}{single vector or list of vectors}
}

\details{Given a list as input, the function returns a structure as [[e1,e2,...], m1], [[f1,f2,...], m2],... where m1, m2,... 
are the instances of c(e1,e2,...), c(f1,f2,...), ... in the input v.}

\value{
 \item{multiset}{list of multiset}
}

\references{
Knuth, Donald E. (1998). The Art of Computer Programming. (3rd ed.). Addison Wesley.

}

\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}
}

\note{
Called by the function \code{countP} in the package \code{kStatistics}.
}

\seealso{
\code{\link{list2Set}},
\code{\link{m2Set}},
\code{\link{countP}}
}

\examples{

# Return the multiset [[1],3], [[2],1] from the input vector (1,2,1,1)  
list2m(c(1,2,1,1 )) 

# Return the multiset [[1,2],2], [[2,3],1] from the input list (c(1,2),c(2,3),c(1,2))  
list2m(list(c(1,2),c(2,3),c(1,2))) 

}


% R KEYWORDS
\keyword{multivariate}
\keyword{univar}
\keyword{list}
\keyword{symbolmath}
\keyword{combinatorics}



