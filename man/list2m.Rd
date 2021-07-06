\name{list2m}
\alias{list2m}
\title{List To Multiset}
\description{The function returns the multiset representation of a vector or a list, in 
increasing order.}

\usage{
list2m( v=c(0) ) 
}

\arguments{

\item{v}{single vector or list of vectors}
}

\details{Given a list in input, the \code{\link{list2m}} function returns a structure as 
\code{[[e1,e2,...], m1], [[f1,f2,...], m2],...} where \code{m1, m2,...} are the instances 
of \code{c(e1,e2,...), c(f1,f2,...), ...} in the input vector \code{v}.}

\value{
 \item{multiset}{the list of multisets}
}

\references{
D.E. Knuth  (1998) The Art of Computer Programming. (3rd ed.) Addison Wesley.

}

\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}
}

\note{
Called by the \code{countP} function in the \code{kStatistics} package.
}

\seealso{
\code{\link{list2Set}},
\code{\link{m2Set}},
\code{\link{countP}}
}

\examples{

# Return the list of multisets [[1],3], [[2],1] from the input vector (1,2,1,1)  
list2m(c(1,2,1,1 )) 

# Return the list of multisets [[1,2],2], [[2,3],1] from the input list (c(1,2),c(2,3),c(1,2))  
list2m(list(c(1,2),c(2,3),c(1,2))) 

}


% R KEYWORDS
\keyword{multivariate}
\keyword{univar}
\keyword{list}
\keyword{symbolmath}
\keyword{combinatorics}



