\name{list2Set}
\alias{list2Set}
\title{List To Set}
\description{Given a list, the function deletes the instances of an element in the list, leaving the order inalterated.}

\usage{
list2Set(v=c(0)) 
}

\arguments{

\item{v}{single vector or list of vectors}
}

\value{
 \item{set}{the sequence of distinct elements}
}

\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}
}

\note{
Called by the \code{\link{list2m}} function in the \code{kStatistics} package.
}

\seealso{
\code{\link{list2m}},
\code{\link{m2Set}}
}

\examples{

# Return the vector c(1,2,3,5,6)
list2Set(c(1,2,3,1,2,5,6)) 
 
# Return the list (c(1,2),c(10,11),c(7,8))
list2Set(list(c(1,2),c(1,2),c(10,11),c(1,2),c(7,8))) 

}


% R KEYWORDS
\keyword{multivariate}
\keyword{univar}
\keyword{list}
\keyword{symbolmath}
