\name{m2Set}
\alias{m2Set}
\title{Multiset To Set}
\description{Transform a multiset in a set.}

\usage{
m2Set( v ) 
}

\arguments{

\item{v}{multiset structure of type [[e1,e2,...], m1], [[f1,f2,...], m2],... with m1, m2 multiplicities}
}

\details{Consider the multiset [a,a,b]. The subdivisions are [[[a,b],[a]],2],[[[a],[a],[b]],1], [[a,a,b],1], [[a,a],[b],1].
m2Set(c(2,1)) deletes block repetitions, that is transforms the given list in [a,b],[a],[b],[a,a,b],[a,a], 
according to the order given in the input. See also the examples.}

\value{
 \item{set}{sequence of distinct elements}
}

\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}
}

\note{
Called by the functions \code{\link{nKM}}, \code{\link{nPM}}  in the package \code{kStatistics}.
}


\seealso{
\code{\link{list2m}},
\code{\link{list2Set}}
}

\examples{

M1 <- mkmSet(c(2,1))
# 
# M1 is the multiset of subdivisions
# [ 
#   [[1,1],[1,0]], 2], 
#   [[1,0],[1,0],[0,1]], 1),
#   [[2,1]], 1], 
#   [[2,0],[0,1]], 1]
# ]
# 

# Return the following set:  [[1,1],[1,0],[0,1],[2,1],[2,0]] 
m2Set( M1 )

}


% R KEYWORDS
\keyword{multivariate}
\keyword{univar}
\keyword{list}
\keyword{symbolmath}

