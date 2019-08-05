\name{list2Set}
\alias{list2Set}
\title{List To Set}
\description{Given a list, delete the instances of an element in the list, leaving the order inalterated.}

\usage{
list2Set(v) 
}

\arguments{

\item{v}{single array or list of arrays}
}

\value{
 \item{set}{sequence of distinct elements given in v}
}

\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}
}

\note{
Called by the function \code{\link{list2m}} in the package \code{kStatistics}
}

\seealso{
\code{\link{list2m}},
\code{\link{m2Set}}
}

\examples{
list2Set(c(1,2,3,1,2,5,6)) 
#returns c(1,2,3,5,6) 

list2Set( list(c(1,2),c(1,2),c(10,11),c(1,2),c(7,8))) 
#returns list(c(1,2),c(10,11),c(7,8))
}


% R KEYWORDS
\keyword{ multivariate }
\keyword{ univar }
\keyword{ list }
\keyword{ symbolmath }
