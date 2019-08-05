\name{ff}
\alias{ff}
\title{Falling factorial}
\description{Computes the descending (falling) factorial of a positive integer n 
with respect to a positive integer k less or equal to n.}

\usage{
ff( n, k ) 
}

\arguments{

\item{n}{integer}
\item{k}{integer}
}

\details{
ff(n, k) returns n(n-1)(n-2).....(n-k+1).
}

\value{
 \item{integer}{descending factorial}
}

\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}
}


\seealso{
\code{\link{mkmSet}},
\code{\link{umSet}},
\code{\link{countP}},
\code{\link{nStirling2}},
\code{\link{intPart}},
\code{\link{mCoeff}}
}

\examples{
ff(6,3) 
# returns 6*5*4 = 120
}

% R KEYWORDS
\keyword{ multivariate }
\keyword{ univar }
\keyword{ list }
\keyword{ symbolmath }


