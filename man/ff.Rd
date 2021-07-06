\name{ff}
\alias{ff}
\title{Falling factorial}
\description{The function computes the descending (falling) factorial of a positive integer \code{n} 
with respect to a positive integer \code{k} less or equal to \code{n}.}

\usage{
ff( n=1, k ) 
}

\arguments{

\item{n}{integer}
\item{k}{integer}
}

\details{Run \code{ff(n, k)} to get \code{n(n-1)(n-2).....(n-k+1)}.
}

\value{
 \item{integer}{the descending factorial}
}

\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}
}


\seealso{
\code{\link{mkmSet}},
\code{\link{countP}},
\code{\link{nStirling2}},
\code{\link{intPart}},
\code{\link{mCoeff}}
}

\examples{
# Return 6*5*4 = 120
ff(6,3) 
}

% R KEYWORDS
\keyword{multivariate}
\keyword{univar}
\keyword{list}
\keyword{symbolmath}


