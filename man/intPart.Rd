\name{intPart}
\alias{intPart}
\title{Integer partitions}
\description{Generates all possible (unique) ways to represent a positive integer n as sum of positive integers.}

\usage{
intPart(n) 
}

\arguments{

\item{n}{integer}
}

\details{A partition of an integer n is a sequence of weakly increasing integers such that their sum returns n. 
The function generates all the partitions of a given integer in sorted order and numbers within a partition in sorted order too.}

\value{
 \item{list}{all partitions of n}
}

\references{
Nijenhuis A. and Wilf. H., Combinatorial Algorithms for Computers and Calculators. Academic Press, Orlando FL, II edition,
1978.
}

\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}
}

\note{
Called by the function \code{\link{mkmSet}} in the package \code{kStatistics}
}

\seealso{
\code{\link{mkmSet}},
\code{\link{umSet}},
\code{\link{mCoeff}},
\code{\link{nStirling2}},
\code{\link{countP}},
\code{\link{df}}
}

\examples{
intPart(3) 
# returns the partitions [1,1,1],[1,2],[3] 

intPart(4) 
# returns the partitions [1,1,1,1],[1,3],[2,2],[4]
}


% R KEYWORDS
\keyword{ multivariate }
\keyword{ univar }
\keyword{ list }
\keyword{ symbolmath }


