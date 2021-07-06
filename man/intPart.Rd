\name{intPart}
\alias{intPart}
\title{Integer partitions}
\description{The function generates all possible (unique) decomposition of a positive integer \code{n} in the 
sum of positive integers less or equal to \code{n}.}

\usage{
intPart(n=0 ,vOutput = FALSE) 
}

\arguments{

\item{n}{integer}
\item{vOutput}{optional boolean parameter, if equal to \code{TRUE} the function produces a 
compact output that is easy to read.}
}

\details{A partition of an integer \code{n} is a sequence of weakly increasing integers such 
that their sum returns \code{n}. The \code{\link{intPart}} function generates all the partitions of a given integer 
in increasing order.}

\value{
 \item{list}{all the partitions of \code{n}}
}

\references{
A. Nijenhuis, H. Wilf. (1978) Combinatorial Algorithms for Computers and Calculators. 
Academic Press, Orlando FL, II edition.
}

\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}
}

\note{
Called by the \code{\link{mkmSet}} function in the \code{kStatistics} package.
}

\seealso{
\code{\link{mkmSet}},
\code{\link{mCoeff}},
\code{\link{nStirling2}},
\code{\link{countP}},
\code{\link{ff}}
}

\examples{
# Return the partition of the integer 3, that is 
# [1,1,1],[1,2],[3]
intPart(3) 

# Return the partition of the integer 4, that is 
# [1,1,1,1],[1,1,2],[1,3],[2,2],[4]
intPart(4) 
# OR (same output) 
intPart(4, FALSE) 

# Return the same output as the previous example but in a compact expression
intPart(4, TRUE)

}


% R KEYWORDS
\keyword{multivariate}
\keyword{univar}
\keyword{list}
\keyword{symbolmath}
\keyword{combinatorics}


