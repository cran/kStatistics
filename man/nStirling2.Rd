\name{nStirling2}
\alias{nStirling2}
\title{Stirling number of second kind}
\description{The function computes the Stirling number of the second kind.}


\usage{
nStirling2( n, k ) 
}

\arguments{

\item{n}{integer}
\item{k}{integer less or equal to \code{n}}
}

\details{The Stirling number of the second kind \code{S(n,k)} is equal to the number of ways to split a 
set of cardinality \code{n} into \code{k} nonempty subsets. For example, if the set is \code{[a,b,c,d]}, then
the partitions in 2 blocks are: \code{[[a], [bcd]], [[b], [acd]], [[c], [abd]], [[d],[abc]]} with 
cardinalities (1,3) and \code{[ab, cd], [ac, bd], [ad, bc]} with cardinalities (2,2). Then \code{S(4,2)} 
is equal to 7. Note that (1,3) and (2,2) are also the partitions of the integer 4 in 2 parts.}

\value{
 \item{integer}{the Stirling number of the second kind}
}

\references{
R. P. Stanley (2012) Enumerative combinatorics. Vol.1. II edition. Cambridge Studies in Advanced Mathematics, 49. 
Cambridge University Press, Cambridge. 
}

\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}
}

\note{
Called by the \code{\link{nKS}} and \code{\link{nKM}} functions in the \code{kStatistics} package.
}

\seealso{
\code{\link{mkmSet}},
\code{\link{mCoeff}},
\code{\link{intPart}},
\code{\link{countP}},
\code{\link{ff}}
}

\examples{
# Return the number of ways to split a set of 6 objects into 2 nonempty subsets
nStirling2(6,2) 

}


% R KEYWORDS
\keyword{ multivariate }
\keyword{ univar }
\keyword{ list }
\keyword{ symbolmath }

