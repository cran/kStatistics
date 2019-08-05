\name{nStirling2}
\alias{nStirling2}
\title{Stirling number of second kind}
\description{Compute the Stirling number of the second kind.}


\usage{
nStirling2( n, k ) 
}

\arguments{

\item{n}{integer}
\item{k}{integer [ k <= n ]}
}

\details{
The Stirling numbers of the second kind, written S(n,k), count the number of ways to partition a set of 
n  labelled objects into k nonempty unlabelled subsets. For example if the set is [a,b,c,d], the partitions 
in 2 blocks are: [[a], [bcd]], [[b], [acd]], [[c], [abd]], [[d],[abc]] with cardinalities (1,3) and  
[ab, cd], [ac, bd], [ad, bc] with cardinalities (2,2). Then S(4,2) is 7. S(4,2) is also the number of 
set partitions of class the partition of 4 in two parts.}

\value{
 \item{integer}{Stirling number of the second kind}
}

\references{
Stanley, R. P. Enumerative combinatorics. Vol.1. II edition. Cambridge Studies in Advanced Mathematics, 49. 
Cambridge University Press, Cambridge, 2012. 
}

\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}
}

\note{
Called by the functions \code{\link{nKS}}, \code{\link{nKM}} in the package \code{kStatistics}
}

\seealso{
\code{\link{mkmSet}},
\code{\link{umSet}},
\code{\link{mCoeff}},
\code{\link{intPart}},
\code{\link{countP}},
\code{\link{df}}
}

\examples{
nStirling2(6,2) 
#generate 31, the number of ways to partition a set of 6 objects into 2 non-empty subsets
}


% R KEYWORDS
\keyword{ multivariate }
\keyword{ univar }
\keyword{ list }
\keyword{ symbolmath }

