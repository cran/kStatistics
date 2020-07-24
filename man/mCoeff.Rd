\name{mCoeff}
\alias{mCoeff}
\title{Multiplicity in a multiset}
\description{Return the instances of an element in a multiset.}

\usage{
mCoeff( v, L ) 
}

\arguments{

\item{v}{vector to be searched in a list}
\item{L}{list of two-dimensional vectors: in the first there is a vector and in the second its instances}
}

\details{This function is useful in the construction of kStatistics. \cr\cr
Compute how many times an element is in a multiset, that is its multiplicity.}


\value{
 \item{float}{multiplicity of the researched item}
}

\references{
E. Di Nardo, G. Guarino, D. Senato (2008) An unifying framework for k-statistics, polykays and their generalizations. 
Bernoulli. 14(2), 440-468. (download from \url{http://arxiv.org/pdf/math/0607623.pdf}) 
 
}

\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}

}

\note{
Called by the functions \code{\link{nPS}}, \code{\link{nKM}}, \code{\link{nPM}} in the package \code{kStatistics}.
}


\seealso{
\code{\link{mkmSet}},
\code{\link{umSet}},
\code{\link{countP}},
\code{\link{nStirling2}},
\code{\link{intPart}},
\code{\link{df}}
}

\examples{

# Return [[1,1,1],1], [[1,2],3], [[3],1]
L1 <- mkmSet(c(3))   

# Return 3 that is the istance of [1,2] in L1
mCoeff( c(1,2), L1)  
 
}


% R KEYWORDS
\keyword{multivariate}
\keyword{univar}
\keyword{list}
\keyword{symbolmath}

