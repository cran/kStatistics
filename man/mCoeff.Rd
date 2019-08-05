\name{mCoeff}
\alias{mCoeff}
\title{Multiplicity in a multiset}
\description{Generates the instances of an element in a multiset.}

\usage{
mCoeff( v, L ) 
}

\arguments{

\item{v}{block array to search}
\item{L}{list of two-dimensional arrays: in the first there is a vector and in the second 
the multiplicative value to be returned}
}

\details{ This function is useful in the construction of kStatistics. \cr\cr
Compute how many times an element is in a multiset, that is its multiplicity.}


\value{
 \item{float}{multiplicity of the researched item}
}

\references{
E. Di Nardo, G. Guarino and D. Senato (2008) An unifying framework for k-statistics, polykays and their generalizations. 
Bernoulli. Vol. 14(2), 440-468.  (download from \url{http://www.elviradinardo.it/lavori1.html}) 

}

\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}

}

\note{
Called by the functions \code{\link{nPS}}, \code{\link{nKM}}, \code{\link{nPM}} in the package \code{kStatistics}
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

L1 <- mkmSet(c(3))   
# generates [[1,1,1],1], [[1,2],3], [[3],1]
mCoeff( c(1,2), L1)  
# returns 3 that is the istance of [1,2] in L1
 
}


% R KEYWORDS
\keyword{ multivariate }
\keyword{ univar }
\keyword{ list }
\keyword{ symbolmath }
