\name{umSet}
\alias{umSet}
\title{Disjoint union of multisets}
\description{The function returns the disjoint union of two or more multisets, by adding the multiplicity of equal elements.}

\usage{
umSet( pM )
}

\arguments{

\item{pM}{list of two or more multisets}
}

\details{
Given the two multisets [a,a,a,b] and [a,b,b,c], the function returns [a,a,a,a,b,b,b,c]. Given the subdivisions
[[a,b],[a]] with multiplicity 3 and [[a,b],[a]] with multiplicity 5, the function returns [[a,b],[a]] with
multiplicity 8. See also examples. 
}

\value{
 \item{list}{the multiset union}
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
Called by the function \code{\link{nPM}} in the package \code{kStatistics}.
}



\seealso{
\code{\link{mkmSet}},
\code{\link{mCoeff}},
\code{\link{countP}},
\code{\link{nStirling2}},
\code{\link{intPart}},
\code{\link{df}}
}

\examples{

# Return the list( list( c(1,1), c(1,0) ), c(8) )
M1 <- list( list( c(1,1), c(1,0) ), c(3) ) 
M2 <- list( list( c(1,1), c(1,0) ), c(5) ) 
umSet(list(M1,M2)) 

}


% R KEYWORDS
\keyword{ multivariate }
\keyword{ univar }
\keyword{ list }
\keyword{ symbolmath }

