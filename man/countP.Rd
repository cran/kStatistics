\name{countP}
\alias{countP}
\title{Multiplicity of a subdivision}
\description{Computes the multiplicity of a subdivision of a multiset.}

\usage{
countP( v )
}

\arguments{
\item{v}{array or list}
}

\details{
The subdivisions of a multiset are obtained as follows: assume all distinct the elements of the multiset, 
determine all set partitions and then replace each element in each block with the original one. For example,
the partitions of the set [a1, a2, a3] are [[a1], [a2], [a3]], [[a1,a2], [a3]], [[a1,a3], [a2]], [[a2,a3], [a1]], 
[[a1,a2,a3]]. Replace a1<-a, a2<-a, a3<-a. The label does not matter.  The subdivisions are [[a], [a], [a]], [[a,a], [a]], [[a,a], [a]], 
[[a,a], [a]], [[a,a,a]]. A short notation for the output is [[1,1,1],1], [[1,2],3], [[3],1].
The function countP( c(1,2) ) returns 3, that is the multiplicity of the subdivision [[a,a], [a]].
If in the multiset, there is more than one element, we pass a list. For example, for the multiset
[a,a,a,a,b,b], the multiplicity of the subdivision [[a,a,b], [a], [a], [b]] is obtained by calling
countP( list(c(2,1), c(1,0), c(1,0), c(0,1)) ), where c(i,j) denotes a block with instances i and j for a and b 
respectively. 
}

\value{
\item{positive integer}{multiplicity of researched item}
}

\references{
E. Di Nardo, G. Guarino and D. Senato (2008) An unifying framework for k-statistics, polykays and their generalizations. 
Bernoulli. Vol. 14(2), 440-468. (download from \url{http://www.elviradinardo.it/lavori1.html}) 
}

\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}
}

\note{
Called by the function \code{\link{mkmSet}}, \code{\link{nPS}} in the package \code{kStatistics}
}


\seealso{
\code{\link{mkmSet}},
\code{\link{umSet}},
\code{\link{mCoeff}},
\code{\link{nStirling2}},
\code{\link{intPart}},
\code{\link{df}}
}

\examples{

countP(c(1,2))
# returns the multiplicity of [[a],[aa]]  

countP(c(4,2))
# returns the multiplicity of [[a,a,a,a],[a,a]]  

countP( list(c(2,0), c(1,0), c(1,0), c(0,1),c(0,2)) )
# returns the multiplicity of [[aa],[a],[a],[b],[bb]] 

}


% R KEYWORDS
\keyword{multivariate}
\keyword{univar}
\keyword{list}
\keyword{symbolmath}