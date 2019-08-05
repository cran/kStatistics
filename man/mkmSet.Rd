\name{mkmSet}
\alias{mkmSet}
\title{Subdivisions of a Multiset}
\description{Generate all the subdivisions of a multiset, reducing the overall computational complexity 
using integer partitions}

\usage{
      mkmSet(vPar)
}

\arguments{

\item{vPar}{Number of blocks to consider or array of numbers of blocks to consider}

}

\value{
 \item{list }{two-dimensional list: in the first there is the partition type, while in the second there is 
its multiplicity}}

\references{
Di Nardo E., G. Guarino and D. Senato (2008) An unifying framework for k-statistics, polykays and their generalizations. 
Bernoulli. Vol. 14(2), 440-468. Official Journal of the Bernoulli Society for Mathematical Statistics and Probability, 
\url{http://isi.cbs.nl/bernoulli/}, (download from \url{http://www.elviradinardo.it/lavori1.html}) 
}

\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}

}

\note{
Called by the functions \code{\link{nKS}}, \code{\link{nKM}}, \code{\link{nPS}}, \code{\link{nPM}} in 
the package \code{kStatistics}
}

\note{This is the most important function for calculating kStatistics and Polykays.\cr \cr
The subdivisions of a multiset are obtained as follows: assume all distinct the elements of the multiset, 
determine all set partitions and then replace each element in each block with the original one. For example,
the partitions of the set [a1, a2, a3] are [[a1], [a2], [a3]], [[a1,a2], [a3]], [[a1,a3], [a2]], [[a2,a3], [a1]], 
[[a1,a2,a3]]. Replace a1<-a, a2<-a, a3<-a. The label does not matter. The subdivisions are [[a], [a], [a]], [[a,a], [a]], [[a,a], [a]], 
[[a,a], [a]], [[a,a,a]]. A short notation for the output is [[1,1,1],1], [[1,2],3], [[3],1], with
the multiplicity of each block. Note that [1,1,1], [1,2], [3] are the partitions of the integer 3. \cr\cr
In the list, the sum of all multiplicities is the Bell Number. Moreover the sum of multiplicities for 
the subdivisions having the same block numbers is the Stirling number of the second kind. 
For example, \emph{mkmSet(4)} returns [[1,1,1,1],1], [[1,1,2],6] ,[[1,3],4], [[2,2],3], [[4],1], where in the second 
position there is the multiplicity of the subdivision indicated in the first position. 
Observe that 3 + 4 = 7 =  S(4,2), where 4 is the input integer, 2 is the length of the blocks [1,3] and [2,2] and
S(i,j) denotes the Stirling numbers of the second kind, see function \code{\link{nStirling2}}. In the same way 1 =  S(4,4) =  S(4,1) and 6 =  S(4,3). Note that 7 + 1 + 1 + 6 = 15 = Bell(4) which is the number of 
partitions of 4.\cr\cr
When in the multiset there are two or more different elements, the resulting list is different. For example, for the 
multiset [a,a,b], the calling is \emph{mkmSet(c(2,1))}. In the output list, [[1,1],[1,0],2] denotes
the subdivision [[a,b],[a]] with multiplicities 2. 

}


\seealso{
\code{\link{umSet}},
\code{\link{mCoeff}},
\code{\link{countP}},
\code{\link{nStirling2}},
\code{\link{intPart}},
\code{\link{df}}
}

\examples{
mkmSet(3)
# generates [ [[1,1,1],1], [[1,2],3], [[3],1]  ]
# The support of the muliset has all the same eleents

mkmSet(c(2,1)) 
# generates [ [[1,1],[1,0],2], [[1,0],[1,0],[0,1],1],[[2,0],[0,1],1], [[2,1],1] ]
# The support of the multiset has 2 elements with instances respectively 2 and 1.

}


% R KEYWORDS
\keyword{ multivariate }
\keyword{ univar }
\keyword{ list }
\keyword{ symbolmath }
