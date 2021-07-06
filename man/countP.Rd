\name{countP}
\alias{countP}
\title{Multiplicity of a multi-index partition.}
\description{The function computes the multiplicity of a multi-index partition. Note that a
multi-index partition corresponds to a subdivision of a multiset having the input multi-index 
as multiplicities.}

\usage{
countP( v=c(1) )
}

\arguments{
\item{v}{vector or list}
}

\details{Consider the partitions of an integer, let's say 3, that are \code{[1,1,1],[1,2],[3]} such that
\code{1+1+1=1+2=3}. Consider the partitions of a set with cardinality 3, let's say \code{[a1, a2, a3]}
\tabular{ll}{\code{(I)} \tab   \code{[[a1], [a2], [a3]], [[a1],[a2,a3]], [[a2],[a1,a3]], [[a3],[a1,a2]], [[a1,a2,a3]]}.}
The multiplicity of a partition of 3 is the number of partitions of \code{[a1, a2, a3]} in blocks having that
partition as cardinalities. In the example, we have 
\tabular{cl}{
 a) \tab 1 partition in 3 blocks of cardinalities 1, that is 
\code{[[a1], [a2], [a3]]} corresponding to \code{[1,1,1]} \cr 
b) \tab 1 partition in 1 block of cardinalities 3, that is 
\code{[[a1,a2,a3]]} corresponding to \code{[3]} \cr
c) \tab 3 partitions in 2 blocks of cardinalities 1 and 2 respectively, that is\cr
   \tab \code{[[a1],[a2,a3]], [[a2],[a1,a3]], [[a3],[a1,a2]]}.}
So running \code{countP(c(1,2))} we get 3, running \code{countP(c(1,1,1))} or \code{countP(c(3))} we get 1. \cr \cr
The same device can be used to find the multiplicity of a subdivision. The subdivisions of a multiset are 
obtained as follows: assume all distinct the elements of the multiset, determine all the corresponding set partitions 
and then replace each element in each block with the original one. For example, consider the multiset \code{[a, a, a]}
having the integer 3 as multiplicity. Assuming all distinct the elements of the multiset, the partitions of \code{[a1, a2, a3]}
are given in \code{(I)}. Now replace \code{a1<-a, a2<-a, a3<-a} and get 
\tabular{l}{\code{[[a], [a], [a]], [[a], [a,a]], [[a], [a,a]], [[a], [a,a]], [[a,a,a]]}}.
Note that the partitions 
\tabular{l}{\code{[[a1,a2], [a3]], [[a1,a3], [a2]], [[a2,a3], [a1]]}}
give rise to the same subdivision \code{[[a,a], [a]]}. Then the multiplicity of \code{[[a,a], [a]]} is 3. Therefore 
the subdivisions of \code{[a, a, a,]}  are 
\tabular{l}{\code{[[a], [a], [a]], [[a], [a,a]], [[a,a,a]]}.}
The multiplicity of the subdivision \code{[[a], [a,a]]} is 3, as for the partition \code{[1,2]} of the integer 3. 
The multiplicity of the subdivisions \code{[[a], [a], [a]]} and \code{[[a,a,a]]} is 1, as for the partitions 
\code{[1,1,1]} and \code{[3]} of the integer 3 respectively. Thus running \code{countP(c(1,2))} we get 3, that is 
\cr
\tabular{cl}{
a) \tab the multiplicity of the subdivision \code{[[a,a], [a]]} \cr
b) \tab the number of partitions of the set \code{[a1, a2, a3]} in two blocks, one of cardinality 1 and the other \cr 
   \tab of cardinality 2 \cr
c) \tab the number of partitions of the set \code{[a1, a2, a3]} corresponding to the partition \code{[1,2]} of \cr
   \tab the integer 3. 
}
Now consider the partition of a multi-index, let's say (4,2). One of the partitions of (4,2) is the matrix 
(see the output of the \code{\link{mkmSet}} function) \cr  
\verb{        
               0  1  1  2
(II)          1  0  0  1}
\cr \cr
as \code{1+1+2=4} and \code{1+1=2}. The partition \code{(II)} corresponds to the subdivision \code{[[b], [a], [a], [a,a,b]]} 
of the multiset \code{[a,a,a,a,b,b]} having the multi-index (4,2) as multiplicities. Indeed each column \code{(i,j)} gives 
the instances \code{i} and \code{j} of \code{a} and \code{b} in a block of the subdivision. Running \code{countP}
\code{(list(c(0,1), c(1,0), c(1,0), c(2,1)))} we get 12. This multiplicity gives the number of partitions of the set \code{[a1,a2,a3,}
\code{a4,b1,b2]} corresponding to the subdivision \code{[[b], [a], [a], [a,a,b]]} after the 
replacement 
\code{a1<-a, a2<-a,} \code{a3<-a, a4<-a, b1<-b, b2<-b.} \cr \cr
Note that the input of the \code{\link{countP}} function does not necessarily be in lexicographic order. To find all the partitions 
of a multi-index see the \code{\link{mkmSet}} function.}

\value{
 \item{integer}{the multiplicity of the given item}
}

\references{
E. Di Nardo, G. Guarino, D. Senato (2008) An unifying framework for k-statistics, polykays and their generalizations. 
Bernoulli. 14(2), 440-468. (download from \url{https://arxiv.org/pdf/math/0607623.pdf}) 
 }

\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}
}

\note{
Called by the \code{\link{mkmSet}} and \code{\link{nPS}} functions in the \code{kStatistics} package.
}


\seealso{
\code{\link{mkmSet}},
\code{\link{mCoeff}},
\code{\link{nStirling2}},
\code{\link{intPart}},
\code{\link{ff}}
}

\examples{

# Return 3 which is the multiplicity of [1,2], partition of the integer 3, or 
# of [[a],[a,a]], subdivision of the multiset [a,a,a]
countP(c(1,2))
  
# Return 15 which is the multiplicity of [4,2], partition of the integer 6, or
# of [[a,a,a,a],[a,a]], subdivision of the multiset [a,a,a,a,a,a]
countP(c(4,2))
  
# Return 18 which is the multiplicity of 
#  0 0 1 1 2
#  1 2 0 0 0
# partition of the multi-index (4,3), or of [[b],[b,b],[a],[a],[a,a]], subdivision  of 
# the multiset [a,a,a,a,b,b,b]
countP( list(c(2,0), c(1,0), c(1,0), c(0,1),c(0,2)) )
 
}


% R KEYWORDS
\keyword{multivariate}
\keyword{univar}
\keyword{list}
\keyword{symbolmath}
\keyword{combinatorics}