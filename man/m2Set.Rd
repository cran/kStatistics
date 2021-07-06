\name{m2Set}
\alias{m2Set}
\title{Detecting equal columns in multi-index partitions}
\description{The function returns the vectors (only counted once) of all the multi-index 
partitions output of the \code{\link{mkmSet}} function. These vectors correspond also to the blocks 
of the subdivisions of the multiset having the given multi-index as multeplicites.}

\usage{
m2Set( v=c(0) ) 
}

\arguments{

\item{v}{sequence of type \code{[[e1,e2,...], m1], [[f1,f2,...], m2],...} with 
\code{m1, m2,...} multiplicities}
}

\details{Consider the multi-index (2,1). The partitions are\cr  
\verb{
0 1 1      0 2      1 1     2
1 0 0      1 0      0 1     1}
\cr \cr
with multiplicities 1, 1, 2, 1 respectively. The \code{\link{m2Set}} function 
deletes column repetitions, that is transforms the given list in 
\code{[[0,1],[1,0],[2,0],[1,1],[2,1]]} 
according to the order given in the input. In terms of subdivisions, suppose to consider 
the multiset \code{[a,a,b]} with multiplicities (2,1). The subdivisions are  
\tabular{c}{\code{[[[b],[a],[a]],1], [[[a,a],[b]],1], [[[a],[a,b]],2], [[a,a,b],1].}}
The \code{\link{m2Set}} function deletes block repetitions, that is transforms the given list 
in 
\tabular{c}{\code{[[b],[a],[a,a],[a,b],[a,a,b]]}}
according to the order given in the input. See also the examples.}

\value{
 \item{set}{the sequence with distinct elements}
}

\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}
}

\note{
Called by the \code{\link{nKM}} and \code{\link{nPM}} functions in the \code{kStatistics} package.
}


\seealso{
\code{\link{list2m}},
\code{\link{list2Set}}
}

\examples{

M1 <- mkmSet(c(2,1))
# M1 is  
#  list(   
#       list( list(  c(0,1), c(1,0), c(1,0) )  ,1),
#       list( list(  c(0,1), c(2,0)         )  ,1), 
#       list( list(  c(1,0), c(1,1)         )  ,2),            
#       list( list(  c(2,1)                 )  ,1),    
#      )
# To print all the partitions of the multi-index (2,1) run mkmSet(c(2,1),TRUE)
#   [( 0 1 )( 1 0 )( 1 0 ),  1 ]
#   [( 0 1 )( 2 0 ),  1 ]
#   [( 1 0 )( 1 1 ),  2 ] 
#   [( 2 1 ),  1 ]
#
# Then m2Set(M1) returns the following set:  [[0,1],[1,0],[2,0],[1,1],[2,1]]
# 
m2Set( M1 )

}


% R KEYWORDS
\keyword{multivariate}
\keyword{univar}
\keyword{list}
\keyword{symbolmath}

