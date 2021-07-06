\name{mkmSet}
\alias{mkmSet}
\title{Partitions of a multi-index}
\description{The function returns  all the partitions of a multi-index, that is a vector of non-negative integers. Note that these partitions correspond 
to the subdivisions of a multiset having the input multi-index as multiplicities.}

\usage{
      mkmSet(vPar = NULL, vOutput = FALSE)
}

\arguments{

\item{vPar}{vector of non-negative integers}
\item{vOutput}{optional boolean variable. If equal to \code{TRUE}, the function produces a compact 
output that is easy to read.}
}

\details{The \code{\link{mkmSet}} function finds all the vectors, different from the zero vector, whose 
sum (in column) is equal to the vector (in column) of nonnegative integers given in input. When the input 
vector is just an integer, let's say \code{n}, the function returns the partitions of \code{n}. 
Each partition is paired with the number of set partitions having that partition as their class. 
For example, if \code{n=3} the output is 
\tabular{c}{\code{[[1,1,1],1], [[1,2],3], [[3],1]},}
where \code{1+1+1=1+2=3}. From this output, the subdivisions of a multiset with multiplicity 3 can be 
recovered. For example, the subdivisions of \code{[a,a,a]} are \code{[[a], [a], [a]]} corresponding to
\code{[1,1,1]}, \code{[[a], [a,a]]} corresponds to \code{[1,2]} and \code{[[a,a,a]]} corresponds to 
\code{[3]}. When the input vector is a multi-index, the function returns all the partitions of the 
multi-index. For example, if the input is (2,1) then the function returns \cr 
\verb{
0 1 1     0 2     1 1     2
1 0 0     1 0     0 1     1}
\cr \cr
with multiplicities 1, 1, 2, 1 respectively, which corresponds to the output (the columns become rows) 
of the \code{\link{mkmSet}} function when the flag variable \code{vOutput} is set equal to \code{TRUE}  
\tabular{l}{
\code{[( 0 1 )( 1 0 )( 1 0 ),  1 ]} \cr
\code{[( 0 1 )( 2 0 ),  1 ]} \cr
\code{[( 1 0 )( 1 1 ),  2 ]} \cr
\code{[( 2 1 ),  1 ]}}
From this output, the subdivisions of a multiset with multiplicity (2,1) can be easily recovered. 
For example the previous partitions correspond to the following subdivisions of the multiset 
\code{[a,a,b]}  
\tabular{l}{
\code{[[b], [a], [a]]} \cr     
\code{[[b], [a,a]]} \cr  
\code{[[a], [a,b]]} \cr 
\code{[[a,a,b]]}}
The \code{\link{mkmSet}} function is the core of the \code{\link{kStatistics}} package. The strategy to find 
all the partitions of a multi-index is described in the refereed papers. To find the multiplicities of the 
multi-index partitions see the \code{\link{countP}} function. }

\value{
 \item{list}{two-dimensional list: in the first there is the partition, while in the second there is 
its multiplicity}}

\references{
E. Di Nardo, G. Guarino, D. Senato (2008) An unifying framework for k-statistics, polykays and their generalizations. 
Bernoulli. 14(2), 440-468. (download from \url{https://arxiv.org/pdf/math/0607623.pdf})

E. Di Nardo, G. Guarino, D. Senato (2008) Symbolic computation of moments of sampling distributions. 
Comp. Stat. Data Analysis. 52(11), 4909-4922. (download from \url{https://arxiv.org/abs/0806.0129}) 
 
}

\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}

}

\note{
Called by the \code{\link{nKS}}, \code{\link{nKM}}, \code{\link{nPS}} and \code{\link{nPM}} functions  in 
the \code{kStatistics} package. \cr \cr 
In the output list, the sum of all multiplicities is the Bell Number whereas the sum of all 
multiplicities of the partitions with the same lenght is the Stirling number of the second kind. 
For example, \code{mkmSet(4, TRUE)} returns  
\tabular{l}{
\code{[( 1 )( 1 )( 1 )( 1 ),  1 ]} \cr
\code{[( 1 )( 1 )( 2 ),  6 ]} \cr
\code{[( 2 )( 2 ),  3 ]} \cr
\code{[( 1 )( 3 ),  4 ]} \cr
\code{[( 4 ),  1 ]}}
Observe that \code{3 + 4 = 7 =  S(4,2)}, where 4 is the input integer, 2 is the length of the 
partitions \code{[1,3]} and \code{[2,2]} and \code{S(i,j)} denotes the Stirling numbers of the second 
kind, see also the \code{\link{nStirling2}} function. Similarly, we have \code{1 =  S(4,4) =  S(4,1)} 
and \code{6 =  S(4,3)}. Note that \code{1 + 6 + 4 + 3 + 1 = 15 = Bell(4)} which is the number of 
partitions of the integer 4.}


\seealso{
\code{\link{mCoeff}},
\code{\link{countP}},
\code{\link{nStirling2}},
\code{\link{intPart}},
\code{\link{ff}}
}

\examples{

# Return [ [[1,1,1],1], [[1,2],3], [[3],1]  ]
# 3 is the multiplicity of a multiset with 3 elements all equal
mkmSet(3)

# Return [ [[1,1],[1,0],2], [[1,0],[1,0],[0,1],1],[[2,0],[0,1],1], [[2,1],1] ]
# (2,1) is the multiplicity of a multiset with 2 equal elements and a third distinct element 
mkmSet(c(2,1)) 
# OR (same output)
mkmSet(c(2,1), FALSE)  

# Returns the same output of the previous example but in a compact form.
mkmSet(c(2,1), TRUE)  

}


% R KEYWORDS
\keyword{ multivariate }
\keyword{ univar }
\keyword{ list }
\keyword{ symbolmath }
