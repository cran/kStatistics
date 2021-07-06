\name{mkT}      
\alias{mkT}

\title{Scomposition of a multi-index}

\description{Given a multi-index, that is a vector of non-negative integers and a positive integer \code{n}, the function returns all 
the lists \code{(v1,...,vn)} of non-negative integer vectors, with the same lenght of the multi-index and such that \code{v=v1+...+vn}.}

\usage{
mkT(v = c(), n = 0, vOutput = FALSE)
}

\arguments{
  \item{v}{vector of integers}
  \item{n}{integer, number of addends}
  \item{vOutput}{optional boolean variable. If equal to \code{TRUE}, the function produces a compact output that is easy to read.}
}

\details{From the input vector \code{v} of non-negative integers, which represents the multi-index, 
the function produces all the 
lists of \code{n} vectors \code{(v1,...,vn)} of non-negative integers, including the zero vector, having the same lenght of 
\code{v} and such that their sum gives \code{v}. Note that two lists are different if they contain the same vectors but permuted.}

\value{\item{list}{the list of \code{n} vectors \code{(v1,...,vn)}}}

\references{
Di Nardo E., Guarino G., Senato D. (2011) A new algorithm for computing the multivariate Faa di Bruno's formula. 
Appl. Math. Comp. 217, 6286-6295. (download from \url{https://arxiv.org/abs/1012.6008})
}

\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}
}

\section{Warning}{The vector in the first variable must be not empty and must contain all non-negative integers.
The second parameter must be a positive integer.}

\note{
Called by the \code{\link{MFB}} function in the \code{kStatistics} package.
The routine uses the \code{\link{mkmSet}} function in the same package.
}



\seealso{
\code{\link{mkmSet}},
\code{\link{MFB}}
}

\examples{

# Return the scompositions of the vector (1,1) in 2 vectors of 2 non-negative integers 
# such that their sum is (1,1), that is 
# ([1,1],[0,0]) - ([0,0],[1,1]) - ([1,0],[0,1]) - ([0,1],[1,0])
mkT(c(1,1),2)  
# OR (same output) 
mkT(c(1,1),2,FALSE)

# Return the scompositions of the vector (1,0,1) in 2 vectors of 3 non-negative integers 
# such that their sum gives (1,0,1), that is 
# ([1,0,1],[0,0,0]) - ([0,0,0],[1,0,1]) - ([1,0,0],[0,0,1]) - ([0,0,1],[1,0,0]).
# Note that the second value in each resulting vector is always zero.
mkT(c(1,0,1),2) 
# OR (same output) 
mkT(c(1,0,1),2, FALSE) 

# Return the same output of the previous example but in a compact form.
mkT(c(1,0,1),2, TRUE) 

# Return the scompositions of the vector (1,1,1) in 3 vectors of 3 non-negative integers 
# such that  their sum gives (1,1,1). The result is given in a compact form.
for (m in mkT(c(1,1,1),3)) {for (n in m) cat(n," - "); cat("\n")}

}


% R KEYWORDS
\keyword{multivariate}
\keyword{univar}
\keyword{list}
\keyword{symbolmath}
\keyword{combinatorics}

