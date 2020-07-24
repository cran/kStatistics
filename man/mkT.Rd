\name{mkT}      
\alias{mkT}

\title{Scomposition of a vector of non-negative integers}

\description{Given a vector v of non-negative integers, the routine finds all the lists (v1, ..., vn) of non-negative integer 
vectors, with the same lenght of v and such that v=v1+...+vn.}

\usage{
mkT(v = c(), n = 0)
}

\arguments{
  \item{v}{vector of integers}
  \item{n}{integer, number of addends}
}

\details{From an input vector v of non-negative integers, the routine generates all the lists of n vectors (v1, ..., vn) of 
non-negative integers, including the zero vector, having the same lenght of v and such that their sum gives v. 
Note that two lists are different if they contain the same vectors but permutated.}

\value{\item{list}{the list of n vectors (v1, ..., vn)}}

\references{
Di Nardo E., Guarino G., Senato D. (2011) A new algorithm for computing the multivariate Faa di Bruno's formula. 
Appl. Math. Comp. 217, 6286--6295. (download from \url{http://arxiv.org/abs/1012.6008})
}

\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}
}

\section{Warning}{The vector of the first parameter must be not empty and must contain all non-negative values.
The second parameter must be a positive integer.}

\note{
Called by the function \code{\link{MFB}} in the package \code{kStatistics}.
It uses the \code{\link{mkmSet}} function in the same package.
}



\seealso{
\code{\link{mkmSet}},
\code{\link{MFB}}
}

\examples{

# Return the scompositions of the vector (1,1) in 2 vectors of 2 non-negative integers such that 
# their sum is (1,1), that is
# ([1,1],[0,0]) - ([0,0],[1,1]) - ([1,0],[0,1]) - ([0,1],[1,0])
mkT(c(1,1),2)

# Return the scompositions of the vector (1,0,1) in 2 vectors of 3 non-negative integers such that 
# their sum gives (1,0,1), that is
# ([1,0,1],[0,0,0]) - ([0,0,0],[1,0,1]) - ([1,0,0],[0,0,1]) - ([0,0,1],[1,0,0]).
# Note that the second value in each resulting vector is always zero.
mkT(c(1,0,1),2)

# Return the scompositions of the vector (1,1,1) in 3 vectors of 3 non-negative integers such that 
# their sum gives (1,1,1). The result is in a compact form expression.
for (m in mkT(c(1,1,1),3)) {for (n in m) cat(n," - "); cat("\n"); }

}


% R KEYWORDS
\keyword{multivariate}
\keyword{univar}
\keyword{list}
\keyword{symbolmath}
\keyword{combinatorics}

