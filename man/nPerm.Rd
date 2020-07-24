\name{nPerm}
\alias{nPerm}
\title{Complete enumeration of all possible permutations 
}
\description{
The function returns all possible different permutations of objects in a list or in a vector.
}
\usage{
nPerm(L = c())
}
\arguments{
  \item{L}{List/Vector}
}
\details{
In order to manage permutations of numbers or vectors, the standard permutation process is applied.
}

\value{\item{list}{all permutations of L}}

\references{Charalambides C. A.  (2002) Enumerative Combinatoris, Chapman & Haii/CRC. 
}

\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}
}


\examples{

# permutations of 1,2,3  
nPerm( c(1,2,3) );

# permutations of 1,2,1 (two elements are equal) 
nPerm( c(1,2,1) );

# permutations of the words "Alice", "Box","Jack"
nPerm( c("Alice", "Box","Jack") );

# permutations of the vectors c(0,1), c(2,3), c(7,3)
nPerm( list(c(0,1), c(2,3), c(7,3)) );
}


% R KEYWORDS
\keyword{ multivariate }
