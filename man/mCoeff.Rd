\name{mCoeff}
\alias{mCoeff}
\title{Extraction of a number from a list}
\description{Given a list containing vectors paired with numbers, the function returns 
the number paired with the vector matching the one passed in input.}

\usage{
mCoeff( v=NULL,L=NULL ) 
}

\arguments{

\item{v}{vector to be searched in the list}
\item{L}{two-dimensional list: in the first there is a vector and in the second 
a number}

}

 
\details{The input variable \code{L} of the \code{\link{mCoeff}} function is a list 
containing vectors and numbers. The  input variable \code{v} of the \code{\link{mCoeff}} 
function is one of vectors contained in the list. The function searches the vector 
\code{v} in the list and returns the number which is paired with \code{v} in the list.
This function is useful in the construction of k-statistics but also to manage monomials 
and their coefficients.}

\value{
 \item{float}{the number paired with the input vector}
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
Called by the \code{\link{nPS}}, \code{\link{nKM}} and \code{\link{nPM}} functions 
in the \code{kStatistics} package.
}


\seealso{
\code{\link{mkmSet}},
\code{\link{countP}},
\code{\link{nStirling2}},
\code{\link{intPart}},
\code{\link{ff}}
}

\examples{

# Run mkmSet(c(3)) to get the list L1 = [[1,1,1],1], [[1,2],3], [[3],1]
L1 <- mkmSet(c(3))   

# Return the number 3, which is the number paired with [1,2] in L1
mCoeff( c(1,2), L1)  
 
}


% R KEYWORDS
\keyword{multivariate}
\keyword{univar}
\keyword{list}
\keyword{symbolmath}

