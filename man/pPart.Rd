
\name{pPart}
\alias{pPart}
\title{
Partition polynomial
}
\description{The function generates the partition polynomial of degree n, whose 
coefficients are the number of partitions of n with k parts for k from 1 to n.}
\usage{
pPart(n = 0)
}
\arguments{
  \item{n}{integer }
}
\details{Faa di Bruno's formula gives the coefficients of the exponential formal 
power series obtained from the composition f(g()) of exponential formal power 
series. The partition polynomial F[n] of degree n is obtained using the Faa  
di Bruno's formula obtained from the MFB function in the case "composition of 
univariate f with univariate g" with f[i]=1/i!, g[i]^k=(i!)^k*k!*y for each i
and k from 1 to n. Note the symbolic substitution g[i]^k=(i!)^k*k!*y, where the
power of g[i] appears in its definition. This function is an example of application of 
Faa di Bruno's formula by using the symbolic calculus with two indexes.}

\value{
\item{string}{expression of the partition polynomial of degree n}
}
\references{
E. Di Nardo E., G. Guarino, D. Senato (2011) A new algorithm for computing the multivariate Faa di Bruno's formula. 
Appl. Math. Comp. 217, 6286--6295. (download from \url{http://arxiv.org/abs/1012.6008})
}

\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}
}

\section{Warning}{The value of the first parameter is the same as the MFB function 
in the univariate with univariate case composition.}

\note{
This function calls the \code{\link{MFB}} function in the package \code{kStatistics}.
}


\seealso{
\code{\link{MFB}}
}


\examples{

# Return the partition polynomial F[5]
pPart(5)

# Return the partition polynomial F[11] and its evaluation when y=7  
#
s<-pPart(11)          # run the command
s<-paste0("1",s)      # add the coefficient to the first term (fixed command)
s<-gsub(" y","1y",s)  # replace the variable y without coefficient (fixed command)
s<-gsub("y", "*7",s)  # assignment y = 7
eval(parse(text=s))   # evaluation of the expression (fixed command)
}

% R KEYWORDS
\keyword{ multivariate }
\keyword{ univar }
\keyword{ list }
\keyword{ symbolmath }
\keyword{ combinatorics }