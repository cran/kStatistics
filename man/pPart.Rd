
\name{pPart}
\alias{pPart}
\title{Partition polynomials}

\description{The function generates the partition polynomial of degree \code{n}, whose 
coefficients are the number of partitions of \code{n} into \code{k} parts for \code{k} 
from \code{1} to \code{n}.}

\usage{
pPart(n = 0)
}

\arguments{
  \item{n}{integer, the degree of the polynomial }
}

\details{Faa di Bruno's formula gives the coefficients of the exponential formal 
power series obtained from the composition \code{f[g()]} of the exponential formal power 
series \code{f} and \code{g}. The partition polynomial \code{F[n]} of degree \code{n} 
is obtained using the Faa di Bruno's formula, output of the \code{\link{MFB}} function, 
in the case "composition of univariate \code{f} with univariate \code{g}" with \code{f[i]=1/n!, 
g[i]^k=(i!)^k*k!*y^k} for \code{i} and \code{k} from \code{1} to \code{n}. Note the 
symbolic substitution of \code{g[i]}, as the power of \code{g[i]} appears 
in the substitution. This function is an example of application of Faa di Bruno's formula 
and the symbolic calculus with two indexes.}

\value{
\item{string}{the expression of the polynomial}
}
\references{
E. Di Nardo E., G. Guarino, D. Senato (2011) A new algorithm for computing the multivariate Faa di Bruno's formula. 
Appl. Math. Comp. 217, 6286-6295. (download from \url{https://arxiv.org/abs/1012.6008})
}

\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}
}

\section{Warning}{The value of the first parameter is the same as the \code{\link{MFB}} function 
in the univariate with univariate case composition.}

\note{
This function calls the \code{\link{MFB}} function in the \code{kStatistics} package.
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