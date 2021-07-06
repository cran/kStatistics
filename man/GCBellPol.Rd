\name{GCBellPol}
\alias{GCBellPol}
\title{Generalized Complete Bell Polynomial}
\description{The function generates a generalized complete Bell polynomial, that is a coefficient 
of the composition \code{exp(y[1] g1(z1,...,zm) + ... + y[n] gn(z1,...,zm))}, where \code{y[1],...,y[n]} are 
variables. The input vector of integers identifies the subscript of the polynomial.}
\usage{
GCBellPol(nv = c(), m = 1, b = FALSE)
}
\arguments{
  \item{nv}{vector of integers, the subscript of the polynomial, corresponding to the powers of the product
among \code{z1, z2, ..., zm}}
  \item{m}{integer, the number of \code{z}'s variables}
  \item{b}{boolean, \code{TRUE} if the inner formal power series \code{"g"} are all equal}
}

\details{The multivariate Faa di Bruno's formula, output of the \code{\link{MFB}} function, gives 
a coefficient of the multivariate exponential power series obtained from the composition of 
the multivariate exponential power series \code{f(x1,...,xn)} with \code{xi=gi(z1,...,zm)} for each 
\code{i} from \code{1} to \code{n}. Now, set \code{f(y[1],...,y[n];x1,...,xn)=exp(y[1] x1 + ... + y[n] xn)}.
In such a case, the coefficients are the generalized complete Bell polynomials, 
see the referred papers. In particular, the \code{\link{GCBellPol}} function  gives 
the expression of these polynomials when \code{n=1} or when \code{n>1} and \code{g1=...=gn=g} or when 
\code{n>1} and  \code{g1, ..., gn} are all different. See the \code{\link{e_GCBellPol}} function for 
evaluating this polynomial when its variables \code{y[1], ..., y[n]} or/and its coefficients are substituted
with numerical values.}

\value{
\item{string}{the expression of the polynomial}}

\references{
G. M. Constantine, T. H. Savits  (1996) A Multivariate Faa Di Bruno Formula With 
Applications. Trans. Amer. Math. Soc. 348(2), 503--520.

E. Di Nardo (2016) On multivariable cumulant polynomial sequence with applications. Jour. 
Algebraic Statistics 7(1), 72-89.  (download from \url{https://arxiv.org/abs/1606.01004})

E. Di Nardo, G. Guarino, D. Senato (2011) A new algorithm for computing the multivariate Faa di Bruno's formula. 
Appl. Math. Comp. 217, 6286--6295. (download from \url{https://arxiv.org/abs/1012.6008})

E. Di Nardo, M. Marena, P. Semeraro (2020) On non-linear dependence of multivariate subordinated Levy processes.
In press Stat. Prob. Letters (download from \url{https://arxiv.org/abs/2004.03933}) 
 
}

\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}
}

\section{Warning}{The value of the first parameter is the same as the \code{\link{mkmSet}} 
function.}

\note{
This function calls the \code{\link{MFB}} function in the \code{kStatistics} package.
}

\seealso{
\code{\link{mkmSet}},
\code{\link{MFB}},
\code{\link{e_GCBellPol}}
}

\examples{

# Return the generalized complete Bell Polynomial for n=1, m=1 and g1=g, 
# that is (y^2)g[1]^2 + (y)g[2]
#
GCBellPol( c(2),1 )

# Return the generalized complete Bell Polynomial for n=1, m=2 and g1=g, 
# 2(y^2)g[1,0]g[1,1] + (y^3)g[0,1]g[1,0]^2 + (y)g[2,1] + (y^2)g[0,1]g[2,0]
#
GCBellPol( c(2,1),1 )

# Return the generalized complete Bell Polynomial for n=2, m=2 and g1=g2=g, 
# (y1)g[1,1] + (y1^2)g[0,1]g[1,0] + (y2)g[1,1] + (y2^2)g[0,1]g[1,0] + 2(y1)(y2)g[0,1]g[1,0]
#
GCBellPol( c(1,1),2, TRUE )

# Return the generalized complete Bell Polynomial for n=2, m=2 and g1 different from g2, 
# that is (y1)g1[1,1] + (y1^2)g1[1,0]g1[0,1] + (y2)g2[1,1] + (y2^2)g2[1,0]g2[0,1] + 
# (y1)(y2)g1[1,0]g2[0,1] + (y1)(y2)g1[0,1]g2[1,0]
#
GCBellPol( c(1,1),2 )

}


% R KEYWORDS
\keyword{multivariate}
\keyword{univar} 
\keyword{list}
\keyword{symbolmath}
\keyword{combinatorics}