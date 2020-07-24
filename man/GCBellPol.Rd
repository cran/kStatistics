\name{GCBellPol}
\alias{GCBellPol}
\title{Generalized Complete Bell Polynomial}
\description{The function returns generalized complete Bell polynomials, that are coefficients of 
exp(y1 g1(z1,...,zm) + ... + yn gn(z1,...,zm)), where y1,...,yn are the variables. The input vector of 
integers identifies the subscripts of the polynomial coefficients.

}
\usage{
GCBellPol(n = 0, m = 1, b = FALSE)
}
\arguments{
  \item{n}{vector of integers, the subscript of the polynomial, corresponding to the powers of z1, z2, ..., zm}
  \item{m}{integer, the number of z's variables}
  \item{b}{boolean, TRUE if g1=g2=...=gn=g}
}

\details{The multivariate Faa di Bruno's formula output of the MFB function gives the coefficients of the multivariate exponential 
power series obtained from the composition of the multivariate exponential power series f(t1,...,tn) with
ti=gi(z1,...,zm) for each i from 1 to n. Choose f(t1,...,tn)=exp(y1 t1 + ... + yn tn). The coefficients obtained
through multivariate Faa di Bruno's formula are the generalized complete Bell polynomials. In particular, the function 
GCBellPol gives the expression of these polynomials when n=1 or n> 1 with g1=...=gn=g or n>1 with g1, ..., gn all different. 
Among the various applications, we point out the cumulant polynomial sequences. Cumulant polynomials allow us to 
compute moments and cumulants of multivariate Levy processes and subordinated multivariate Levy processes, 
multivariate compound Poisson processes with applications in photocounting,  multivariable Sheffer polynomial sequences, 
see the referred papers. See the function e_GCBellPol for evaluating the generalized complete Bell polynomials by assigning numerical values 
to the variables and their coefficients.}

\value{
\item{string}{expression of the generalized complete Bell polynomial}}

\references{
Constantine G. M., Savits T. H.  (1996) A Multivariate Faa Di Bruno Formula With 
Applications. Trans. Amer. Math. Soc. 348(2), 503--520.

E. Di Nardo E. (2016) On multivariable cumulant polynomial sequence with applications. Jour. 
Algebraic Statistics 7(1), 72-89.  (download from \url{http://arxiv.org/abs/1606.01004})

E. Di Nardo, G. Guarino, D. Senato (2011) A new algorithm for computing the multivariate Faa di Bruno's formula. 
Appl. Math. Comp. 217, 6286--6295. (download from \url{http://arxiv.org/abs/1012.6008})

E. Di Nardo, M. Marena, P. Semeraro (2020) On non-linear dependence of multivariate subordinated Levy processes.
In press Stat. Prob. Letters (download from \url{https://arxiv.org/abs/2004.03933}) 
 
}

\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}
}

\section{Warning}{The value of the first parameter is the same as the mkmSet function, i.e. the number of blocks considered.}

\note{
It calls the function \code{\link{MFB}} in the package \code{kStatistics}.
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
# corresponding to the cumulant polinomial indexed by (2,1), that is 
# 2(y^2)g[1,0]g[1,1] + (y^3)g[0,1]g[1,0]^2 + (y)g[2,1] + (y^2)g[0,1]g[2,0]
#
GCBellPol( c(2,1),1 )


# Return the generalized complete Bell Polynomial for n=2, m=2 and g1=g2=g, 
# corresponding to the cumulant polinomial in y1+y2 indexed by (1,1), that is 
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