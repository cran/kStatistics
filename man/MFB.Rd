\name{MFB}
\alias{MFB}

\title{Multivariate Faa di Bruno's formula}
\description{The function returns the coefficient indexed by the integers \code{i1,i2,...} of an exponential 
formal power series composition through the univariate or multivariate Faa di Bruno's formula.}

\usage{
MFB(v = c(), n = 0)
}

\arguments{
  \item{v}{vector of integers, the subscript of the coefficient}
  \item{n}{integer, the number of inner functions \code{g}'s}
}

\details{The \code{\link{MFB}} function computes a coefficient of an exponential formal power series composition: 
\tabular{ll}{a) \tab univariate \code{f} with univariate \code{g}, that is \code{f[g(z)]},  \cr
             b) \tab univariate \code{f} with multivariate \code{g}, that is \code{f[g(z1,z2,...,zm)]},  \cr
             c) \tab multivariate \code{f} with multivariate \code{g}'s, that is \code{f[g1(z1,z2,...,zm),...,gn(z1,z2,...,gm)].}
}
If \code{i1} is the power of \code{z1}, \code{i2} is the power of \code{z2} and so on up to \code{im} power of \code{zm}, then 
\code{(i1,i2,....im)} is the subscript of the output coefficient corresponding to the product \code{z1^i1 z2^i2 ....zm^im.} 
Note that this coefficient gives also the (partial) derivative of order \code{(i1,i2,...,im)} of the composition of 
the multivariate functions \code{f} and \code{g}'s in terms of the partial derivatives of \code{f} and \code{g}'s respectively. 
See the \code{\link{e_MFB}} function, for evaluating this coefficient when the coefficients of 
\code{f} and to the coefficients of \code{g}'s are substituted with numerical values.}

\value{\item{string}{the expression of Faa di Bruno's formula}
}

\references{

G. M. Constantine, T. H. Savits  (1996) A Multivariate Faa Di Bruno Formula With 
Applications. Trans. Amer. Math. Soc. 348(2), 503-520.

E. Di Nardo, G. Guarino, D. Senato (2008) An unifying framework for k-statistics, polykays and their generalizations. 
Bernoulli. 14(2), 440-468. (download from \url{https://arxiv.org/pdf/math/0607623.pdf}) 
 
E. Di Nardo, G. Guarino, D. Senato (2008) Symbolic computation of moments of sampling distributions. 
Comp. Stat. Data Analysis. 52(11), 4909-4922. (download from \url{https://arxiv.org/abs/0806.0129}) 

E. Di Nardo E., G. Guarino, D. Senato (2011) A new algorithm for computing the multivariate Faa di Bruno's formula. 
Appl. Math. Comp. 217, 6286-6295. (download from \url{https://arxiv.org/abs/1012.6008})

}

\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}
}

\section{Warning}{The value of the first parameter is the same as the \code{\link{mkmSet}} function}

\note{
Called by the \code{\link{e_MFB}} function in the \code{kStatistics} package.
The routine uses the \code{\link{mkmSet}} function in the same package.
}



\seealso{
\code{\link{mkmSet}},
\code{\link{e_MFB}}
}

\examples{

#----------------------------------------#
# Univariate f with Univariate g         #
#----------------------------------------#

# The coefficient of z^2 in f[g(z)], that is f[2]g[1]^2 + f[1]g[2], where 
# f[1] is the coefficient of x in f(x) with x=g(z) 
# f[2] is the coefficient of x^2 in f(x) with  x=g(z) 
# g[1] is the coefficient of z in g(z)   
# g[2] is the coefficient of z^2 in g(z)   
# 
MFB( c(2), 1 )

# The coefficient of z^3 in f[g(z)], that is f[3]g[1]^3 + 3f[2]g[1]g[2] + f[1]g[3] 
#
MFB( c(3), 1 )

#----------------------------------------#
#  Univariate f with Multivariate g      #
#----------------------------------------#

# The coefficient of z1 z2 in f[g(z1,z2)], that is f[1]g[1,1] + f[2]g[1,0]g[0,1] 
# where 
# f[1]   is the coefficient of x in f(x) with x=g(z1,z2)
# f[2]   is the coefficient of x^2 in f(x) with x=g(z1,z2) 
# g[1,0] is the coefficient of z1 in g(z1,z2) 
# g[0,1] is the coefficient of z2 in g(z1,z2)  
# g[1,1] is the coefficient of z1 z2 in g(z1,z2)  
# 
MFB( c(1,1), 1 )

# The coefficient of z1^2 z2 in f[g(z1,z2)] 
#
MFB( c(2,1), 1 )

# The coefficient of z1 z2 z3 in f[g(z1,z2,z3)] 
#
MFB( c(1,1,1), 1 )

#----------------------------------------------------------------#
#  Multivariate f with Univariate/Multivariate g1, g2, ..., gn   #
#----------------------------------------------------------------#

# The coefficient of z in f[g1(z),g2(z)], that is  f[1,0]g1[1] + f[0,1]g2[1] where 
# f[1,0] is the coefficient of x1 in f(x1,x2) with x1=g1(z) and x2=g2(z)
# f[0,1] is the coefficient of x2 in f(x1,x2) with x1=g1(z) and x2=g2(z)
# g1[1]  is the coefficient of z of g1(z)  
# g2[1]  is the coefficient of z of g2(z)  
MFB( c(1), 2 )

# The coefficient of z1 z2 in f[g1(z1,z2),g2(z1,z2)], that is
# f[1,0]g1[1,1] + f[2,0]g1[1,0]g1[0,1] + f[0,1]g2[1,1] + f[0,2]g2[1,0]g2[0,1] + 
# f[1,1]g1[1,0]g2[0,1] + f[1,1]g1[0,1]g2[1,0] where 
# f[1,0]  is the coefficient of x1 in f(x1,x2) with x1=g1(z1,z2) and x2=g2(z1,z2)
# f[0,1]  is the coefficient of x2 in f(x1,x2) with x1=g1(z1,z2) and x2=g2(z1,z2)
# g1[1,1] is the coefficient of z1z2 in g1(z1,z2)  
# g1[1,0] is the coefficient of z1 in g1(z1,z2)   
# g1[0,1] is the coefficient of z2 in g1(z1,z2)  
# g2[1,1] is the coefficient of z1 z2 in g2(z1,z2)  
# g2[1,0] is the coefficient of z1 in g2(z1,z2)  
# g2[0,1] is the coefficient of z2 in g1(z1,z2)  
MFB( c(1,1), 2 )

# The coefficient of z1 in f[g1(z1,z2),g2(z1,z2),g3(z1,z2)] 
MFB( c(1,0), 3 )

# The coefficient of z1 z2 in f[g1(z1,z2),g2(z1,z2),g3(z1,z2)] 
MFB( c(1,1), 3 )

# The coefficient of z1^2 z2 in f[g1(z1,z2),g2(z1,z2)]  
MFB( c(2,1), 2 )

# The coefficient of z1^2 z2 in f[g1(z1,z2),g2(z1,z2),g3(z1,z2)]  
MFB( c(2,1), 3 )

# The previous result expressed in a compact form
for (m in unlist(strsplit( MFB(c(2,1),3), " + ", fixed=TRUE)) ) cat( m,"\n" )

# The coefficient of z1 z2 z3 in f[g1(z1,z2,z3),g2(z1,z2,z3),g3(z1,z2,z3)]  
MFB( c(1,1,1), 3 )

# The previous result expressed in a compact form
for (m in unlist(strsplit( MFB(c(1,1,1),3), " + ", fixed=TRUE)) ) cat( m,"\n" )

}


% R KEYWORDS
\keyword{ multivariate }
\keyword{ univar }
\keyword{ list }
\keyword{ symbolmath }
\keyword{ composition }
\keyword{ deriv }


