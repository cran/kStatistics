\name{MFB}
\alias{MFB}

\title{Faa di Bruno's formula}
\description{The routine generates the coefficients of the composition of (exponential) formal power series provided by 
the univariate and multivariate Faa di Bruno's formula indexed by the integers i1,i2,....}

\usage{
MFB(v = c(), n = 0)
}

\arguments{
  \item{v}{vector of integers, the subscript of the coefficient}
  \item{n}{integer, the number of inner functions "g"}
}

\details{The function computes the coefficients of the composition of (exponential) formal power series: \cr
1) Univariate with Univariate Case:      f(g(z),z,z...) \cr
2) Univariate with Multivariate Case:    f(g(z1,z2,...),z1,z1,..,z2,z2...,.. ) \cr
3) Multivariate with Multivariate Case:  f(g1(z1,z2,..),g2(z1,z2,..),..), z1,z1,...,z2,z2,..,..) \cr
If i1 is the number of z1, i2 is the number of z2 and so on, then (i1,i2, ...) is the index of the output coefficient 
corresponding to the product z1^i1 z2^i2 .... \cr
Note that this coefficient is also related to the (partial) derivative of order (i1,i2, ...) of the composition of 
(exponential) formal power series evaluated at z1=0, z2=0, and so on. See the function e_MFB, for evaluating 
these coefficients by assigning numerical values to the coefficients of f() and to the coefficients of g1(),g2(),...}

\value{\item{string}{the expression of Faa di Bruno's formula}
}

\references{

E. Di Nardo, G. Guarino, D. Senato (2008) An unifying framework for k-statistics, polykays and their generalizations. 
Bernoulli. 14(2), 440-468. (download from \url{http://arxiv.org/pdf/math/0607623.pdf}) 
 
E. Di Nardo, G. Guarino, D. Senato (2008) Symbolic computation of moments of sampling distributions. 
Comp. Stat. Data Analysis. 52(11), 4909-4922. (download from \url{http://arxiv.org/abs/0806.0129}) 

E. Di Nardo E., G. Guarino, D. Senato (2011) A new algorithm for computing the multivariate Faa di Bruno's formula. 
Appl. Math. Comp. 217, 6286--6295. (download from \url{http://arxiv.org/abs/1012.6008})

}

\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}
}

\section{Warning}{The value of the first parameter is the same as the mkmSet function, i.e. the number of assigned blocks.}

\note{
Called by the function \code{\link{e_MFB}} in the package \code{kStatistics}.
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

# The coefficient of z^2 in f(g(z)), that is f[2]g[1]^2 + f[1]g[2], where 
# f[1] is the coefficient of t in f(t) with t=g(z) 
# f[2] is the coefficient of t^2 in f(t) with  t=g(z) 
# g[1] is the coefficient of z in g(z)   
# g[2] is the coefficient of z^2 in g(z)   
# 
MFB( c(2), 1 )

# The coefficient of z^3 in f(g(z)), that is f[3]g[1]^3 + 3f[2]g[1]g[2] + f[1]g[3] 
#
MFB( c(3), 1 )

#----------------------------------------#
#  Univariate f with Multivariate g      #
#----------------------------------------#

# The coefficient of z1 z2 in f(g(z1,z2)), that is f[1]g[1,1] + f[2]g[1,0]g[0,1] 
# where 
# f[1]   is the coefficient of t in f(t) with t=g(z1,z2)
# f[2]   is the coefficient of t^2 in f(t) with t=g(z1,z2) 
# g[1,0] is the coefficient of z1 in g(z1,z2) 
# g[0,1] is the coefficient of z2 in g(z1,z2)  
# g[1,1] is the coefficient of z1 z2 in g(z1,z2)  
# 
MFB( c(1,1), 1 )

# The coefficient of z1^2 z2 in f(g(z1,z2)) 
#
MFB( c(2,1), 1 )

# The coefficient of z1 z2 z3 in f(g(z1,z2,z3)) 
#
MFB( c(1,1,1), 1 )

#-------------------------------------------------#
#  Multivariate f with Multivariate g1, g2, ...   #
#-------------------------------------------------#

# The coefficient of z in f(g1(z),g2(z)), that is  f[1,0]g1[1] + f[0,1]g2[1] where 
# f[1,0] is the coefficient of t1 in f(t1,t2) with t1=g1(z) and t2=g2(z)
# f[0,1] is the coefficient of t2 in f(t1,t2) with t1=g1(z) and t2=g2(z)
# g1[1]  is the coefficient of z of g1(z)  
# g2[1]  is the coefficient of z of g2(z)  
MFB( c(1), 2 )

# The coefficient of z1 z2 in f(g1(z1,z2),g2(z1,z2)), that is
# f[1,0]g1[1,1] + f[2,0]g1[1,0]g1[0,1] + f[0,1]g2[1,1] + f[0,2]g2[1,0]g2[0,1] + 
# f[1,1]g1[1,0]g2[0,1] + f[1,1]g1[0,1]g2[1,0] where 
# f[1,0]  is the coefficient of t1 in f(t1,t2) with t1=g1(z1,z2) and t2=g2(z1,z2)
# f[0,1]  is the coefficient of t2 in f(t1,t2) with t1=g1(z1,z2) and t2=g2(z1,z2)
# g1[1,1] is the coefficient of z1z2 in g1(z1,z2)  
# g1[1,0] is the coefficient of z1 in g1(z1,z2)   
# g1[0,1] is the coefficient of z2 in g1(z1,z2)  
# g2[1,1] is the coefficient of z1 z2 in g2(z1,z2)  
# g2[1,0] is the coefficient of z1 in g2(z1,z2)  
# g2[0,1] is the coefficient of z2 in g1(z1,z2)  
MFB( c(1,1), 2 )

# The coefficient of z1 in f(g1(z1,z2),g2(z1,z2),g3(z1,z2)) 
MFB( c(1,0), 3 )

# The coefficient of z1 z2 in f(g1(z1,z2),g2(z1,z2),g3(z1,z2)) 
MFB( c(1,1), 3 )

# The coefficient of z1^2 z2 in f(g1(z1,z2),g2(z1,z2))  
MFB( c(2,1), 2 )

# The coefficient of z1^2 z2 in f(g1(z1,z2),g2(z1,z2),g3(z1,z2))  
MFB( c(2,1), 3 )

# The previous result expressed in a compact form
for (m in unlist(strsplit( MFB(c(2,1),3), " + ", fixed=TRUE)) ) cat( m,"\n" )

# The coefficient of z1 z2 z3 in f(g1(z1,z2,z3),g2(z1,z2,z3),g3(z1,z2,z3))  
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


