\name{kStatistics-package}
\alias{kStatistics-package}
\alias{kStatistics}
\alias{kstatistics}
\alias{k-Statistics}
\alias{k-statistics}
\docType{package}
\title{
\packageTitle{kStatistics}
}
\description{kStatistics is a package allowing to estimate (joint) cumulants and products of (joint) cumulants of a given 
distribution using (multivariate) k-statistics and (multivariate) polykays, which are symmetric unbiased estimators. The 
procedures rely on a symbolic method arising from the classical umbral calculus and described in the referred papers. In 
the package, a set of combinatorial devices are given such as integer partitions, set partitions and multiset subdivisions, 
pairing and merging of multisets, useful in the construction of these estimators. In the package, there are also functions 
to recover univariate and multivariate cumulants from univariate and multivariate moments (and viceversa) using Faa di Bruno's 
formula. The function returning Faa di Bruno's formula allows to recover coefficients of composition of exponential power series
such as f(g(z)) with f and g both univariate, such as f(g(z1,...,zn)) with f univariate and g multivariate, such as 
f(g1(z1,...,zn),...,gm(z1,...,zn)) with f and g both multivariate. Faa di Bruno's formula might also be employed to recover 
iterated derivative of all these compositions. Lastly, using Faa di Bruno's formula, some special families of polynomials 
are also generated, such as Bell polynomials, generalized complete Bell polynomials, partition polynomials, generalized 
partition polynomials. Applications of these polynomials are described in the referred papers. } 


\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}


\packageAuthor{kStatistics}

Maintainer: \packageMaintainer{kStatistics}
}

\references{

Charalambides C.A. (2002) Enumerative Combinatoris, Chapman & Haii/CRC. 

Constantine G. M., Savits T. H.  (1996) A Multivariate Faa Di Bruno Formula With 
Applications. Trans. Amer. Math. Soc. 348(2), 503--520.

E. Di Nardo E. (2016) On multivariable cumulant polynomial sequence with applications. Jour. 
Algebraic Statistics 7(1), 72-89.  (download from \url{http://arxiv.org/abs/1606.01004})

E. Di Nardo, G. Guarino, D. Senato (2008) An unifying framework for k-statistics, polykays and their generalizations. 
Bernoulli. 14(2), 440-468. (download from \url{http://arxiv.org/pdf/math/0607623.pdf}) 
 
E. Di Nardo, G. Guarino, D. Senato (2008) Symbolic computation of moments of sampling distributions. 
Comp. Stat. Data Analysis. 52(11), 4909-4922. (download from \url{http://arxiv.org/abs/0806.0129}) 

E. Di Nardo, G. Guarino, D. Senato (2009) A new method for fast computing unbiased estimators of cumulants. 
Statistics and Computing, 19, 155-165.  (download from \url{https://arxiv.org/abs/0807.5008}) 

E. Di Nardo, G. Guarino, D. Senato (2011) A new algorithm for computing the multivariate Faa di Bruno's formula. 
Appl. Math. Comp. 217, 6286--6295. (download from \url{http://arxiv.org/abs/1012.6008})

E. Di Nardo, M. Marena, P. Semeraro (2020) On non-linear dependence of multivariate subordinated Levy processes.
In press Stat. Prob. Letters (download from \url{https://arxiv.org/abs/2004.03933}) 

P. McCullagh, J. Kolassa (2009), Scholarpedia, 4(3):4699. \url{http://www.scholarpedia.org/article/Cumulants}

}

\keyword{ multivariate }
\keyword{ univar }
\keyword{ list }
\keyword{ symbolmath }
\keyword{ combinatorics }

\examples{
# Some of the most important functions:

# Data assignment
data1<-c(16.34, 10.76, 11.84, 13.55, 15.85, 18.20, 7.51, 10.22, 12.52, 14.68, 
16.08, 19.43,8.12, 11.20, 12.95, 14.77, 16.83, 19.80, 8.55, 11.58, 12.10, 
15.02, 16.83, 16.98, 19.92, 9.47, 11.68, 13.41, 15.35, 19.11)

# Data assignment
data2<-list(c(5.31,11.16),c(3.26,3.26),c(2.35,2.35),c(8.32,14.34),c(13.48,49.45),
c(6.25,15.05),c(7.01,7.01),c(8.52,8.52),c(0.45,0.45),c(12.08,12.08),c(19.39,10.42))

# Return an estimate of the third cumulant of the population distribution with the indication 
# of which function has been employed
# KS:[1] -1.44706
nPolyk(c(3), data1, TRUE)

# Return an estimate of the product of the mean and the variance of the population distribution 
# with the indication of which function has been employed 
# PS:[1] 177.4233
nPolyk( list(  c(2), c(1) ), data1, TRUE)

# Return an estimate of the joint cumulant c[2,1] of the population distribution  with the 
# indication of which function has been employed
# KM:[1] -23.7379
nPolyk(c(2,1), data2, TRUE);

# Return an estimate of the product of joint cumulants c[2,1]*c[1,0] of the population 
# distribution with the indication of which function has been employed
# PM:[1] 48.43243
nPolyk( list(  c(2,1), c(1,0) ), data2, TRUE)

# Return all subdivisions of a multiset with only one element of multiplicity 3
mkmSet(3)

# Return all subdivisions of a multiset with two elements, 
# having multiplicity respectively 2 and 1
mkmSet(c(2,1)) 

# Faa di Bruno's formula (Univariate with Univariate Case)
# The coefficient of z^2 in f(g(z)), that is f[2]g[1]^2 + f[1]g[2], where 
# f[1] is the coefficient of t in f(t) with t=g(z) 
# f[2] is the coefficient of t^2 in f(t) with  t=g(z) 
# g[1] is the coefficient of z in g(z)   
# g[2] is the coefficient of z^2 in g(z)   
# 
MFB( c(2), 1 )

# Faa di Bruno's formula (Univariate with Multivariate Case)    
# The coefficient of z1 z2 in f(g(z1,z2)), that is f[1]g[1,1] + f[2]g[1,0]g[0,1] 
# where 
# f[1]   is the coefficient of t in f(t) with t=g(z1,z2)
# f[2]   is the coefficient of t^2 in f(t) with t=g(z1,z2) 
# g[1,0] is the coefficient of z1 in g(z1,z2) 
# g[0,1] is the coefficient of z2 in g(z1,z2)  
# g[1,1] is the coefficient of z1z2 in g(z1,z2)  
# 
MFB( c(1,1), 1 )

# Faa di Bruno's formula (Multivariate with Multivariate Case) 
# The coefficient of z in f((g1(z),g2(z)), that is  f[1,0]g1[1] + f[0,1]g2[1] where 
# f[1,0] is the coefficient of t1 in f(t1,t2) with t1=g1(z) and t2=g2(z)
# f[0,1] is the coefficient of t2 in f(t1,t2) with t1=g1(z) and t2=g2(z)
# g1[1]  is the coefficient of z of g1(z)  
# g2[1]  is the coefficient of z of g2(z)  
MFB( c(1), 2 )

# The numerical value of f[1]g[1,1] + f[2]g[1,0]g[0,1], that is the coefficient of x1x2 
# in f(g1(x1,x2),g2(x1,x2))) output of MFB(c(1,1),1) when 
# f[1] = 5 and f[2] = 10 
# g[0,1]=3,  g[1,0]=6,  g[1,1]=9
e_MFB(c(1,1),1, c(5,10), c(3,6,9))

# Multivariate cumulant k[3,1] in terms of the multivariate moments m[i,j] for i=0,1,2,3 and j=0,1.
cum2mom(c(3,1))

# Multivariate moment m[3,1] in terms of the multivariate cumulants k[i,j] for i=0,1,2,3 and j=0,1.
mom2cum(c(3,1))

# Partition polynomial F[5]
pPart(5)

#  The general partition polynomial G[2], that is a2(y1^2) + a1(y2)
gpPart(2)

# The complete exponential Bell Polynomial B[5], that is (y1^5) + 10(y1^3)(y2) + 
# 15(y1)(y2^2) + 10(y1^2)(y3) + 10(y2)(y3) + 5(y1)(y4) + (y5)
BellPol(5)

# The partial ordinary Bell polynomial Bo[5,3], 330(y1)(y2^2) + 60(y1^2)(y3)
BellPol(5,3,FALSE);

# The generalized complete Bell Polynomial for n=1, m=1 and g1=g, 
# that is (y^2)g[1]^2 + (y)g[2]
#
GCBellPol( c(2),1 )

# The eneralized complete Bell Polynomial for n=1, m=2 and g1=g, corresponding to the 
# cumulant polinomial indexed by (2,1), 
# that is 2(y^2)g[1,0]g[1,1] + (y^3)g[0,1]g[1,0]^2 + (y)g[2,1] + (y^2)g[0,1]g[2,0]
#
GCBellPol( c(2,1),1 )


# The generalized complete Bell Polynomial for n=2, m=2 and g1=g2=g, corresponding to the 
# cumulant polinomial in y1+y2 indexed by (1,1), 
# that is (y1)g[1,1] + (y1^2)g[0,1]g[1,0] + (y2)g[1,1] + (y2^2)g[0,1]g[1,0] + 2(y1)(y2)g[0,1]g[1,0]
#
GCBellPol( c(1,1),2, TRUE )

# The polynomial 2(y^2)g[1,1]g[1,0] + (y^3)g[1,0]^2g[0,1] + (y)g[2,1] + (y^2)g[2,0]g[0,1], output of 
# GCBellPol( c(2,1),1 ), when g[0,1]=1, g[1,0]=2, g[1,1]=3, g[2,0]=4, g[2,1]=5, 
# that is 16(y^2) + 4(y^3) + 5(y)
#
e_GCBellPol(c(2,1),1,,c(1:5))
#
# OR (same output)
#
e_GCBellPol(c(2,1),1,,c(1,2,3,4,5))
#
# OR (same output)
#
e_GCBellPol( c(2,1),1,"g[0,1]=1, g[1,0]=2, g[1,1]=3, g[2,0]=4, g[2,1]=5" )

# The polynomial 2(y^2)g[1,1]g[1,0] + (y^3)g[1,0]^2g[0,1] + (y)g[2,1] + (y^2)g[2,0]g[0,1], output of 
# GCBellPol( c(2,1),1 ) when g[0,1]=1, g[1,0]=2, g[1,1]=3, g[2,0]=4, g[2,1]=5 and y=7, that is 2191
#
e_GCBellPol( c(2,1),1,c(7),c(1:5) )
#
# OR (same output)
#
e_GCBellPol( c(2,1),1,"y=7, g[0,1]=1, g[1,0]=2, g[1,1]=3, g[2,0]=4, g[2,1]=5" )

 
}
