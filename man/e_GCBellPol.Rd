\name{e_GCBellPol}
\alias{e_GCBellPol}
\title{Evaluation of Generalized Complete Bell Polynomial}
\description{The function assigns numerical values to the coefficients of the generalized complete Bell polynomial, output of the function 
\code{\link{GCBellPol}}. The input vector of integers identifies the subscripts of the polynomial coefficients. When numerical values are
assigned to the variables y1,...,yn too, the function returns an overall numerical value. }

\usage{
e_GCBellPol(pv = c(), pn = 0, pyc = c(), pc = c(), b = FALSE)
}
\arguments{
  \item{pv}{vector of integers, the subscript of the generalized complete Bell polynomial}
  \item{pn}{integer, the number of y's variables}
  \item{pyc}{vector, the values to be assigned to the y's variables [optional], or string, the direct assignment of the
y's variables and/or the related coefficients}
  \item{pc}{vector, the values to be assigned to the coefficients of the polynomial, [optional if pyc is a string]}
  \item{b}{boolean, if TRUE the function prints the list of all the assignments}
}

\details{The output of the function \code{\link{GCBellPol}} is a coefficient of exp(y1 g1(z1,...,zm) + ... 
+ yn gn(z1,...,zm)), where y1,...,yn are variables. The function e_GCBellPol allows to assign numerical
values to the coefficients of the power series g1,...,gn together with the variables y1,...,yn (optional). These values are passed to e_GCBellPol
through the third and the fourth input parameter. The y's and the g's in the resulting expression are managed in lexicographic 
order. There is one more input boolean parameter: when it is TRUE, the function prints the list of all the 
assignments. See the examples for more details on the employment of this boolean parameter for assigning values to the 
coefficients and/or to the variables of the generalized complete Bell polynomial.}

\value{
\item{string or numerical}{evaluation of the generalized complete Bell Polynomial}}

\references{
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
Called by the function \code{\link{GCBellPol}} in the package \code{kStatistics}.
}

\seealso{
\code{\link{mkmSet}},
\code{\link{MFB}},
\code{\link{GCBellPol}}
}

\examples{

#-------------------------------------------------------------------------------#
# Evaluation of the generalized complete Bell polynomial indexed by 2
#-------------------------------------------------------------------------------#
#
# The polynomial (y^2)g[1]^2 + (y^1)g[2], output of GCBellPol( c(2),1 ), when
# g[1]=3 and g[2]=4, that is 9(y^2) + 4(y)
#
e_GCBellPol( c(2),1,,c(3,4) )
#
# OR (same output)
#
e_GCBellPol( c(2),1,"g[1]=3,g[2]=4" )

# Check the assignments setting the boolean variable equals TRUE, that is g[1]=3 and g[2]=4
e_GCBellPol( c(2),1,,c(3,4),TRUE )

# The numerical value of (y^2)g[1]^2 + (y^1)g[2], output of GCBellPol( c(2),1 ), when 
# g[1]=3 and g[2]=4 and y=7, that is 469
#
e_GCBellPol( c(2),1,c(7),c(3,4) )
#
# OR (same output)
#
e_GCBellPol( c(2),1,"y=7, g[1]=3,g[2]=4" )

# Check the assignments setting the boolean variable equals TRUE, that is g[1]=3 and g[2]=4 
# and y=7
e_GCBellPol( c(2),1,c(7),c(3,4),TRUE )

#-------------------------------------------------------------------------------#
# Evaluation of the generalized complete Bell polynomial indexed by (2,1)
#-------------------------------------------------------------------------------#
#
# The polynomial 2(y^2)g[1,1]g[1,0] + (y^3)g[1,0]^2g[0,1] + (y)g[2,1] + (y^2)g[2,0]
# g[0,1], output of GCBellPol( c(2,1),1 ), when g[0,1]=1, g[1,0]=2, g[1,1]=3, g[2,0]=4, 
# g[2,1]=5, that is 16(y^2) + 4(y^3) + 5(y)
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

# Check the assignments setting the boolean variable equals TRUE, that is
# g[0,1]=1, g[1,0]=2, g[1,1]=3, g[2,0]=4, g[2,1]=5
e_GCBellPol( c(2,1),1,,c(1:5), TRUE )

# The numerical value of 2(y^2)g[1,1]g[1,0] + (y^3)g[1,0]^2g[0,1] + (y)g[2,1] + (y^2)g[2,0]
# g[0,1], output of GCBellPol( c(2,1),1 ) when g[0,1]=1, g[1,0]=2, g[1,1]=3, g[2,0]=4, 
# g[2,1]=5 and y=7, that is 2191
#
e_GCBellPol( c(2,1),1,c(7),c(1:5) )
#
# OR (same output)
#
e_GCBellPol( c(2,1),1,"y=7, g[0,1]=1, g[1,0]=2, g[1,1]=3, g[2,0]=4, g[2,1]=5" )

# Check the assignments setting the boolean variable equals TRUE, that is 
# g[0,1]=1, g[1,0]=2, g[1,1]=3, g[2,0]=4, g[2,1]=5, y=7
e_GCBellPol( c(2,1),1,c(7),c(1:5) )

#-----------------------------------------------------------------------------------#
# Evaluation of the generalized complete Bell Polynomial indexed by (1,1)
#-----------------------------------------------------------------------------------#

# The polynomial (y1)g1[1,1] + (y1^2)g1[1,0]g1[0,1] + (y2)g2[1,1] + (y2^2)g2[1,0]
# g2[0,1] + (y1)(y2)g1[1,0]g2[0,1] + (y1)(y2)g1[0,1]g2[1,0], output of GCBellPol(c(1,1),2) 
# when g1[0,1]=1, g1[1,0]=2, g1[1,1]=3, g2[0,1]=4, g2[1,0]=5, g2[1,1]=6, that is 
# 3(y1) + 2(y1^2) + 6(y2) + 20(y2^2) + 13(y1)(y2)
#
e_GCBellPol( c(1,1),2,,c(1:6))
#
# OR (same output)
#
e_GCBellPol(c(1,1),2,,c(1,2,3,4,5,6))
#
# OR (same output)
# 
e_GCBellPol( c(1,1),2,"g1[0,1]=1, g1[1,0]=2, g1[1,1]=3, g2[0,1]=4, g2[1,0]=5, g2[1,1]=6" )


# Check the assignments setting the boolean variable equals TRUE, that is 
# g1[0,1]=1, g1[1,0]=2, g1[1,1]=3, g2[0,1]=4, g2[1,0]=5, g2[1,1]=6
e_GCBellPol( c(1,1),2,,c(1:6), TRUE )

# The numerical value of (y1)g1[1,1] + (y1^2)g1[1,0]g1[0,1] + (y2)g2[1,1] + (y2^2)g2[1,0]
# g2[0,1] + (y1)(y2)g1[1,0]g2[0,1] + (y1)(y2)g1[0,1]g2[1,0], output of GCBellPol(c(1,1),2) 
# when g1[0,1]=1, g1[1,0]=2, g1[1,1]=3, g2[0,1]=4, g2[1,0]=5, y1=7 and y2=8, that is 2175
e_GCBellPol( c(1,1),2,c(7,8),c(1:6))
#
# OR (same output)
# 
cVal<-"y1=7, y2=8, g1[0,1]=1, g1[1,0]=2, g1[1,1]=3, g2[0,1]=4, g2[1,0]=5,g2[1,1]=6"
e_GCBellPol(c(1,1),2,cVal)

# To recover which coefficients and variables are involved in the generalized complete 
# Bell polynomial, run the function without any assignment. 
# The error message prints which coefficients and variables are involved, that is
# Error in e_GCBellPol(c(1, 1), 2) : 
#   The third parameter must contain the 2 values of y: y1 y2. 
#   The fourth parameter must contain the 6 values of g: 
#       g1[0,1] g1[1,0] g1[1,1] g2[0,1] g2[1,0] g2[1,1]

# Syntax to correctly assign values to the coefficients and the variables.
# 1) run e_GCBellPol(c(1, 1), 2) and get the errors with the indication of the involved 
#    coefficients and variables, that is 
#      The third parameter must contain the 2 values of y: y1 y2
#      The fourth parameter must contain the 6 values of g: 
#          g1[0,1] g1[1,0] g1[1,1] g2[0,1] g2[1,0] g2[1,1]
# 2) initialize g1[0,1] g1[1,0] g1[1,1] g2[0,1] g2[1,0] g2[1,1] with the first 6 natural 
#    numbers and do the same for y1 and y2, that is
#          e_GCBellPol(c(1,1),2, c(1,2), c(1,2,3,4,5,6), TRUE)
# 3) trought the boolean value TRUE, recover the string y1=1, y2=1, g1[0,1]=1, g1[1,0]=2, 
#    g1[1,1]=3, g2[0,1]=4, g2[1,0]=5, g2[1,1]=6 
# 4) copy and past the string as input in place of "..." when run 
#    e_GCBellPol(c(1,1),2,"...")
# 5) change the assignments if necessary
cVal<-"y1=10,y2=11,g1[0,1]=1.1,g1[1,0]=-2,g1[1,1]=3.2,g2[0,1]=-4,g2[1,0]=10,g2[1,1]=6"
e_GCBellPol(c(1,1), 2,cVal) 
}

% R KEYWORDS
\keyword{multivariate}
\keyword{univar}
\keyword{list}
\keyword{symbolmath}
\keyword{combinatorics}