\name{e_MFB}
\alias{e_MFB}

\title{Evaluation of Faa di Bruno's formula}
\description{The function evaluates the Faa di Bruno's formula obtained as output of  the function \code{\link{MFB}}, when
values are assigned to the coefficients of the exponential formal power series f() and g1(),...,gn() in f[g1(),...,gn()].}

\usage{
e_MFB(pv = c(), pn = 0, pf = c(), pg = c(), b = FALSE)
}

\arguments{
  \item{pv}{vector of integers, the subscript of Faa di Bruno's formula}
  \item{pn}{integer, number of the inner formal power series "g" in the composition}
  \item{pf}{vector, the values to be assigned to the coefficients of the outer formal power series f() or the string with the direct assignments 
   to the coefficients of both f() and g()}
  \item{pg}{vector, the values to be assigned to the coefficients of the inner formal power series "g"  
   [Optional if pf is the string with the assignments to the coefficients of f() and g()]}
  \item{b}{boolean}
}
 
\details{The output of the function \code{\link{MFB}} is a coefficient of the composition of the exponential formal power series f() 
and g() in the cases univariate f univariate g, univariate f multivariate g and multivariate f multivariates g1,...,gn. The function 
e_MFB evaluates this coefficient by assigning values to the coefficients of "f" and "g". These values are passed to e_MFB trough 
the third and the fourth input parameters. There is one more input boolean parameter: when it is TRUE, the function prints 
the list of all the assignments. See the examples for more details on how to use this boolean parameter when the form of "f" and "g" 
becomes more complex.}

\value{
\item{numerical}{evaluation of the output of the function \code{\link{MFB}}}
}

\references{
E. Di Nardo, G. Guarino, D. Senato (2008) An unifying framework for k-statistics, polykays and their generalizations. 
Bernoulli. Vol. 14(2), 440-468. (download from \url{http://www.elviradinardo.it/lavori1.html}) 
 
E. Di Nardo, G. Guarino, D. Senato (2008) Symbolic computation of moments of sampling distributions. 
Comp. Stat. Data Analysis Vol. 52(11), 4909-4922, (download from \url{http://www.elviradinardo.it/lavori1.html}) 

E. Di Nardo, G. Guarino, D. Senato (2011) A new algorithm for computing the multivariate Faa di Bruno's formula. 
Appl. Math. Comp. 217, 6286--6295. (download from \url{http://arxiv.org/abs/1012.6008})

}

\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}
}

\section{Warning}{The value of the first parameter is the same as the mkmSet function, i.e. the number of considered blocks.}

\note{This function is called from the function \code{\link{MFB}} of the package \code{kStatistics}.}



\seealso{
\code{\link{mkmSet}},
\code{\link{MFB}}
}

\examples{

# The numerical value of f[1]g[1,1] + f[2]g[1,0]g[0,1], that is the coefficient of x1x2 in 
# f(g1(x1,x2),g2(x1,x2))) output of MFB(c(1,1),1) when 
# f[1] = 5 and f[2] = 10 
# g[0,1]=3,  g[1,0]=6,  g[1,1]=9
e_MFB(c(1,1),1, c(5,10), c(3,6,9))

# Same as the previous example, with a string of assignments as third input parameter
e_MFB(c(1,1),1, "f[1]=5, f[2]=10, g[0,1]=3, g[1,0]=6, g[1,1]=9")


# Use the boolean parameter to verify the assignments to the coefficients of "f" and "g", 
# that is f[1]=5, f[2]=10, g[0,1]=3, g[1,0]=6, g[1,1]=9
e_MFB(c(1,1),1, c(5,10), c(3,6,9), TRUE)

# To recover which coefficients are involved in the Faa di Bruno's formula, run the function 
# without any assignment. The error message recalls which coefficients are necessary, that is
# e_MFB(c(1,1),1)
# Error in e_MFB(c(1, 1), 1) : 
#   The third parameter must contain the 2 values of f: f[1] f[2]. 
#   The fourth parameter must contain the 3 values of g: g[0,1] g[1,0] g[1,1]

# Syntax to correctly assign values to the coefficients of "f" and "g" when the functions 
# become more complex.
# 1) run e_MFB(c(1,1),2) and get the errors with the indication of the involved coefficients 
#    of "f" and "g", that is 
#      The third parameter must contain the 5 values of f: 
#                      f[0,1] f[0,2] f[1,0] f[1,1] f[2,0]
#      The fourth parameter must contain the 6 values of g: 
#                      g1[0,1] g1[1,0] g1[1,1] g2[0,1] g2[1,0] g2[1,1]"
# 2) initialize f[0,1] f[0,2] f[1,0] f[1,1] f[2,0] with the first 5 natural numbers and do 
#    the same for g1[0,1] g1[1,0] g1[1,1] g2[0,1] g2[1,0] g2[1,1], that is
#    e_MFB(c(1,1),2, c(1:5), c(1:6), TRUE)
# 3) trought the boolean value TRUE, recover the string f[0,1]=1, f[0,2]=2, f[1,0]=3, f[1,1]=4, 
#    f[2,0]=5, g1[0,1]=1, g1[1,0]=2, g1[1,1]=3, g2[0,1]=4, g2[1,0]=5, g2[1,1]=6
# 4) copy and past the string as input in place of " ... " when run e_MFB(c(1,1),1," ... ")
# 5) change the assignments if necessary
cfVal<-"f[0,1]=2, f[0,2]=5, f[1,0]=13, f[1,1]=-4, f[2,0]=0"
cgVal<-"g1[0,1]=-2.1, g1[1,0]=2,g1[1,1]=3.1, g2[0,1]=5, g2[1,0]=0, g2[1,1]=6.1"
cVal<-paste0(cfVal,",",cgVal)
e_MFB(c(1,1),2,cVal)
}


% R KEYWORDS
\keyword{multivariate}
\keyword{univar}
\keyword{list}
\keyword{symbolmath}
\keyword{combinatorics}

