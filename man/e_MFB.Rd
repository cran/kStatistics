\name{e_MFB}
\alias{e_MFB}

\title{Evaluation of Faa di Bruno's formula}
\description{The function evaluates the Faa di Bruno's formula, output of the \code{\link{MFB}} 
function, when the coefficients of the exponential formal power series \code{f} and \code{g1,...,gn} 
in the composition \code{f[g1(),...,gn()]} are substituted with numerical values.}

\usage{
e_MFB(pv = c(), pn = 0, pf = c(), pg = c(), b = FALSE)
}

\arguments{
  \item{pv}{vector of integers, the subscript of Faa di Bruno's formula}
  \item{pn}{integer, the number of the inner formal power series \code{"g"}}
  \item{pf}{vector, the numerical values in place of the coefficients of the outer formal power series 
\code{"f"} or the string with the direct assignments in place of the coefficients of both \code{"f"} and \code{"g"}}
  \item{pg}{vector, the numerical values in place of the coefficients of the inner formal power series \code{"g"}  
   [Optional if \code{pf} is a string]}
  \item{b}{boolean}
}
 
\details{The output of the \code{\link{MFB}} function is a coefficient of the exponential formal 
power series compositions in the cases 
\tabular{cl}{
a) \tab univariate \code{f} with univariate \code{g} \cr
b) \tab univariate \code{f} with multivariate \code{g} \cr
c) \tab multivariate \code{f} with multivariates \code{{gi}}}
The \code{\link{e_MFB}} function evaluates this coefficient when the coefficients of \code{f} and 
\code{{gi}} are substituted with numerical values. These values are passed to the \code{\link{e_MFB}} 
function trough the third and the fourth input parameter. There is one 
further input boolean parameter: when equal to \code{TRUE}, the function prints the list of all 
the assignments. See the examples for more details on how to use this boolean parameter when the expression 
of the coefficients of \code{f} and \code{{gi}} becomes more complex.}

\value{
\item{numerical}{the evaluation of Faa di Bruno's formula}
}

\references{
E. Di Nardo, G. Guarino, D. Senato (2008) An unifying framework for k-statistics, polykays and their generalizations. 
Bernoulli. Vol. 14(2), 440-468. (download from \url{http://www.elviradinardo.it/lavori1.html}) 
 
E. Di Nardo, G. Guarino, D. Senato (2008) Symbolic computation of moments of sampling distributions. 
Comp. Stat. Data Analysis Vol. 52(11), 4909-4922, (download from \url{http://www.elviradinardo.it/lavori1.html}) 

E. Di Nardo, G. Guarino, D. Senato (2011) A new algorithm for computing the multivariate Faa di Bruno's formula. 
Appl. Math. Comp. 217, 6286--6295. (download from \url{https://arxiv.org/abs/1012.6008})

}

\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}
}

\section{Warning}{The value of the first parameter is the same as the \code{\link{mkmSet}} function.}

\note{Called from the \code{\link{MFB}} function in the \code{kStatistics} package.}

\seealso{
\code{\link{mkmSet}},
\code{\link{MFB}}
}

\examples{

# The numerical value of f[1]g[1,1] + f[2]g[1,0]g[0,1], that is the coefficient of z1z2 in 
# f(g1(z1,z2),g2(z1,z2))) output of MFB(c(1,1),1) when 
# f[1] = 5 and f[2] = 10 
# g[0,1]=3,  g[1,0]=6,  g[1,1]=9
e_MFB(c(1,1),1, c(5,10), c(3,6,9))

# Same as the previous example, with a string of assignments as third input parameter
e_MFB(c(1,1),1, "f[1]=5, f[2]=10, g[0,1]=3, g[1,0]=6, g[1,1]=9")


# Use the boolean parameter to verify the assignments to the coefficients of "f" and "g", 
# that is f[1]=5, f[2]=10, g[0,1]=3, g[1,0]=6, g[1,1]=9
e_MFB(c(1,1),1, c(5,10), c(3,6,9), TRUE)

# To recover which coefficients are involved, run the function without any assignment. 
# The error message recalls which coefficients are necessary, that is
# e_MFB(c(1,1),1)
# Error in e_MFB(c(1, 1), 1) : 
#   The third parameter must contain the 2 values of f: f[1] f[2]. 
#   The fourth parameter must contain the 3 values of g: g[0,1] g[1,0] g[1,1]

# To assign correctly the values to the coefficients of "f" and "g" when the functions 
# become more complex:
# 1) run e_MFB(c(1,1),2) and get the errors with the indication of the involved coefficients 
#    of "f" and "g", that is 
#      The third parameter must contain the 5 values of f: 
#                      f[0,1] f[0,2] f[1,0] f[1,1] f[2,0]
#      The fourth parameter must contain the 6 values of g: 
#                      g1[0,1] g1[1,0] g1[1,1] g2[0,1] g2[1,0] g2[1,1]"
# 2) initialize f[0,1] f[0,2] f[1,0] f[1,1] f[2,0] with - for example - the first 5 integer 
#    numbers and do the same for g1[0,1] g1[1,0] g1[1,1] g2[0,1] g2[1,0] g2[1,1], that is
#    e_MFB(c(1,1),2, c(1:5), c(1:6), TRUE)
# 3) trought the boolean value TRUE, recover the string f[0,1]=1, f[0,2]=2, f[1,0]=3, f[1,1]=4, 
#    f[2,0]=5, g1[0,1]=1, g1[1,0]=2, g1[1,1]=3, g2[0,1]=4, g2[1,0]=5, g2[1,1]=6
# 4) copy and past the string in place of " ... " when run 
#    e_MFB(c(1,1),1," ... ")
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

