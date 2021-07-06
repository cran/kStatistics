\name{Set2expr}
\alias{Set2expr}
\title{Conversion of a vector into a string}

\description{
The function converts a set into a string.
}
\usage{
Set2expr(v = NULL )
}
\arguments{
  \item{v}{Set}
}

\value{
 \item{string}{the string}
}

\note{Called by the \code{\link{MFB}} and \code{\link{MFB}}  functions in the \code{\link{kStatistics}} package
being useful for manipulating the result before its print.}

\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}
}



\examples{
 
# To print 6f[3]^2g[2]^5 run 
Set2expr( list(c("1","2","f","3","2"),c("1","3","g","2","5"))) 

# Run MFB(c(3),1) to recover f[3]g[1]^3 + 3f[2]g[1]g[2] + f[1]g[3]
# Run S<-MFB2Set(MFB(c(3),1)) to convert the output of MFB(c(3),1) into a vector.  
# The result is
# "1" "1" "f" "3" "1"
# "1" "1" "g" "1" "3"
# "2" "3" "f" "2" "1"
# "2" "1" "g" "1" "1"
# "2" "1" "g" "2" "1"
# "3" "1" "f" "1" "1"
# "3" "1" "g" "3" "1"
# To set f[2]=1, run S[[3]][4]<-"" and S[[3]][3]<-"".
# Then run Set2expr(S) to recover  
# f[3]g[1]^3 + 3g[1]g[2] + f[1]g[3]
#
S<-MFB2Set(MFB(c(3),1))
S[[3]][4]<-""
S[[3]][3]<-""
Set2expr(S)
}


% R KEYWORDS
\keyword{ multivariate }
\keyword{ univar }
\keyword{ list }
\keyword{ symbolmath }

