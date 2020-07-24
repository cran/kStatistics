\name{Set2expr}
\alias{Set2expr}
\title{converts a vector into a string
}
\description{
Converts a set into a string. Useful for manipulating the result before being printed again.
}
\usage{
Set2expr(v = c())
}
\arguments{
  \item{v}{Set}
}

\value{
 \item{string}{output as string}
}

\author{
Elvira Di Nardo \email{elvira.dinardo@unito.it},\cr 
Giuseppe Guarino \email{giuseppe.guarino@rete.basilicata.it}
}

\seealso{
\code{\link{MFB}}
\code{\link{MFB2Set}}
}

\examples{
 
# MFB(c(3),1) generate f[3]g[1]^3 + 3f[2]g[1]g[2] + f[1]g[3]
# Convert the output of the MFB(c(3),1) to a vector using
# S<-MFB2Set(MFB(c(3),1)). It generates the following result:
# "1" "1" "f" "3" "1"
# "1" "1" "g" "1" "3"
# "2" "3" "f" "2" "1"
# "2" "1" "g" "1" "1"
# "2" "1" "g" "2" "1"
# "3" "1" "f" "1" "1"
# "3" "1" "g" "3" "1"
# for example I want to set f[2]=1 then I have to execute
# S[[3]][4]<-""; S[[3]][3]<-"";
# After I run Set2expr(S). It generate
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

