nStirling2 <-
function (n, k) {
  if (n<0) stop("n < 0")
  if (k<0) stop("k < 0")
  if (k>n) stop("k > n")
  m<-0;
  for (j in 0:k) m<-m+(-1)^(k-j)*choose(k,j)*j^n;
  return(m/factorial(k)); 
}
