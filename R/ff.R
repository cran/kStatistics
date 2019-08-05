ff <-
function (n,k) {
  if (n==0) return(1)
  if (k>n) stop("k > n")
  if (n<0) stop("n < 0")
  nff<-1;
  for (i in 0:(k-1) ) nff<-nff*(n-i);
  return(nff);       
}
