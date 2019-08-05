intPart <-
function (n) {
  if (n<1) stop("n must be > 0");
  #-------------------- 
  # Start Sub Function
  #-------------------- 
  f_intPart <-  function (n1,n2) {
    if (n1 == 0) return( c() );
    if (n2 == 1) return( list(c(rep(1,times=n1))) );
    vP <- c();
    for (i in 1:n2 ) {
       v <- f_intPart(n1-i,min(n1-i,i))  ;
       if (length(v) == 0) { v<-list(c(n1)); }
       else {
       for (j in 1:length(v) ) {
           v[[j]] <- c( v[[j]] , i );
        }}
       vP <- c( vP ,v );
       } 
    return(vP);
  }
  #-------------------- 
  # End Sub Function
  #-------------------- 
  f_intPart(n,n) 
}
