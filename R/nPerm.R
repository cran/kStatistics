nPerm <-
function(L=c()) {
  # SubFunction
  insc <- function (pv, c) {
    U<-c();
    for (v in pv) {
      u<-list(c(c,v));lv<-length(v);
      for (i in 1:(length(v)-1) ) {
        if ( toString(v[i])==toString(c)) break;
        u[[(i+1)]]<-c(v[1:i],c,v[(i+1):lv]) ;
      }
      if ( (toString(v[i+1])!=toString(c)) && (toString(v[i])!=toString(c)) ) 
        u[[lv+1]]<-c(v,c);
      U<-c(c(U, u));
    } 
    return(U);
  } 
  
  if (length(L)==0) stop("The vetor/list is empty");
  if (length(L)==1) return(L);
  if (toString(L[1])==toString(L[2])) {u<-list(c( L[1],L[2] ) );}
  else {u<-list2Set( list(c( L[1],L[2] ),c( L[2],L[1] ))  );}
  
  if (length(L)==2) return(u);
  
  for (i in 3:length(L)) u<-insc(u,L[i]);
  
  return(u);
}
