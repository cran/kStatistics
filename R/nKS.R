nKS <-
function( v, V) {
  for (i in unlist(v)) if (i < 0) 
      stop("The values cannot be negative");
  for (i in 2:length(V))  
      if (length(V[[i]])!=length(V[[1]])) stop("The data arrays must have the same length")
  if (sum(v)>length(V)) stop("The database must contain more data")              
  npk <-0;
  N<-length(V);
  n<-sum(v);  
  vTab<-mkmSet(v);

  # --- Moments array ---- OK
  vx<-c();
  for (i in 1:n) vx <- c( vx, (-1)^(i-1)*factorial(i-1)/ff(N,i) );
  
  #---------------------------

  vk<-list();
  for (i in 1:n ) {
      u<-c(); 
      for (j in 1:i) {
          u<-c(u, nStirling2(i,j)*(-1)^(j-1)*factorial(j-1) ); 
      }
      vk[[i]]<-u;
   }
   
   
   vS <- c();
   for (i in 1:n) vS<-c( vS, powS(i,V) ); 
      
   s<-0;
   for (u in vTab ) {
      pk<-list();
      for (i in 1:length(u[[1]])) pk[[i]] <- vk[[ u[[1]][[i]] ]];
      r<-1;
      vm<-pPoly( pk );  # vettore momenti - polinomio in x  
      evm<-0; # evalueate vm
      for (i in 1:length(vm)) evm<-evm+vx[i]*vm[i]*u[[2]]; 
      
      pS<-1;
      for (s1 in u[[1]]) pS<-pS*vS[s1];

      s <- s + pS*evm;
  }
  return( s ); 
}
