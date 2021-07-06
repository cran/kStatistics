nKM <-
function( v = NULL, V = NULL) {
  if ( is.null(v) ) stop("The first parameter is missing");
  if ( is.null(V) ) stop("The second parameter is missing");
  if (sum(unlist(v))>length(V))  stop("The database must contain more data")
  for (i in unlist(v)) if (i < 0) 
       stop("The values cannot be negative");
  for (i in 2:length(V))  
      if (length(V[[i]])!=length(V[[1]])) stop("The arrays in the data set must have the same length")
  if (length(v)!=length(V[[1]])) stop("The first parameter and the arrays in the data set must have the same length")
  npk <-0;
  N<-length(V);
  n<-sum(v);  
  vTabS<-mkmSet(v);

  vTabK<-list(); k<-0;
  for (u1 in vTabS) {
      k<-k+1;
      app<-c();
      for ( u2 in u1[[1]] ) app <- c( app, sum(u2));
      vTabK[[k]]<-list( app, c(u1[[2]]));
  }
  
  # --- moments array ---- 
  vx<-c();
  for (i in 1:n) vx <- c( vx, (-1)^(i-1)*factorial(i-1)/ff(N,i) );
  
  # --- vK array ----------
  vk<-list();
  for (i in 1:n ) {
      u<-c(); 
      for (j in 1:i) {
          u<-c(u, nStirling2(i,j)*(-1)^(j-1)*factorial(j-1) ); 
      }
      vk[[i]]<-u;
   }
   
   
   #----- vS -----#
   vS <- list();k<-0;
   for (u in m2Set(vTabS) ) {k<-k+1; vS[[k]]<-list( c(u), c(powS(u,V)) );} 
  
   #----- vK -----#
  
   s<-0;
   for (j in 1:length(vTabK) ) {
       u<-vTabK[[j]]; 
       pk<-list();
       for (i in 1:length(u[[1]])) pk[[i]] <- vk[[ u[[1]][[i]] ]];
       r<-1;
       vm<-pPoly( pk );  # vector of moments - polynomial in x  
       evm<-0; # evalueate vm
       for (i in 1:length(vm)) evm<-evm+vx[i]*vm[i]*u[[2]]; 
       
       pS<-1;
       for (s1 in vTabS[[j]][[1]]) pS<-pS * mCoeff( s1, vS);
 
       s <- s + pS*evm;
   }
 
  return( s );

}
