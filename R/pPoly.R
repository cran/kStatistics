pPoly <-
function( L ) {
  nL<-length(L);
  if (nL==1) return(L[[1]]);

  vdim <- 0; for (i in 1:nL) vdim<-vdim+length(L[[i]]);

  uv<-c(rep(0,vdim));
  u<-L[[1]];   
  for (k in 2:nL) {
      v<-L[[k]];
      for (i in 1:length(u)) 
          for (j in 1:length(v)) 
               uv[i+j] <- uv[i+j]+u[i]*v[j];
       u<-uv[uv!=0];
       uv<-c(rep(0,vdim));
  }
  return( c(rep(0, vdim-length(u)),u));
}
