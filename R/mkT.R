mkT <-
  function(v=c(),n=0, vOutput=FALSE) {
    if (length(v)==0) stop ("The first parameter is empty");
    if (n==0) stop ("The second parameter must be a positive integer");
    u<-list();
    m<-mkmSet(v); 
    for (i in 1: length(m) ) {
      lm<-length(m[[i]][[1]]);
      if ((lm-n)==0) u[[i]]<-c( m[[i]][[1]] ); 
      if ( lm<n)     u[[i]]<-c( m[[i]][[1]], rep(list(c(rep(0,length(v)))),(n-lm)  ) ); 
    }
    
    U<-c();
    for (m in u) { if(!is.null(m) ) U<-c( U, nPerm(m));}
    U<-list2Set(U);
    if (!vOutput)  return(list2Set(U));
    for (mm in U) {cat("["); for (m in mm) {cat("(",m,")") };cat("]\n")} 
  }