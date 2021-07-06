nPS <-
function( v = NULL, V = NULL) {
  if ( is.null(v) ) stop("The first parameter is missing");
  if ( is.null(V) ) stop("The second parameter is missing");
  if (sum(v)>length(V)) stop("The database must contain more data")
  for (i in unlist(v)) if (i < 0) 
       stop("The values cannot be negative");
  for (i in 2:length(V))  
      if (length(V[[i]])!=length(V[[1]])) stop("The data arrays must have the same length")
  # - - - Start Sub Function  - - - - - - - - - - - - - - - - - - - - - - - -
  kProd<- function (u,v) {
     uv<-list(); euv<-c(); k<-0;
     if (typeof(u)=="list") {  
        for (i in 1:length(u)) for (j in 1:length(v)) {
             k<-k+1; uv[[k]]<-sort(c(u[[i]],v[[j]] ));}
        }  else  {
        for (i in 1:length(u)) for (j in 1:length(v)) {
            k<-k+1; uv[[k]]<-sort(c(u[[i]]*v[[j]] )) ;}
        }
     return(uv);
  }
  # - - - End Sub Function  - - - - - - - - - - - - - - - - - - - - - - - - -
  npk <-0;
  N<-length(V);
  n<-sum(v);

  vTab<-mkmSet(sum(v));

  #  moment vectors ---- 
  vP<-list();
  for (i in 1:length(v) ) vP[[i]]<-intPart(v[i]);
  vP<-pCart(vP);
  vMu<-list();
  for (i in 1:length(vP)) {
      p<-1;sdim<-0
      for (v in vP[[i]]) {
           p<-p*(-1)^(length(v)-1)*factorial(length(v)-1)*countP(v); 
           sdim<-sdim+length(v)}

      v<-sort(c(unlist(vP[[i]])));
      p<-p/ff(N,sdim)/countP(v); 
      vMu[[i]] <- list( v , c(p) );
  }
  vApp<-vTab;
  for (i in 1:length(vApp)) { 
      vApp[[i]][[2]]<-0;
      for (j in 1:length(vMu)){
          if (identical(vApp[[i]][[1]],vMu[[j]][[1]])) vApp[[i]][[2]]<-vApp[[i]][[2]]+vMu[[j]][[2]];
      }
   }
   vMu<-vApp;
   
   vk<-list();
   for (i in 1:n) vk[[i]]<-intPart(i);
   evk<-vk;
   for (i in 1:length(vk) ) 
       for (j in 1:length(vk[[i]]))
            evk[[i]][[j]]<-(-1)^(length(vk[[i]][[j]])-1)*factorial(length(vk[[i]][[j]])-1)*countP(vk[[i]][[j]])*countP(sum(vk[[i]][[j]]));
   
    
    vS <- c();
    for (i in 1:n) vS<-c( vS, powS(i,V) ); 
    
    
    s<-0;
    for (u in vTab ) {
       app_vk<-c();app_evk<-c(); 
       if (length(u[[1]])==1) {
           app_vk  <-  vk[[ u[[1]] ]];
           app_evk <- c(unlist(evk[[ u[[1]] ]])) * u[[2]];
           } 
       else { 
           app_vk <- kProd( vk[[ u[[1]][[1]] ]],  vk[[ u[[1]][[2]] ]] );
           app_evk<- kProd(c(unlist(evk[  u[[1]][[1]]  ])), c(unlist(evk[[  u[[1]][[2]] ]])) );
           app_evk <-c(unlist(app_evk)) * u[[2]];
           
           if (length(u[[1]])>2) {
               for (i in 3:length(u[[1]])) {  
                   app_vk <- kProd(app_vk,  vk[[ u[[1]][[i]] ]] );
                   app_evk<- kProd(c(unlist(app_evk)), c(unlist(evk[[ u[[1]][[i]] ]] )));
               }
           }
       }
   
       pS<-1;
       for (i in u[[1]]) pS<-pS*vS[i];
       
       sC<-0; 
       for (i in 1:length(app_vk) ) sC <- sC + mCoeff(app_vk[[i]], vMu)*app_evk[[i]];
       
       s <- s + sC*pS;
 }
 return( s ); 
}
