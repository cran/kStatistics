nPM <-
function( v = NULL, V = NULL) {
  if ( is.null(v) ) stop("The first parameter is missing");
  if ( is.null(V) ) stop("The second parameter is missing");
  if (typeof(v)!="list") stop("The fist parameter must by a list")
  if (typeof(V)!="list") stop("The second parameter must by a list")
  if (length(v[[1]])!=length(V[[1]])) stop("The dimension of the polykay indexes and the number of the columns of the data must be equal")
  if (sum(unlist(v))>length(V)) stop("The database must contain more data")
  for (i in unlist(v)) if (i < 0) 
       stop("The values cannot be negative");
  for (i in 2:length(V))  
      if (length(V[[i]])!=length(V[[1]])) stop("The arrays in the data set must have the same length")
  for (i in 2:length(v))  
      if (length(v[[i]])!=length(v[[1]])) stop("The arrays in the first parameter must have the same length")
  # - - - Start Sub Function  - - - - - - - - - - - - - - - - - - - - - - - - 
  ricalcMF <- function (M) {
      appM<-M;
      for (i in 1:length(appM) ) 
          appM[[i]][[2]]<-appM[[i]][[2]] * 
                        (-1)^(length(appM[[i]][[1]])-1) * 
                     factorial(length(appM[[i]][[1]])-1);
      return(appM);
  }
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  umSet <- function (pM=NULL) {
       if ( is.null(pM) ) stop("The parameter is missing");
       M<-pM; 
       for (i in 1:(length(M)-1) )
           for (j in (i+1):length(M) ) {
               if ( setequal( M[[i]][[1]], M[[j]][[1]]  ) & length(M[[i]][[1]])==length(M[[j]][[1]] ) )
                  { M[[i]][[2]]<-M[[i]][[2]]+M[[j]][[2]];
                    M[[j]][[2]]<-0;
           }
      }
      oM<-list();k<-0;
      for (i in 1:length(M)) if (M[[i]][[2]]!=0) {k<-k+1; oM[[k]]<-M[[i]]}
        return(oM);
  }
  # - - - End Sub Function  - - - - - - - - - - - - - - - - - - - - - - - - - 
  npk <-0;
  NN<-length(V);

  u<-c();
  for (x in v) {x<-x[x>0]; u<-c( u, length(x) );}  
  M<-max(u);

  u<-c();
  for (i in 1:length(v[[1]])) {
      su<-0; 
      for (j in 1:length(v))  {
           su<-su+v[[j]][[i]]
      }; 
      u<-c( u, su) 
  }  

  vTab<-mkmSet(u);

  N<-0;
  for (x in v)   N<-N+sum(x);


  
  #----- vS -----#
  vS <- list();k<-0;
  for (x in m2Set(vTab) ) {k<-k+1; vS[[k]]<-list( c(x), c(powS(x,V)) );} 
  
  #----- vK -----#
  vTabS<-vTab;
  for (i in 1:length(vTabS)) {
      pS<-1;  
      for (m in vTabS[[i]][[1]]) { pS<-pS*mCoeff( m, vS) }
      vTabS[[i]][[2]]<-vTabS[[i]][[2]]*pS;
  }
  #----- vKS -----#
  

  # --- Vettore vK ------------------

  vk_ptr<-list();
  for (i in 1:length(vS) ) vk_ptr[[i]]<-vS[[i]][[1]];
  
  U<-list();ptr_i<-0;
  for (m in vk_ptr) {
      ptr_i<-ptr_i+1;
      U[[ptr_i]]<-list( c(m), c( ricalcMF(mkmSet(c(m)))) );
  }

  u<-ricalcMF(mkmSet(v[[1]]));
  for (i in 2:length(v)) u<-mpCart( u, ricalcMF(mkmSet(v[[i]])) );
    
  u<-umSet(u);
  
  for (i in 1:length(u) )  u[[i]][[2]]<-u[[i]][[2]]/mCoeff(u[[i]][[1]], vTab)/ff(NN,length(u[[i]][[1]]));
    
  s<-0
  for (m in vTabS) {
      p<-m[[2]];
      if (length(m[[1]])==1) {
         Ue <- mCoeff(m[[1]][[1]], U);}
      else {
         m1<-mCoeff(m[[1]][[1]], U);
         m2<-mCoeff(m[[1]][[2]], U);
         if (length(m1)!=0 & length(m2)!=0) { 
         Ue <- mpCart (m1, m2);
           if (length(m[[1]])>2) 
              for (i in 3:length(m[[1]]))  Ue <- mpCart (Ue, mCoeff(m[[1]][[i]],U));
         }
      }

      for (i in 1:length(Ue)) Ue[[i]][[2]]<-Ue[[i]][[2]]*p;

      sUe<-0;
      for (m1 in Ue) {
           sUe<-sUe+mCoeff(m1[[1]],u)*m1[[2]];}

      s<-s+sUe;
  }
  return(s);
}
