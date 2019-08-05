mkmSet <-
function (vPar) {
  for (i in unlist(vPar)) if (i < 0) 
      stop("The values cannot be negative");
  #======================#
  #  START SUB FUNCTIONS # 
  #======================#
  URv <- function (Mp, n) {
     Mr<-list();
     L=length(Mp[[1]]);
     for (i in length(Mp):1) {if(n <= Mp[[i]][L]) break;}
     ptr<-i; ou<-c(-1);
     for (i in ptr:length(Mp)) {
          if (!identical(ou,Mp[[i]][1:L-1]) & n > Mp[[i]][L] & Mp[[i]][L]==0) {
              Ms<-Mp; 
              Ms[[i]][L]=n;
              ou<-Ms[[i]][1:L-1];
              Mr[[length(Mr)+1]]<-Ms;     
            }
     } 
     if (n==0) { Mr[[length(Mr)+1]]<-c(Mp) ;}
     else      { Mr[[length(Mr)+1]]<-c(Mp, list(c(rep(0,L-1),n))) ;}
     return( Mr );
  } 
  URV <- function (M1, M2) {
    Mr=c();
    M<-list(c(M1));
    for (i in 1:length(M2) ){
         for (j in 1:length(M)) {   
             Mr=c( Mr, URv( M[[j]], M2[[i]] ) );
         }
         M<-Mr;
         Mr<-c();
    }
    return( M ); 
  } 
  aDim <- function (M) {
       Mr<-list();
       for (i in 1:length(M) ) {
           Mr[[i]]=c(M[[i]],c(0)); 
       }
       return( Mr );
  } 
  #======================#
  #  END SUB FUNCTIONS   # 
  #======================#
     
  U<-list();
  v<-vPar;
  if (sum(v)==0) return(c(0)); 
  
  if (length(v)==1) {
     s<-sum(v);u<-intPart(s);
     for (i in 1:length(u) )  
         U[[i]]<-c(c(u[i]),countP(u[[i]]) );
         return(U);}
  

  vHead<-c();
  for (i in 1:length(v))
      if (v[i]==0) {vHead[i]<-0;}
      else         {v<-v[i:length(v)];break }

  if (length(v)==1) {
     s<-sum(v);u<-intPart(s);
     for (i in 1:length(u) ) {
         u1<-list();
         for (j in 1:length(u[[i]]) ) {
             u1[[j]]<-c( vHead, u[[i]][[j]] ); 
         }
         U[[i]]<-list( c(u1), countP(u[[i]]) );
     }
     return(U);
  }

  s1<-v[1];
  u1<-intPart(s1);

  for (ik in 2:length(v)){
  s2<-v[ik];
  if (s2==0) {u2<-c(0);}
  else       {u2<-intPart(s2);}
  u<-list();
  for (i in 1:length(u1)) u[[i]]<-aDim( u1[[i]] );
  k<-0;
  for (i in 1:length(u )) 
      for (j in 1:length(u2) ) { 
           for (ui in URV(u[[i]], u2[[j]])){ 
                k<-k+1; U[[k]]<-ui;   
          }
      }
  u1<-U;  
  }
  u<-list();
  for (i in 1:length(U)) u[[i]]<-list( U[[i]], countP(U[[i]]) );
  if (length(vHead)>0) { 
     for (i in 1:length(u) ) for (j in 1:length(u[[i]][[1]])) u[[i]][[1]][[j]] <-c( vHead, u[[i]][[1]][[j]] );
  }
 
  return( u );  
}
