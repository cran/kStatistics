Set2expr <-
function(v=c()) {
 u<-list(); U<-list();
 nv<-length(v)
 
 #-- make a list() -------------------
 for (i in 2:nv) {
   u<-c(u, list(v[[i-1]]) )
   if ( v[[i]][1]!=v[[i-1]][1] ) {
        U<-c(U, list(u)); u<-list();
   }
 }
 u<-c(u, list(v[[nv]]) )
 U<-c(U, list(u)); 
 #------------------------------------
 S<-list();
 for (m in U) {
   lm<-length(m);
   #internal sort 
   for (i in 1:(lm-1) ) {
      for (j in (i+1):lm ) { 
       if( (m[[i]][3]==m[[j]][3]) && (m[[i]][4]>m[[j]][4]) )
         { app_m<- m[[i]];m[[i]]<-m[[j]];m[[j]]<-app_m  }  
     }
   }
   #------------------------------------
   c<-"1"; # coeff
   for (i in 1:lm ) {
     c<-paste0(c,"*",m[[i]][2]);
     m[[i]][2]<-1;
   }
   c<-eval(parse(text=c));
   #------------------------------------
   s<-"";
   for (i in 1:(lm-1) ) {
     k<-strtoi(m[[i]][5]);
     for (j in (i+1):lm ) { 
        if( (m[[i]][3]==m[[j]][3]) && (m[[i]][4]==m[[j]][4]) && (m[[j]][3]!="") )
        { k<-k+strtoi(m[[j]][5]); m[[j]][3]<-""}  
     }
     s<-paste0(s,m[[i]][3],ifelse(m[[i]][4]!="",paste0("[",m[[i]][4],"]"),""),ifelse(k>1,paste0("^",k),""));
   }
   if (m[[lm]][3]!="")  {k<-strtoi(m[[lm]][5]); s<-paste0(s,m[[lm]][3],ifelse(m[[lm]][4]!="",paste0("[",m[[lm]][4],"]"),""),ifelse(k>1,paste0("^",k),""));}
   S<-c(S, list(c(c,s)));
 }
 
 # compact monomial
 V<-"";
 if (length(S)>1) {
    for (i in 1:(length(S)-1)){
        if (S[[i]][2]!="") { 
            c<-S[[i]][1];
            for (j in (i+1):length(S)){
                if (( S[[i]][2]==S[[j]][2]) && (S[[j]][2]!="")) {
                    c<-paste0(c,"+",S[[j]][1]);
                    S[[j]][2]<-"";
                }
            }
            c<-eval(parse(text=c));
            signc<-ifelse(c>0," + "," - ");
            c<-ifelse( (abs(c)==1),"",abs(c)) ;
            if (c!=0) V<-paste0(ifelse(nchar(V)>0,paste0(V,signc),""),c,S[[i]][2]);
        }
    }
 }
 i<-length(S);
 c<-S[[i]][1];
 c<-eval(parse(text=c));
 signc<-ifelse(c>0," + "," - ");
 c<-ifelse( (abs(c)==1),"",abs(c)) ;
 if ((c!=0) && (S[[i]][2]!="")) V<-paste0(ifelse(nchar(V)>0,paste0(V,signc),""),c,S[[i]][2]);
 noquote(V);
}
