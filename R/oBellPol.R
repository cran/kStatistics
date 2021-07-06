oBellPol <-
  function(n=1,m=0) {
    v<-MFB(n,1);                              # Call the MFB Function
    v<-MFB2Set( v );                          # Expression to vector
    for (j in 1:length(v)) {
      # ----- read -----------[ fix block ]---#---------------#
      c <- as.character(v[[j]][2]);           # coefficient
      x <-              v[[j]][3] ;           # variable
      i <-              v[[j]][4] ;           # subscript
      k <-       strtoi(v[[j]][5]);           # power
      # ----- change -------------------------#---------------#
      if (x=="f") {
        if ((m>0) && (i!=as.character(m))) {c<-"0";}
           else {c<-paste0(c,"*(factorial(",i,"))");}
        x<-""; 
        i<-""; 
      } 
      else if (x=="g") {
        c<-paste0(c,"*(factorial(",i,"))^",k );
        x<-paste0("(y",i,ifelse(k>1,paste0("^",k),""),")");
        i<-"";k<-1;
      }
      
      # ----- write ---------[ fix block ]------#---------------#
      v[[j]][2] <- c;
      v[[j]][3] <- x;
      v[[j]][4] <- i;
      v[[j]][5] <- k;
      # ----------------------------------------#---------------#
    }
    if (n==1) return(Set2expr(v));
    noquote(paste0("1/",factorial(n),"( ",Set2expr(v), " )"));
}
