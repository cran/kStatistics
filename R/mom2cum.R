mom2cum <-
function(n=0) {
  v<-MFB(n,1);                                # Recall MFB Function
  v<-MFB2Set( v );                            # Expression to vector 
  for (j in 1:length(v)) {
    # ----- read -----------[ fix block ]-----#---------------#
      c <- as.character(v[[j]][2]);           # coefficient
      x <-              v[[j]][3] ;           # variable
      i <-              v[[j]][4] ;           # subscript
      k <-       strtoi(v[[j]][5]);           # power
    # ----- change ---------------------------#---------------#
    if (x=="f") {
      x<-""; 
      i<-""; 
    } 
    else if (x=="g") {
      x<-"k";
    }
    # ----- write ---------[ fix block ]------#---------------#
    v[[j]][2] <- c;
    v[[j]][3] <- x;
    v[[j]][4] <- i;
    v[[j]][5] <- k;
    # ----------------------------------------#---------------#
  }
  Set2expr(v);
}
