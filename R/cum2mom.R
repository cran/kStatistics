cum2mom <-
function(n=1) {
  v<-MFB(n,1);                                  # Call the MFB Function
  v<-MFB2Set( v );                              # Expression to vector 
  for (j in 1:length(v)) {
      # ----- read -----------[ fix block ]-----#---------------#
        c <- as.character(v[[j]][2]);           # coefficient
        x <-              v[[j]][3] ;           # variable
        i <-              v[[j]][4] ;           # subscript
        k <-       strtoi(v[[j]][5]);           # power
      # ----- change ---------------------------#---------------#
      if (x=="f") {
          i<-strtoi(i);                         # as integer
          c<-paste0(c,"*(-1)^(",i-1,")*factorial(",i-1,")" );
          x<-""; 
          i<-""; 
      } 
      else if (x=="g") {
          x<-"m";
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
