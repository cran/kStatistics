GCBellPol <-
function(nv=c(),m=1,b=FALSE) {
  v<-MFB(nv,m);                             # Call the MFB Function
  v<-MFB2Set( v );                          # Expression to vector
  for (j in 1:length(v)) {
    # ----- read -----------[ fix block ]---#---------------#
    c <- as.character(v[[j]][2]);           # coefficient
    x <-              v[[j]][3] ;           # variable
    i <-              v[[j]][4] ;           # subscript
    k <-       strtoi(v[[j]][5]);           # power
    if (x=="f") {
      vi<-unlist(strsplit(i,",",fixed=TRUE));
      if (length(vi)==1) {x<-paste0("(y",ifelse(i==1,")", paste0("^",i,")")) );}
      else {x<-""; 
           for (lvi in 1:length(vi)) 
               if (vi[lvi]!="0") x<-paste0(x,"(y",lvi,ifelse(vi[lvi]=="1","",paste0("^",vi[lvi])),")");
      }
      i<-"";
    }
    else if ( (m>1) && (b) )
         {x<-"g";
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
