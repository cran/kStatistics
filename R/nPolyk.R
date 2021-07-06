nPolyk <-
function (L=NULL,data=NULL, bhelp=NULL) {
 if ( is.null(L) ) stop("The first parameter is missing");
 if ( is.null(data) ) stop("The second parameter is missing");
 if (is.null(bhelp) || bhelp!=TRUE) {bHelp<-FALSE} else {bHelp<-TRUE}

 if (typeof(L)=="list") {
    if (length(L)==1) {
       if (length(L[[1]])==1) { if (bHelp) cat("KS:");return(nKS((L[[1]]),data));}
       else {if (bHelp) cat("KM:");return(nKM(L[[1]],data));}
    }
    else
    { isPS<-TRUE; for (v in L) if (length(v)>1) isPS<-FALSE;
      if (isPS) { if (bHelp) cat("PS:");return(nPS((unlist(L)),data));}
         else   { if (bHelp) cat("PM:");return(nPM((L),data));}
    }    
    cat("Error: type of sub-function not determined\n\r");  
   }
 else {
    if (length(L)==1) { if (bHelp) cat("KS:");return(nKS((L),data));}
    else {if (bHelp) cat("KM:");return(nKM(L,data));}
 }

}
