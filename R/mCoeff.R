mCoeff <-
function (v=NULL,L=NULL) {
 if ( is.null(v) ) stop("The first parameter is missing")
 if ( is.null(L) ) stop("The second parameter is missing")    
 if (typeof(v)=="list") {
     for (u in L) { 
         if ( setequal( v ,u[[1]]  ) & length(v)==length(u[[1]]) ) return(u[[2]]);
    }
 }
 else {
     for (u in L ) {
 if (identical(as.double(v),as.double(c(u[[1]])))) return(u[[2]])};
 }
 return(0);
}
