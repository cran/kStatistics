pCart <-
function (L) {
     pCartAB <- function (v1, v2) {
          v12<-list(); p12<-0;
          for (i in 1:length(v1) ) {
               for (j in 1:length(v2) ){
                   p12<-p12+1;
                   if (typeof(v1[[i]])=="list") 
                      { v12[[p12]]<-list(); 
                        for (k in 1:length(v1[[i]])) 
                             v12[[p12]][k] <- v1[[i]][k]; 
                        v12[[p12]][k+1] <- v2[j] ;
                      }
                   else
                      { v12[[p12]] <- c( c(v1[i]), c(v2[j]) ); }
         }}
       return(v12);
     } 
     vL<-list(); pL<-0;
     if (length(L)==1) return(L);
     vL<-pCartAB(L[[1]],L[[2]]);
     if (length(L) < 3) return(vL);
     for (i in 3:length(L) ) {
          vL<-pCartAB(vL, L[[i]]);
     }
 return(vL);
}
