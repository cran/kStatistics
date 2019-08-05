umSet <-
function (pM) {
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
