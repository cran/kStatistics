mpCart <-
function (M1, M2) {
     M12<-list();k<-0;
     for (i in 1:length(M1) ) {
          for (j in 1:length(M2) ){
              k<-k+1;M12[[k]]<-list(); 
              M12[[k]][[1]] <- c( M1[[i]][[1]], M2[[j]][[1]] );
              M12[[k]][2]   <- M1[[i]][[2]]*M2[[j]][[2]] ;
    }}
  return(M12);
}
