m2Set <-
function (v) {
      nv<-list();k<-0;
      for (i in 1:length( v ) ) for (j in 1:length(v[[i]][[1]]))
          if (!is.element( list(c(v[[i]][[1]][[j]])), nv) ) 
             { k<-k+1; nv[[k]]<-v[[i]][[1]][[j]]; }
  return(nv);
}
