list2Set <-
function (v=c(0)) {
      nv<-list( c(v[[1]]) );
      if (length(v) > 1 ) for (i in 2:length(v) ) {
          if (!is.element( list(c(v[[i]])), nv)) nv<-c( nv, list(v[[i]]));
      }
  return(nv);
}
