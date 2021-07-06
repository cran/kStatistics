list2m <-
function (v=c(0)) {
      v_set<-list2Set(v);
      nv<-c();
      for (i in 1:length(v_set) ){
          k<-0; 
          for (j in 1:length(v) ) if ( is.element ( list(c(v_set[[i]])),list(c(v[[j]]))))  k<-k+1;
          if (length(nv) > 0)  nv<-c( nv, list( list(v_set[[i]],k )))
             else              nv<-c(     list( list(v_set[[i]],k )));
      }
  return(nv);
}
