countP <-
function (v) {
  for (i in unlist(v)) if (i < 0) stop("The values cannot be negative");
  if (typeof(v)=="list") {
      n<-1; u<-c();
      for (j in 1:length(v[[1]])){
            s<-0;p<-1;
            for (i in 1:length(v))
               { m<-v[[i]][[j]];
                 u<-c( u, m);
                 p<-p*factorial(m);
                 s<-s+m; 
               } 
           n<-n*factorial(s)/p;
         } 
  }  
  else { p<-1; for (x in v) p<-p*factorial(x);
         n<-factorial( sum(v) )/p;  }

  # product of factorial of multiplicity
  u<-list2m(v); np<-1;
  for (i in 1:length(u)) np<-np*factorial(u[[i]][[2]]);
  
  return(n/np); 
}
