powS <-
function (vn=NULL,lvd=NULL) {
  if ( is.null(vn) )  stop("The first parameter is missing");
  if ( is.null(lvd) ) stop("The second parameter is missing");

  for (i in 1:length(vn) ) if (vn[[i]]<0) 
      stop("The values of the first parameter cannot be negative")
  
  for (i in 1:length(lvd) ) if (length(vn)!=length(lvd[[i]])) 
      stop("All the vectors in the list must have the same size as the first parameter");
  n<-0;
  for (i in 1:length(lvd) ) { 
      p<-1;
      for (j in 1:length(vn) ) p<-p*lvd[[i]][j]^vn[j];
      n<-n+p;
  } 
  return(n);       
}
