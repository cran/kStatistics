MFB2Set <-
function(sExpr) {
  if (nchar(sExpr)==0) stop("The first parameter cannot be empty");
  l_MFB<-sExpr;
  l_MFB<-gsub(" ","",l_MFB);
  v_MFB<-unlist(strsplit(l_MFB,"+",fixed=TRUE));
  k<-0;v<-list();
  for (i in 1:length(v_MFB)) {
    u<-unlist(strsplit(v_MFB[i],"g",fixed=TRUE));
    u_f<-unlist(strsplit(u[1],"f",fixed=TRUE));
    k<-k+1;
    v[[k]]<-c(i,ifelse( u_f[1]=="",1,u_f[1]),"f",u_f[2],1);
    v[[k]]<-gsub("\\[","",v[[k]]);v[[k]]<-gsub("\\]","",v[[k]]);
    for (j in 2:length(u)) {
      k<-k+1;
      u_g<-unlist(strsplit(u[j],"[",fixed=TRUE));
      v[[k]]<-c(i,1,paste0("g",u_g[1]));
      u_g[2]<-gsub("\\]","",u_g[2]);
      u_g<-unlist(strsplit(u_g[2],"^",fixed=TRUE));
      v[[k]]<-c(unlist(v[[k]]),u_g[1],ifelse(length(u_g)==1,1,u_g[2]));
    }
  }
  return(v);
}
