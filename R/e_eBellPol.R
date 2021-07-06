e_eBellPol <-
function(n=1,m=0,v=c(rep(1,n))) {
    if (length(v) < n) stop("The length of the data vector is less than ",n);    
    s<-eBellPol(n,m);
    s<-gsub(" ","",s);
    if ( substr(s,1,1)=="(" ) s<-paste0("1",s);
    s<-gsub("\\(", "\\*",s);
    s<-gsub("\\)", "",s);
    for (i in n:1) s<-gsub( paste0("y",i), paste0("\\(",v[i],"\\)"),s);
    s<-gsub(" ","",s);
    s<-gsub("\\+\\*","\\+",s);
    eval(parse(text=s));
}

