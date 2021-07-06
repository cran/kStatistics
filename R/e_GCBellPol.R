e_GCBellPol <-
function(pv=c(), pn=0, pyc=c(), pc=c(), b=FALSE) {
  
  if (length(pv)<1) stop("The first parameter must be a non-zero vector of integers");
  if (pn<1)  stop("The second parameter must be a positive integer")
  for (m in unlist(pv)) if (m<0) stop("The first parameter cannot contain negative values");
  if (typeof(pyc)=="character") {if ((length(pc)!=0) || (b==TRUE)) stop (pyc)};
  vyc<-c();vc<-c();
  l_MFB<-GCBellPol(pv,pn);
  l_MFB<-gsub("\\(","",l_MFB);
  l_MFB<-gsub("\\)","",l_MFB);
  
  v<-unlist(strsplit(l_MFB,""));
  ls<-length(v);
  c_word<-"";
  i<-1;
  while (i<=ls) {       
    if (v[i]=="g") {  
      i<-i+1;
      c_word<-"g";
      repeat { 
        c_word<-paste0(c_word, v[i] ); 
        if(v[i]=="]") {break}
        i<-i+1;
      }
      vc<-c(vc,c_word);
    }
    i<-i+1;
  }
  
  vyc<-c();  
  if (pn>1) {for (i in 1:pn) vyc<-c(vyc, paste0("y",i));}
  else {vyc<-c("y");}
  
  vc<-sort(unlist(list2Set(c(vc))));
  
  s<-"";
  
  if (typeof(pyc)!="character") if ((length(pyc)!=pn) && (length(pyc)>0)) stop("The length of the vector in the third parameter must be equal to the value of the second parameter");
    
  
  if ((length(pyc)==0) && (length(pc)==0) ) {
     if (pn==1) {s<-paste0("The third parameter must contain the value of");}
     else       {s<-paste0("The third parameter must contain the ",length(vyc)," values of y:");}
     for (m in vyc) s<-paste0(s," ",m);
  }
  
  if (length(pc)==0) if (typeof(pyc)=="character") {
     s<-gsub(" ","",pyc);
     s<-gsub(",g","~g",s);
     s<-gsub(",y","~y",s);
     s<-unlist(strsplit(s,"~"));
     vyc<-c();
     vc<-c();
     for (m in s) {
       v<-unlist(strsplit(m,"="));
       v[1]<-gsub("\\[","\\\\[",v[1]);
       v[1]<-gsub("\\]","\\\\]",v[1]); 
       vyc<-c( vyc, c(v[[1]]),c(v[[2]]) );  
     }
     s<-"";
  }
  else {     
     s<-paste0(s,".\n The fourth parameter must contain the ",length(vc)," values of g:");
     for (m in vc) s<-paste0(s," ",m);
  }
  
  if (s!="") stop(s);
  if ((b==TRUE) && (length(vc)==length(pc)) ){
    s<-"";
    for (i in 1:length(vyc)) s<-paste0(s,vyc[[i]],"=",pyc[[i]],", ");
    for (i in 1:length(vc)) s<-paste0(s,vc[[i]],"=",pc[[i]],", ");
    s<-noquote(substr(s,1,nchar(s)-2));
  }
  else {
    s<-gsub("y","*y",l_MFB);
    s<-gsub("\\+ \\*","\\+ ",s);
    if (substr(s,1,1)=="*") s<-substr(s,2,nchar(s));
    s<-gsub("g","*g",s);

    if ( (length(vyc)>0) && (length(vc)==0)) {
       for (i in seq(1,(length(vyc)-1),2) ) s<-gsub(vyc[i],vyc[i+1],s);
    }
    else {
      if (length(vc)!=length(pc)) stop("The vector of the \"g\" values must have ", length(vc)," elements.");
      vc<-gsub("\\[","\\\\[",vc);
      vc<-gsub("\\]","\\\\]",vc); 
      for (i in 1:length(vc)) s<-gsub(vc[i],pc[[i]],s);
      if ( (typeof(pyc)!="character") && (length(pyc)>0)) for (i in 1:length(vyc)) s<-gsub(vyc[i],pyc[[i]],s);
    }

    if (length( unlist(strsplit(s,"g")) ) >1 )  stop( paste("Some \"g\" values have not been assigned:",s));

    if (length( unlist(strsplit(s,"y")) ) == 1 )  
    {s<-eval(parse(text=s));}
    else {
       v<-unlist(strsplit(s,"+",fixed=TRUE));
       vs<-c();
       for (m1 in v){
         m2<-unlist(strsplit(m1,"*", fixed=TRUE));
         s1<-"";s2<-""
         for (m3 in m2)  {
             if (gsub("y","",m3)==m3) {s2<-paste0(s2,"*",m3)}
             else {s1<-paste0(s1,"(",m3,")")}
         }
         s2<-substr(s2,2,nchar(s2))
         vs<-c(vs, eval(parse(text=s2)), gsub(" ","",s1));
       }
       for (i in seq(1,(length(vs)-2),2) ) {
           if (vs[i+1]!="") {
              for (j in seq(i+2,length(vs),2) ) {
                   if (vs[i+1]==vs[j+1]) {
                       vs[i]<-(as.numeric(vs[i])+as.numeric(vs[j]));
                       vs[j+1]<-"";
                   }  
               }
            }
       }
       s<-"";
       for (i in seq(1,length(vs)-1,2) )  {
            if (vs[i+1]!="") s<-paste0(s," + ",ifelse(vs[i]==1,"",vs[i]),vs[i+1]);
       }
       s<-noquote(substr(s,4,nchar(s)));    
    }
    
  }
  s<-gsub(" + -"," - ",s,fixed=TRUE);
  return(noquote(s));
}
