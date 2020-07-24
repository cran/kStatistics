e_MFB <-
function(pv=c(), pn=0, pf=c(), pg=c(), b=FALSE) {
  
   if (length(pv)<1) stop("the first parameter must be a non-zero vector of integers");
   if (pn<1)  stop("the second parameter must be a positive integer")
   for (m in unlist(pv)) if (m<0) stop("the first parameter cannot contain negative values")
  
   vf<-c();vg<-c();
   l_MFB<-MFB(pv,pn);
   
   v<-unlist(strsplit(l_MFB,""));
   ls<-length(v);
   c_word<-"";
   i<-1;
   while (i<=ls) {       
     if (v[i]=="f") {  
        i<-i+1;
        c_word<-"f";
        repeat { 
        c_word<-paste0(c_word, v[i] ); 
        if(v[i]=="]") {break}
        i<-i+1; 
        }
        vf<-c(vf,c_word);
     }
     if (v[i]=="g") {  
       i<-i+1;
       c_word<-"g";
       repeat { 
         c_word<-paste0(c_word, v[i] ); 
         if(v[i]=="]") {break}
         i<-i+1;
       }
       vg<-c(vg,c_word);
     }
     i<-i+1;
   }
   
   vf<-sort(unlist(list2Set(c(vf))));
   vg<-sort(unlist(list2Set(c(vg))));
   
   s<-"";
   
    if (length(pf)==0) {
      s<-paste0("The third parameter must contain the ",length(vf)," values of f:");
      for (m in vf) s<-paste0(s," ",m);
      }
  
    if (length(pg)==0) if (typeof(pf)=="character") {
      
     s<-gsub(" ","",pf);
     s<-gsub(",g","~g",s);
     s<-gsub(",f","~f",s);
     s<-unlist(strsplit(s,"~"));
     vf<-c();
     vg<-c();
     for (m in s) {
       v<-unlist(strsplit(m,"="));
       v[1]<-gsub("\\[","\\\\[",v[1]);
       v[1]<-gsub("\\]","\\\\]",v[1]); 
       vf<-c( vf, c(v[[1]]),c(v[[2]]) );  
     }
     s<-"";
   }
   else {     
       s<-paste0(s,". The fourth parameter must contain the ",length(vg)," values of g:");
       for (m in vg) s<-paste0(s," ",m);
   }

   if (s!="") stop(s);
     
   if (b==TRUE) {
     s<-"";
     for (i in 1:length(vf)) s<-paste0(s,vf[[i]],"=",pf[[i]],", ");
     for (i in 1:length(vg)) s<-paste0(s, vg[[i]],"=",pg[[i]],", ");
     s<-noquote(substr(s,1,nchar(s)-2));
   }
   else {
     s<-gsub("f","*f",l_MFB);
     s<-gsub("\\+ \\*","\\+ ",s);
     if (substr(s,1,1)=="*") s<-substr(s,2,nchar(s));
     s<-gsub("g","*g",s);
     
     if ( (length(vf)>0) && (length(vg)==0)) {
       for (i in seq(1,length(vf)-1,2) ) s<-gsub(vf[i],vf[i+1],s);
       if (length( unlist(strsplit(s,"g")) ) >1 )  stop( paste("Alncuni parametri 'g' non sono stati valutati",s));
       if (length( unlist(strsplit(s,"f")) ) >1 )  stop( paste("Alncuni parametri 'f' non sono stati valutati",s));
       
       }
     else {
       vf<-gsub("\\[","\\\\[",vf);
       vf<-gsub("\\]","\\\\]",vf); 
       vg<-gsub("\\[","\\\\[",vg);
       vg<-gsub("\\]","\\\\]",vg); 
       for (i in 1:length(vf)) s<-gsub(vf[i],pf[[i]],s);
       for (i in 1:length(vg)) s<-gsub(vg[i],pg[[i]],s);
     }
     
     s<-eval(parse(text=s));
     
   }
   
   return(s);
}
