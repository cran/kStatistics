MFB <-
function(v=c(), n=0) {
      #----------------------------------------------------------------------------
      # Sub Function um_MFB
      #----------------------------------------------------------------------------
      #    um_MFB - Faa di Bruno's formula - univariate with univariate case and  
      #                                      univariate with multivariate case
      #    Diff_f(g(x))  or Diff_f(g(x1,x2,...))  
      # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      # Input.......: vector
      # Output......: string 
      # Example.....: um_MFB( c(2) )    -> f[2]g[1]^2 + f[1]g[2]
      #                                  it is result for (d^2/dx^2)( f(g(x)) )
      # Example.....: um_MFB( c(1,1) )   -> f[1]g[1,1] + f[2]g[1,0]g[0,1] 
      #                  it is result for (d^2/dxdy)( f(g(x,y)) )
      # Example.....: um_MFB( c(1,1) )   -> f[1]g[1,1] + f[2]g[1,0]g[0,1] 
      #                  it is result for (d^2/dxdy)( f(g(x,y)) )
      # Called by...: MFB 
      #----------------------------------------------------------------------------
      um_MFB <- function (v) {
      s<-0;
      for (m in unlist(v)) s<-s+m; 
      if (s==0) return(1);
      s<-'';
      for (m in mkmSet(v)) { 
          s<-paste0(s,{if( (m[[2]]) >1) paste0("(",m[[2]],")");},'f[',length(m[[1]]),']');
          for (r in list2m(m[[1]]) ){ 
              s<-paste0(s,'g[',paste(r[[1]],collapse=","),']',{if(r[2]>1) paste0('^',r[2]);});
              }
          s<-paste0(s,' + '); 
          };
      return( noquote( substr(s,1,nchar(s)-2)) );
      }
      #
      # Example
      # um_MFB( c(0,1) );
      #
      #----------------------------#
      # SubFunction gval           #
      # evaluate g function        #
      #----------------------------#
      gval <- function(s,n0) {
        v<-unlist(strsplit(s,""));
        ls<-length(v);
        c_word<-"";c_rest<-"";
        n<-1;i<-1;
        nv<-c( rep(0,n0) );
        while (i<=ls) {       
              if (v[i]=="(") 
               {  i<-i+1;
                  c_word<-"";
                  repeat { 
                         c_word<-paste0(c_word, v[i] ); 
                         i<-i+1; 
                         if(v[i]==")") {n<-n*strtoi(c_word);c_word<-"";i<-i+1;break}
                  }
               }
               else if (v[i]=="f") 
               {   i<-i+1;
                   c_word<-"";
                   repeat { 
                          c_word<-paste0(c_word, v[i] ); 
                          i<-i+1; 
                          if(v[i]=="[") {p<-strtoi(c_word);c_word<-"";i<-i+1;break}
                   }
                   repeat { 
                          c_word<-paste0(c_word, v[i] ); 
                          i<-i+1; 
                          if(v[i]=="]") {nv[p]<-strtoi(c_word);c_word<-"";i<-i+1;break}
                   }
              }  
              else 
              {c_rest<-paste0(c_rest, v[i]);
               i<-i+1;
              }
        }
        f_out<-"";
        for (m in nv) f_out<-paste0( ifelse(f_out=="","f[", paste0(f_out,",")),m);
        f_out<-paste0(f_out,"]"); 
        return( c(n,f_out,c_rest) );
      }
      #
      # Example
      # gval( "(12)(2)f1[2]g1[1,1]g1[1,0]^2(2)f2[2]g2[1,1]g2[1,0]" );
      #----------------------------#
      # SubFunction gf             #
      # recall gval function       #
      #----------------------------#
      gf <- function(ps,n) {
         s<-gval(ps,n)
         noquote(paste0(ifelse(s[1]>1,paste0("(",s[1],")"),""),s[2],s[3]));
      }
      #
      # Example
      # gf("(12)(2)f2[3]g1[1,1]g1[1,0]^2(2)f1[2]g2[1,1]g2[1,0]");
      # gf("f1[1]g1[1,1]",2)
      #----------------------------#
      # SubFunction JOINT          #
      #----------------------------#
      joint <- function(v) {
            n<-length(v);
            u<-list(); 
            #---------------- fattore moltiplicativo -------------
            p<-1;
            for (i in 1:length(v[[1]])) {a<-0; for (m in v) a<-a+m[[i]]; p<-p*factorial(a);}  
            for (m in c(unlist(v)) ) p<-p/factorial(m);
            #----------------------------------------------------
            for (i in 1:length(v)) {
            df<-um_MFB(v[[i]]);
            if (df==1)
               {u[[i]]<-"";}
            else
               {u[[i]]<-unlist(strsplit(gsub("f",paste0("f",i), gsub( "g",paste0("g",i), df)),"+",fixed=TRUE));}
            } 
            u1<-list();
            for (i in 1:length(u) ) {
                u2<-list(); 
                for (j in 1:length(u[[i]]) ) u2[[j]]<-gsub(" ", "",u[[i]][[j]]) ;
            u1[[i]]<-u2;
            }
            u2<-pCart(u1);
            u1<-list();
            for (i in 1:length(u2) ) 
                u1[[i]]<-gf(paste0(ifelse(p>1,paste0( "(",p,")"),""),paste0(u2[[i]], collapse="", sep="")),n);
          
            u1<-paste0(u1, collapse="", sep=" + "); 
            u1<-substr(u1,1,nchar(u1)-3 );
      return( noquote( u1 ) );   
      }
      #
      # Example
      # um_MFB( c(1,0) );
      # um_MFB( c(0,1) );
      # joint ( list( c(1,1), c(0,0) ) );
      # for (m in unlist(strsplit( s, " + ", fixed=TRUE)) ) cat( m,"\n" );
      #============================#
      # Master Function            #
      #============================#      
      if (length(v)<1) stop("the first parameter must be a non-zero vector of integers");
      if (n<1)  stop("the second parameter must be a positive integer")
      for (m in unlist(v)) if (m<0) stop("the first parameter cannot contain negative values")
      if (n==1) {u<-um_MFB(v);}
      else {u<-list();M<-mkT(v,n);  
            for (i in 1:length(M) ) u[[i]]<-joint( M[[i]] )
            u<-paste0(u, collapse="", sep=" + ");
            u<-substr(u,1,nchar(u)-3 );
      }
      u<-gsub( "\\)","",u );
      u<-gsub( "\\(","",u );
 return( noquote( u ) );
}
