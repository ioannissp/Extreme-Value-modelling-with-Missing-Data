##### Simulation Function #####
simulate<-function(runs){ #ignoring the missingness in the data
  res<-array(NA, dim=c(3,runs, 3))
  dimnames(res)<-list(c("b0", "b1","xi"), as.character(1:runs), c("estimate", "2.5 %","97.5 %"))
  fails<-0
  for(run in 1:(runs)){
    
    l<-make_data(run=run)
    u<-as.numeric(l[3])
    
    data_sim<-data.frame(l$x,l$y)
    colnames(data_sim)[1]<-"x"
    colnames(data_sim)[2]<-"y"
    data_sim<-make_missing(data_sim, run=run)
    data_sim<-na.omit(data_sim)
    n<-nrow(data_sim)
    result<-try({ 
      model<-gpd.fit(data_sim$y, threshold = u, ydat=matrix(data_sim$x, ncol=1), sigl=1, siglink=exp)
      q<-model$mle
      t<-model$se
      low1<-q[1]-qt(0.975, n-3)*t[1]
      up1<-q[1]+qt(0.975, n-3)*t[1]
      low2<-q[2]-qt(0.975, n-3)*t[2]
      up2<-q[2]+qt(0.975, n-3)*t[2]
      low3<-q[3]-qt(0.975, n-3)*t[3]
      up3<-q[3]+qt(0.975, n-3)*t[3]
      res[1,run,1]<-q[1]
      res[1,run,2]<-low1
      res[1,run,3]<-up1
      res[2,run,1]<-q[2]
      res[2,run,2]<-low2
      res[2,run,3]<-up2
      res[3,run,1]<-q[3]
      res[3,run,2]<-low3
      res[3,run,3]<-up3
      
      
      
    })
    if (inherits(result, "try-error")){
      fails<-fails+1
      next
    }
    
    
    #}
  }
  return(list(list=res, number=fails))
}