##### Simulation Function #####
simulate<-function(runs){
  res<-array(NA, dim=c(3,runs, 3))
  dimnames(res)<-list(c("b0","b1", "xi"), as.character(1:runs), c("estimate", "2.5 %", "97.5 %"))
  runs<-c(1:runs)
  fails<-0
  for(run in runs){
    
    l<-make_data(run=run)
    
    u<-as.numeric(l[3])
    data_sim<-data.frame(l$x,l$y)
    colnames(data_sim)[1]<-"x"
    colnames(data_sim)[2]<-"y"
    
    data_sim<-make_missing(data_sim, run=run)
    
    data_mis<-data_sim[is.na(data_sim$y), ]
    nmis<-length(data_mis$y)
    data_obs<-data_sim[!is.na(data_sim$y),]
    
    data_obs$sign<-rep(NA, length(data_obs$y)) #creating classes
    data_obs$sign[data_obs$y>u]<-1
    data_obs$sign[data_obs$y<=u]<-0
    
    #undersampling
    class0<-data_obs[data_obs$sign==0, ]
    class1<-data_obs[data_obs$sign==1, ]
    n1<-length(class1$x)
    class_0_sample <- class0[sample(nrow(class0), n1), ]
    undersampled<-rbind(class_0_sample, class1)
    
    #logistic regression
    log_reg<-glm(sign~x, data=undersampled, family="binomial")
    
    preds<-as.vector(predict(log_reg, newdata=data.frame(x=data_mis$x), type="response"))
    
    prob<-sapply(preds, function(p) rbinom(1, 1, p))
    data_mis$sign<-prob
    
    data_com<-rbind(data_obs, data_mis)
    data_com<-data_com[data_com$sign==1, ]
    data_com<-subset(data_com, select=-sign) # the data with y extreme
    
    
    result<-try({ 
      imp<-gpd_impute(data_com, u=u2, run=run, m=5)
      pooled_result<-pool_custom(imp, thresh=u2, run=run, m=5)
      if (is.null(pooled_result)) {
        
        fails <- fails + 1
        next  
      }
      res[, run,]<-pooled_result
    })
    if (inherits(result, "try-error")){
      fails<-fails+1
      next
    }
    
    
  }
  res
}
