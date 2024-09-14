##### Required Packages #####
library(VGAM)
library(ismev)


#####Imputation Function#####
gpd_impute<-function(data, m=5, u, run){ #m is number of imputations, u is threshold
  data_no_miss<-data[!is.na(data$y),]
  data_miss<-data[is.na(data$y),]
  n1<-length(data_no_miss$x)
  n0<-length(data_miss$x)
  dfs<-list()
  set.seed(seed=run)
  for (i in 1:m){
    
    bootstrap_indices <- sample(1:n1, size = n1, replace = TRUE)
    data_imp<-data_no_miss[bootstrap_indices,]
    
    
    model_imp<-gpd.fit(xdat=data_imp$y, threshold=u, ydat=matrix(data_imp$x, ncol=1), sigl=1, siglink=exp)
    
    
    coefs<-as.numeric(model_imp$mle)
    y<-u+sapply(data_miss$x, function(chi) rgpd(1, loc=0, scale=exp(coefs[1]+coefs[2]*chi), shape=coefs[3]))                         
    x<-data_miss$x
    new_df<-rbind(data.frame(x,y),data_no_miss)
    dfs[[i]]<-new_df
    
  }
  return(dfs)
}