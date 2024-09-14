library(mice)
library(evd)
library(extRemes)
library(VGAM)
library(ismev)
#sigma_tilde=1, mu=0, xi=0.5, u=1, sigma=0.5
make_data<-function(mu=0, xi=0.5, n=1000, run=1){
  set.seed(seed=run)
  x<-rnorm(n)
  #x<-scale(x)
  y<-sapply(x, function(dx) rgpd(1, loc=mu, scale=exp(1+dx), shape=xi))                                                  #    rgpd(n, loc=mu, scale=exp(1+x), shape=xi)
  df<-data.frame(x,y)
  return(df)
}



make_missing<-function(data, p=0.5){
  ry<-rbinom(nrow(data), 1, p)
  data[ry==0, "y"]<-NA
  return(data)
}

gpd_impute<-function(data, m=5){   #min ksexaseis na valeis to seed
  data_no_miss<-data[!is.na(data$y),]
  data_miss<-data[is.na(data$y),]
  n1<-length(data_no_miss$x)
  n0<-length(data_miss$x)
  dfs<-list()
  for (i in 1:m){
    #ximp<-sample(data_no_miss$x, n1, replace=TRUE)
    #yimp<-sample(data_no_miss$y, n1, replace=TRUE)
    #data_imp<-data.frame(ximp, yimp)
    bootstrap_indices <- sample(1:n1, size = n1, replace = TRUE)
    data_imp<-data_no_miss[bootstrap_indices,]
    num_exceedances <- sum(data_imp$y > 0)
    if (num_exceedances < 5) {
      stop("Insufficient exceedances over the threshold for GPD fitting.")
    }
    model_imp<-gpd.fit(xdat=data_imp$y, threshold=0, ydat=matrix(data_imp$x, ncol=1), sigl=1, siglink=exp)
    if (!is.numeric(model$mle) | !is.numeric(model$se)){
      print("Error in impute")
      break
      
    }
    coefs<-model_imp$mle
    y<-sapply(data_miss$x, function(xi) rgpd(1, loc=0, scale=exp(coefs[1]+coefs[2]*xi), shape=coefs[3]))                         #rgpd(n0, loc=0, scale=exp(coefs[1]+coefs[2]*data_miss$x), shape=coefs[3])
    x<-data_miss$x
    new_df<-rbind(data.frame(x,y),data_no_miss)
    dfs[[i]]<-new_df
    
  }
  return(dfs)
}

pool_custom<-function(imputed, m=5, k=3){
  
  q<-c(0,0,0)
  Q<-matrix(0, ncol=3, nrow=m)
  u<-matrix(0, ncol=3, nrow=3)
  b<-matrix(0, ncol=3, nrow=3)
  
  for (i in 1:m){
    data<-imputed[[i]]
    #data$y<-exp(data$y)
    model<-gpd.fit(xdat=data$y, threshold=0, ydat=matrix(data$x, ncol=1), sigl=1, siglink=exp)
    if (sum(is.na(model$mle))>0 | sum(is.na(model$se))>0){
      print("Error in pool")
      break
      
    }
    q<-q+model$mle
    Q[i,]<-model$mle
    u<-u+model$cov
    
    
  }
  q<-q/m
  u<-u/m
  for (i in 1:m){
    b<-b+(Q[i,]-q)%*%t(Q[i,]-q) 
  }
  b<-b/(m-1)
  t<-u+(1+1/m)*b
  ncom<-length(imputed[[1]]$y)-k
  
  lam<-c((1+1/m)*b[1,1]/t[1,1],(1+1/m)*b[2,2]/t[2,2],(1+1/m)*b[3,3]/t[3,3])
  
  #r1<-(1+1/m)*sum(diag(b%*%solve(u)))/k
  #ttilde<-(1+r1)*u
  
  nold1<-(m-1)/(lam[1]^2)
  nold2<-(m-1)/(lam[2]^2)
  nold3<-(m-1)/(lam[3]^2)
  
  nobs1<-((ncom+1)/(ncom+3))*ncom*(1-lam[1])
  nobs2<-((ncom+1)/(ncom+3))*ncom*(1-lam[2])
  nobs3<-((ncom+1)/(ncom+3))*ncom*(1-lam[3])
  
  nu1<-nold1*nobs1/(nold1+nobs1)
  nu2<-nold2*nobs2/(nold2+nobs2)
  nu3<-nold3*nobs3/(nold3+nobs3)
  #smallt<-k*(m-1)
  #if (smallt>4){
  #nu1<-4+(smallt-4)*(1+(1-2/smallt)/r1)^2
  #}
  #else{
  #nu1<-(smallt(1+1/k)*(1+1/r1)^2)/2
  #}
  low1<-q[1]-qt(0.975, nu1)*sqrt(t[1,1])
  up1<-q[1]+qt(0.975, nu1)*sqrt(t[1,1])
  low2<-q[2]-qt(0.975, nu2)*sqrt(t[2,2])
  up2<-q[2]+qt(0.975, nu2)*sqrt(t[2,2])
  low3<-q[3]-qt(0.975, nu3)*sqrt(t[3,3])
  up3<-q[3]+qt(0.975, nu3)*sqrt(t[3,3])
  
  is<-1
  true <- c(0,1, 0.5)
  #if (t(q-true)%*%solve(ttilde)%*%(q-true)/k>qf(0.5, df1=k, df2=nu1)){ #<= #
  #is<-0
  #}
  ret<-array(NA, dim=c(3,3))
  dimnames(ret)<-list(c("b0", "b1", "xi"), c("estimate", "2.5 %", "97.5 %"))
  ret[1,1]<-q[1]
  ret[1,2]<-low1
  ret[1,3]<-up1
  ret[2,1]<-q[2]
  ret[2,2]<-low2
  ret[2,3]<-up2
  ret[3,1]<-q[3]
  ret[3,2]<-low3
  ret[3,3]<-up3
  return(ret)
  #print(t(q-true)%*%solve(ttilde)%*%(q-true)/k)
  
  
}

simulate<-function(runs){
  res<-array(NA, dim=c(3,runs+1, 3))
  dimnames(res)<-list(c("b0","b1", "xi"), as.character(1:(runs+1)), c("estimate", "2.5 %", "97.5 %"))
  runs<-c(1:runs+1)
  runs<-runs[runs!=69]
  for(run in runs){
    repe<-run
    data<-make_data(run=run) #no 69
    data<-make_missing(data)
    imp<-gpd_impute(data)
    res[, run, ]<-pool_custom(imp)
    
    
  }
  res
}

res<-simulate(100)[1]

true <- c(1,1, 0.5)
#RB <- rowMeans(res[,, "estimate", drop="FALSE"]) - true
#PB <- 100 * abs((rowMeans(res[,, "estimate", drop="FALSE"]) - true)/ true)
CR <- c(rowMeans(res[1,, "2.5 %", drop="FALSE"] < true[1] & true[1] < res[1,, "97.5 %", drop="FALSE"]), rowMeans(res[2,, "2.5 %", drop="FALSE"] < true[2] & true[2] < res[2,, "97.5 %", drop="FALSE"]),rowMeans(res[3,, "2.5 %", drop="FALSE"] < true[3] & true[3] < res[3,, "97.5 %", drop="FALSE"]))
#AW <- rowMeans(res[,, "97.5 %", drop="FALSE"] - res[,, "2.5 %", drop="FALSE"])
#RMSE <- sqrt(rowMeans((res[,, "estimate", drop="FALSE"] - true)^2))
data.frame(CR) #AW