library(mice)
library(evd)
library(extRemes)
library(VGAM)
library(ismev)
#sigma_tilde=1, mu=0, xi=0.5, u=1, sigma=0.5
make_data<-function(mu=0, xi=0.5, n=1000, run=1, p=0.9){
  set.seed(seed=run)
  x<-rnorm(n)
  x<-exp(x)
  phi<-rbinom(n,1,p)
  y<-rep(NA, n)
  y<-sapply(x, function(dx) rnorm(1, mean=1+dx))
  y[y>5]<-5+sapply(x[y>5], function(dx) rgpd(1, loc=mu, scale=1+dx, shape=xi))                                                  
  df<-data.frame(x,y)
  return(df)
}



make_missing<-function(data, p=0.5){
  ry<-rbinom(nrow(data), 1, p)
  data[ry==0, "y"]<-NA
  return(data)
}

pool_custom<-function(imputed, m=5, k=3){
  
  q<-c(0,0,0)
  Q<-matrix(0, ncol=3, nrow=m)
  u<-matrix(0, ncol=3, nrow=3)
  b<-matrix(0, ncol=3, nrow=3)
  
  for (i in 1:m){
    data<-complete(imputed,i)
    #data$y<-exp(data$y)
    #model<-gpd.fit(xdat=data$y, threshold=0, ydat=matrix(data$x, ncol=1), sigl=1, siglink=exp)
    model<-fevd(data$y, threshold=0, type="GP", scale.fun = ~data$x)
    q<-q+model$results$par
    Q[i,]<-model$results$par
    u<-u+solve(model$results$hessian)
    
    
  }
  q<-q/m
  u<-u/m
  for (i in 1:m){
    b<-b+(Q[i,]-q)%*%t(Q[i,]-q) 
  }
  b<-b/(m-1)
  t<-u+(1+1/m)*b
  ncom<-length(complete(imputed,1)$y)-k
  
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

  

test.impute<-function(data, method="norm"){
  imp<-mice(data, method=method, m=5, print=FALSE)
  fit<-with(imp, gpd.fit(xdat=y, threshold = 0, ydat=matrix(x, ncol=1), sigl=1, siglink=exp))
  tab<-summary(pool(fit), "all", conf.int=TRUE)
  as.numeric(tab[2,c("estimate", "2.5 %", "97.5 %")])
}

simulate<-function(runs){
  res<-array(NA, dim=c(3,runs, 3))
  dimnames(res)<-list(c("b0","b1", "xi"), as.character(1:runs), c("estimate", "2.5 %", "97.5 %"))
  for(run in 1:runs){
    rep<-run
    data_sim<-make_data(run=run)
    data_sim<-make_missing(data_sim)
    
    data_sim<-make_data(run=run)
    data_sim<-make_missing(data_sim)
    data_mis<-data_sim[is.na(data_sim$y), ]
    nmis<-length(data_mis$y)
    data_obs<-data_sim[!is.na(data_sim$y),]
    data_obs$sign<-rep(NA, length(data_obs$y))
    data_obs$sign[data_obs$y>5]<-1
    data_obs$sign[data_obs$y<=5]<-0
    log_reg<-glm(sign~x, data=data_obs, family="binomial")
    preds<-as.vector(predict(log_reg, newdata=list(x=as.vector(data_mis$x)), type="response"))
    prob<-sapply(preds, function(p) rbinom(1, 1, p))
    data_mis$sign<-prob
    data_com<-rbind(data_obs, data_mis)
    data_com<-data_com[data_com$sign==1, ]
    data_com<-subset(data_com, select=-sign)
    
    
    imp<-mice(data_com, method="norm.boot", m=5, print=FALSE)
    res[, run, ]<-pool_custom(imp)
    
    
  }
  res
}

res<-simulate(1000)

true <- c(1,1, 0.5)
#RB <- rowMeans(res[,, "estimate", drop="FALSE"]) - true
#PB <- 100 * abs((rowMeans(res[,, "estimate", drop="FALSE"]) - true)/ true)
CR <- c(rowMeans(res[1,, "2.5 %", drop="FALSE"] < true[1] & true[1] < res[1,, "97.5 %", drop="FALSE"]), rowMeans(res[2,, "2.5 %", drop="FALSE"] < true[2] & true[2] < res[2,, "97.5 %", drop="FALSE"]),rowMeans(res[3,, "2.5 %", drop="FALSE"] < true[3] & true[3] < res[3,, "97.5 %", drop="FALSE"]))
#AW <- rowMeans(res[,, "97.5 %", drop="FALSE"] - res[,, "2.5 %", drop="FALSE"])
#RMSE <- sqrt(rowMeans((res[,, "estimate", drop="FALSE"] - true)^2))
data.frame(CR) #AW