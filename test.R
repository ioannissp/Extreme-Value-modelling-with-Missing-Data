library(mice)
library(evd)
library(extRemes)
library(VGAM)
library(ismev)



pool_custom<-function(imputed, m=5, k=3){
  
  q<-c(0,0,0)
  Q<-matrix(0, ncol=3, nrow=m)
  u<-matrix(0, ncol=3, nrow=3)
  b<-matrix(0, ncol=3, nrow=3)
  
  for (i in 1:m){
    data<-complete(imputed,i)
    data$y<-exp(data$y)
    model<-gpd.fit(xdat=data$y, threshold=0, ydat=matrix(data$x, ncol=1), sigl=1, siglink=exp)
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

#confint_custom<-function(data){
  
#}