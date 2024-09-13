##### Required Packages #####
library(VGAM)
library(ismev)

##### Data Generating Function #####
make_data<-function(mu=0, xi=0.5, n=10000, run=1){
  set.seed(seed=run)
  x<-rnorm(n, mean = 0, sd=1)
  #x<-exp(x)
  x<-x[order(x, decreasing=FALSE)]
  
  y<-rep(NA, n)
  n1<-0.95*n
  
  y[1:n1]=sapply(x[1:n1], function(chi) rnorm(1, mean=1+chi, sd=1))
       
  u1<-max(y[1:n1]) #exact threshold
  u2<-quantile(y[1:n1], 0.995) #approximate threshold
  
  y[(n1+1):n]=u+sapply(x[(n1+1):n], function(chi) rgpd(1, loc=mu, scale=exp(1+chi), shape=xi))
  df<-data.frame(x,y)
  return(c(df, u2))
  
  
}