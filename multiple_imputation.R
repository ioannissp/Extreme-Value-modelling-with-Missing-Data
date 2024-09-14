library(mice)
library(evd)
library(extRemes)
library(VGAM)
library(ismev)
#sigma_tilde=1, mu=0, xi=0.5, u=1, sigma=0.5
make_data<-function(mu=0, xi=0.5, n=1000, run=1){
  set.seed(seed=run)
  x<-rnorm(n)
  x<-scale(x)
  y<-sapply(x, function(dx) rgpd(1, loc=mu, scale=exp(1+dx), shape=xi)) 
  #y<-scale(y)          #    rgpd(n, loc=mu, scale=exp(1+x), shape=xi)
  df<-data.frame(x,y)
  return(df)
}

make_missing<-function(data, p=0.5){
  ry<-rbinom(nrow(data), 1, p)
  data[ry==0, "y"]<-NA
  return(data)
}

simulate<-function(runs){
  res<-array(NA, dim=c(3,runs, 3))
  dimnames(res)<-list(c("b0", "b1","xi"), as.character(1:runs), c("estimate", "2.5 %","97.5 %"))
  for(run in 1:runs){
    data_sim<-make_data(run=run)
    data_sim<-make_missing(data_sim)
    model<-gpd.fit(xdat=na.omit(data_sim)$y, threshold=0, ydat=matrix(na.omit(data_sim)$x, ncol=1), sigl=1, siglink=exp)
    n<-length(na.omit(data_sim)$y)-3
    q<-model$mle
    t<-model$se
    low1<-q[1]-qt(0.975, n)*t[1]
    up1<-q[1]+qt(0.975, n)*t[1]
    low2<-q[2]-qt(0.975, n)*t[2]
    up2<-q[2]+qt(0.975, n)*t[2]
    low3<-q[3]-qt(0.975, n)*t[3]
    up3<-q[3]+qt(0.975, n)*t[3]
    res[1,run,1]<-q[1]
    res[1,run,2]<-low1
    res[1,run, 3]<-up1
    res[2,run, 1]<-q[2]
    res[2,run, 2]<-low2
    res[2, run, 3]<-up2
    res[3,run, 1]<-q[3]
    res[3,run, 2]<-low3
    res[3,run, 3]<-up3
    
    
  }
  return(res)
}

res<-simulate(1000)
#apply(res, c(3, 3), mean, na.rm = TRUE)
true <- c(1,1,0.5)
#RB <- rowMeans(res[,, "estimate", drop="FALSE"]) - true
#PB <- 100 * abs((rowMeans(res[1,, "estimate",drop="FALSE"]) - true)/ true)
CR <- c(rowMeans(res[1,, "2.5 %", drop="FALSE"] < true[1] & true[1] < res[1,, "97.5 %", drop="FALSE"]), rowMeans(res[2,, "2.5 %", drop="FALSE"] < true[2] & true[2] < res[2,, "97.5 %", drop="FALSE"]),rowMeans(res[3,, "2.5 %", drop="FALSE"] < true[3] & true[3] < res[3,, "97.5 %", drop="FALSE"]))               #rowMeans(res[,, "2.5 %", drop="FALSE"] < true & true < res[,, "97.5 %", drop="FALSE"])
#AW <- rowMeans(res[,, "97.5 %", drop="FALSE"] - res[,, "2.5 %", drop="FALSE"])
#RMSE <- sqrt(rowMeans((res[,, "estimate", drop="FALSE"] - true)^2))
data.frame(CR)