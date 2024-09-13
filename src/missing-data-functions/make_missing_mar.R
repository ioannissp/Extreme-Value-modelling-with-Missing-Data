##### Missingness Generating Function #####
make_missing<-function(data, run){
  set.seed(seed=run)
  probmis<-sapply(data$x, function(chi) plogis(-1+chi)) #probability of each datum missing
  ry<-sapply(probmis, function(p) rbinom(1,1,p))
  data[ry==1, "y"]<-NA
  return(data)
  
}