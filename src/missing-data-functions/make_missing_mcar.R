##### Missingness Generating Function #####
make_missing<-function(data, p=0.5, run){
  set.seed(seed=run)
  ry<-rbinom(nrow(data), 1, p)
  data[ry==0, "y"]<-NA
  return(data)
}
