####Libraries####
library(extRemes)

####Standardisation####
mu<-mean(data_nona$propvalue_adj)
sigma<-sd(data_nona$propvalue_adj)
data_nona$propvalue_adj<-(data_nona$propvalue_adj-mu)/sigma


####GEV####
maxs<-blockmaxxer(data_nona, blocks=data_nona$iyear, which="propvalue_adj")

model<-fevd(maxs$propvalue_adj, type="GEV", method="MLE")


####GPD####
model_gpd<-fevd(data_nona$propvalue_adj, threshold = 0.025, type="GP")
