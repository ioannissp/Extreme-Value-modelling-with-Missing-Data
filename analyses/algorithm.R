##### Required Packages #####
library(here)
library(ismev)
library(VGAM)

##### Simulating the CCA #####
here("data", "data_generation_for_exact_threshold.R") #for exact threshold
#here("data", "data_generation_for_approximate_threshold.R") #for approximate threshold

here("src/missing-data-functions/", "make_missing_mcar.R") #for data mcar
#here("src/missing-data-functions/", "make_missing_mar.R") #for data mar

here("src/pooling-function/", "pool_custom.R")

here("src/imputation-function/", "gpd_impute.R")

here("src/simulation-functions/", "simulate_cca.R")

sims<-simulate(1000)
fails<-sims$number
res<-sims$list

true <- c(1,1,0.5)
##metrics##
RB <- c(rowMeans(res[1,, "estimate", drop="FALSE"], na.rm=TRUE) - true[1], rowMeans(res[2,, "estimate", drop="FALSE"], na.rm=TRUE) - true[2], rowMeans(res[3,, "estimate", drop="FALSE"], na.rm=TRUE) - true[3])
PB <- c(100 * abs((rowMeans(res[1,, "estimate",drop="FALSE"], na.rm=TRUE) - true[1])/ true[1]), 100 * abs((rowMeans(res[2,, "estimate",drop="FALSE"], na.rm=TRUE) - true[2])/ true[2]), 100 * abs((rowMeans(res[3,, "estimate",drop="FALSE"], na.rm=TRUE) - true[3])/ true[3]))
CR <- c(rowMeans(res[1,, "2.5 %", drop="FALSE"] < true[1] & true[1] < res[1,, "97.5 %", drop="FALSE"], na.rm = TRUE), rowMeans(res[2,, "2.5 %", drop="FALSE"] < true[2] & true[2] < res[2,, "97.5 %", drop="FALSE"], na.rm=TRUE),rowMeans(res[3,, "2.5 %", drop="FALSE"] < true[3] & true[3] < res[3,, "97.5 %", drop="FALSE"], na.rm=TRUE))               #rowMeans(res[,, "2.5 %", drop="FALSE"] < true & true < res[,, "97.5 %", drop="FALSE"])
AW <- c(rowMeans(res[1,, "97.5 %", drop="FALSE"] - res[1,, "2.5 %", drop="FALSE"], na.rm=TRUE), rowMeans(res[2,, "97.5 %", drop="FALSE"] - res[2,, "2.5 %", drop="FALSE"], na.rm=TRUE), rowMeans(res[3,, "97.5 %", drop="FALSE"] - res[3,, "2.5 %", drop="FALSE"], na.rm=TRUE))
RMSE <- c(sqrt(rowMeans((res[1,, "estimate", drop="FALSE"] - true[1])^2, na.rm=TRUE)), sqrt(rowMeans((res[2,, "estimate", drop="FALSE"] - true[2])^2, na.rm=TRUE)), sqrt(rowMeans((res[3,, "estimate", drop="FALSE"] - true[3])^2, na.rm=TRUE)))
data.frame(RB, PB, CR, AW, RMSE)