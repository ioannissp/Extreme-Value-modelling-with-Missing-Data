#libraries
library(readxl)
library(dplyr)
library(lubridate)
library(tidyverse)

#data
#year, month, day (they have 0s), latitude, longitude, region, attacktype_1 (has unknowns), weaptype_1 (has unknowns and others), 
#propvalues (has unknowns and need adjustment for inflation),
#gtd<-read_excel("globalterrorismdb_0522dist.xlsx")
#head(gtd)
prop_damage<-gtd$propvalue[gtd$property==1]
data<-gtd
data<-data %>% 
  select(iyear, imonth, iday, latitude, longitude, region, attacktype1, weaptype1, property, propvalue, targtype1) %>%
  filter(property==1)
  
data<-data %>%
  filter(iyear!=0 & imonth!=0 & iday!=0 & weaptype1!=1 & weaptype1!=3 & weaptype1!=7)
data<-data %>%
  mutate(idate=make_date(iyear, imonth, iday))

n<-length(data$propvalue)

cpi_monthly<-read_excel("C:/Users/Ιωάννης Σπανός/thesis/SeriesReport-20240711141415_c0eb9b.xlsx")
cpi_monthly<- cpi_monthly %>% 
  select(Year, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec)
cpi<-cpi_monthly %>%
  filter(Year > 1969)
cpi<-cpi %>%
  rename(
    'iyear'='Year',
    '1'=Jan,
    '2'=Feb,
    '3'=Mar,
    '4'=Apr,
    '5'=May,
    '6'=Jun,
    '7'=Jul,
    '8'=Aug,
    '9'=Sep,
    '10'=Oct,
    '11'=Nov,
    '12'=Dec)
cpi<-cpi %>%
  pivot_longer(cols='1':'12', names_to='imonth',values_to='CPI')
cpi<-cpi %>%
  mutate(imonth=as.numeric(imonth))

data<- data %>%
  left_join(cpi, by=c('iyear', 'imonth'))

cpijan2024<-308.417
data$propvalue_adj<-data$propvalue*cpijan2024/data$CPI 

data_nona<-data %>%
    select(iyear, idate, latitude, longitude, region, attacktype1, weaptype1, targtype1, propvalue_adj)
data_nona<-data_nona[!is.na(data_nona$propvalue_adj),]
#data_nona<-data_nona[!is.na(data_nona$latitude) & !is.na(data_nona$longitude),]
#data_nona<-data %>%
#  select(iyear, idate,latitude, longitude, region, attacktype1, weaptype1, targtype1, propvalue_adj)
#data_nona<-data_nona[!is.na(data_nona$propvalue_adj), ]

#missing dates
missing_time_perch<-sum(gtd$iyear[gtd$property==1]==0 | gtd$imonth[gtd$property==1]==0 | gtd$iday[gtd$property==1]==0)/length(prop_damage)

#missing damages
nas_sum<-(sum(is.na(data$propvalue))+sum(data$propvalue==-99, na.rm=TRUE))
nas<-nas_sum/length(data$propvalue)

nas_by_reg<-data %>% group_by(region) %>% summarise(mean_n=mean(is.na(propvalue) | propvalue==-99))
nas_by_wep<-data %>% group_by(weaptype1) %>% summarise(mean_n=mean(is.na(propvalue | propvalue==-99)))
nas_by_attack<-data %>% group_by(attacktype1) %>% summarise(mean_n=mean(is.na(propvalue) | propvalue==-99))
nas_by_targ<-data %>% group_by(targtype1) %>% summarise(mean_n=mean(is.na(propvalue) | propvalue==-99))
nas_by_year<-data %>% group_by(iyear) %>% summarise(mean_n=mean(is.na(propvalue) | propvalue==-99))

nas_perc_reg<-data %>% group_by(region) %>% summarise(perch=sum(is.na(propvalue) | propvalue==-99)/nas_sum)
nas_perc_wep<-data %>% group_by(weaptype1) %>% summarise(perch=sum(is.na(propvalue) | propvalue==-99)/nas_sum)
nas_perc_attack<-data %>% group_by(attacktype1) %>% summarise(perch=sum(is.na(propvalue) | propvalue==-99)/nas_sum)
nas_perc_targ<-data %>% group_by(targtype1) %>% summarise(perch=sum(is.na(propvalue) | propvalue==-99)/nas_sum)

regs<-data %>% group_by(region) %>% summarise(perch=length(propvalue)/n)
wep<-data %>% group_by(weaptype1) %>% summarise(sum=length(!is.na(propvalue) & propvalue!=-99)/n)
attack<-data %>% group_by(attacktype1) %>% summarise(sum=length(!is.na(propvalue) & propvalue!=-99)/n)
targ<-data %>% group_by(targtype1) %>% summarise(perch=sum(!is.na(propvalue) & propvalue!=-99)/n)