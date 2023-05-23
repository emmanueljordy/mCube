library(data.table)
library(feather)
library(R2DT)
library(devFunc)
library(MASS)
library(stringr)
library(zoo)
library(plyr)
library(nnet)
library(cluster)
library(tpSuite)
library(ismev)
library(caret)
library(gPdtest)
library(concProb)
library(robustbase)
library(QuantifQuantile)
library(survival)
library(dplyr)
library(ChainLadder)
library(lubridate)
library(tidyverse)
set.seed(9*2020)


microDataBI <- as.data.table(read.table("Gabrielli_dat_AZ.txt", sep=";", header=T))

Date_cols <- which(grepl( "Date" , names( microDataBI)))
date_cols <- c(Date_cols,which(grepl( "date" , names( microDataBI))))


microDataBI <- microDataBI %>%
  mutate_at(vars(date_cols), funs(as.Date(., "%Y-%m-%d")))

microDataBI[,transType := as.factor(transType)]



nrow(microDataBI[ind == 1 & year(accDate)>= 1994  & year(accDate) <= 2005 & year(repDate) <= 2005,])
microDataBI[,devPeriod := year(bookDate) - year(repDate)] #Here I use the definition of Gabrielli because we work with repDate to define the dev periods
# In the simulation machine, each claim has 12 years info from the year of accident. If reporting delay is >=1, we still have only 12 years from accident so less than 12 year info from reporting
microDataBI[, maxdevPeriod := max(devPeriod), by = polNumb]
microDataBISub <- copy(microDataBI[year(accDate)>=1994& year(repDate)>= 1994  & year(repDate) <= 2005 & devPeriod <= 11,])
#microDataBISub <- copy(microDataBI[ maxdevPeriod <= 6 &  year(accDate)>= 1994  & year(accDate) <= 2005 & vzCode == "NA",])
table(year(microDataBISub[ind == nLines, bookDate]),year(microDataBISub[ind == nLines, accDate]))
microDataBISub <- microDataBISub[LoB ==2,]
#delete data that is not needed anymore to free space 
rm(microDataBI)
##############################################

# we want to take into account also the claims that have a development of more than 6 years. You can look at cum_CF below to see what the square looks like
# This will act as some kind of tail factor
microDataBISub[payInd == 1 & devPeriod ==0, Pay00 := sum(delt0Pay), by = polNumb]# payment made in the same year in which the claim is reported
microDataBISub[, Pay00 := na.locf(Pay00, na.rm = FALSE), by = polNumb]
microDataBISub[payInd == 1 & devPeriod ==1, Pay01 := sum(delt0Pay), by = polNumb]# payment made in the one year after the claim is reported 
microDataBISub[, Pay01 := na.locf(Pay01, na.rm = FALSE), by = polNumb]
microDataBISub[payInd == 1 & devPeriod ==2, Pay02 := sum(delt0Pay), by = polNumb]
microDataBISub[, Pay02 := na.locf(Pay02, na.rm = FALSE), by = polNumb]
microDataBISub[payInd == 1 & devPeriod ==3, Pay03 := sum(delt0Pay), by = polNumb]
microDataBISub[, Pay03 := na.locf(Pay03, na.rm = FALSE), by = polNumb]
microDataBISub[payInd == 1 & devPeriod ==4, Pay04 := sum(delt0Pay), by = polNumb]
microDataBISub[, Pay04 := na.locf(Pay04, na.rm = FALSE), by = polNumb]
microDataBISub[payInd == 1 & devPeriod ==5, Pay05 := sum(delt0Pay), by = polNumb]
microDataBISub[, Pay05 := na.locf(Pay05, na.rm = FALSE), by = polNumb]
microDataBISub[payInd == 1 & devPeriod ==6, Pay06 := sum(delt0Pay), by = polNumb]
microDataBISub[, Pay06 := na.locf(Pay06, na.rm = FALSE), by = polNumb]
microDataBISub[payInd == 1 & devPeriod ==7, Pay07 := sum(delt0Pay), by = polNumb]
microDataBISub[, Pay07 := na.locf(Pay07, na.rm = FALSE), by = polNumb]
microDataBISub[payInd == 1 & devPeriod ==8, Pay08 := sum(delt0Pay), by = polNumb]
microDataBISub[, Pay08 := na.locf(Pay08, na.rm = FALSE), by = polNumb]
microDataBISub[payInd == 1 & devPeriod ==9, Pay09 := sum(delt0Pay), by = polNumb]
microDataBISub[, Pay09 := na.locf(Pay09, na.rm = FALSE), by = polNumb]
microDataBISub[payInd == 1 & devPeriod ==10, Pay10 := sum(delt0Pay), by = polNumb]
microDataBISub[, Pay10 := na.locf(Pay10, na.rm = FALSE), by = polNumb]
microDataBISub[payInd == 1 & devPeriod ==11, Pay11 := sum(delt0Pay), by = polNumb]
microDataBISub[, Pay11 := na.locf(Pay11, na.rm = FALSE), by = polNumb]

microDataBISub[,deltRep :=  year(repDate) - year(accDate)]
#open indicator represents if the claim is still open at the end of the year
microDataBISub[devPeriod ==0, Open00 := ifelse(is.na(closedDate),1,ifelse(year(closedDate) > year(bookDate),1,0)), by = polNumb]# payment made in the same year in which the claim is reported
#microDataBISub[devPeriod >=0 & deltRep>=1, Open00 := 0, by = polNumb]# payment made in the same year in which the claim is reported
microDataBISub[, Open00 := na.locf(Open00, na.rm = FALSE), by = polNumb]

microDataBISub[devPeriod ==1, Open01 := ifelse(is.na(closedDate),1,as.integer(year(closedDate) > year(bookDate))), by = polNumb]# payment made in the same year in which the claim is reported
#microDataBISub[devPeriod >=1 & deltRep>=2, Open01 := 0, by = polNumb]# payment made in the same year in which the claim is reported
microDataBISub[, Open01 := na.locf(Open01, na.rm = FALSE), by = polNumb]

microDataBISub[devPeriod ==2, Open02 := ifelse(is.na(closedDate),1,as.integer(year(closedDate) > year(bookDate))), by = polNumb]# payment made in the same year in which the claim is reported
#microDataBISub[devPeriod >=2 & deltRep>=3, Open02 := 0, by = polNumb]# payment made in the same year in which the claim is reported
microDataBISub[, Open02 := na.locf(Open02, na.rm = FALSE), by = polNumb]

microDataBISub[devPeriod ==3, Open03 := ifelse(is.na(closedDate),1,as.integer(year(closedDate) > year(bookDate))), by = polNumb]# payment made in the same year in which the claim is reported
#microDataBISub[devPeriod >=3 & deltRep>=4, Open03 := 0, by = polNumb]# payment made in the same year in which the claim is reported
microDataBISub[, Open03 := na.locf(Open03, na.rm = FALSE), by = polNumb]

microDataBISub[devPeriod ==4, Open04 := ifelse(is.na(closedDate),1,as.integer(year(closedDate) > year(bookDate))), by = polNumb]# payment made in the same year in which the claim is reported
#microDataBISub[devPeriod >=4 & deltRep>=5, Open04 := 0, by = polNumb]# payment made in the same year in which the claim is reported
microDataBISub[, Open04 := na.locf(Open04, na.rm = FALSE), by = polNumb]

microDataBISub[devPeriod ==5, Open05 := ifelse(is.na(closedDate),1,as.integer(year(closedDate) > year(bookDate))), by = polNumb]# payment made in the same year in which the claim is reported
#microDataBISub[devPeriod >=5 & deltRep>=6, Open05 := 0, by = polNumb]# payment made in the same year in which the claim is reported
microDataBISub[, Open05 := na.locf(Open05, na.rm = FALSE), by = polNumb]

microDataBISub[devPeriod == 6, Open06 := ifelse(is.na(closedDate),1,as.integer(year(closedDate) > year(bookDate))), by = polNumb]# payment made in the same year in which the claim is reported
#microDataBISub[devPeriod >=6 & deltRep>=7, Open06 := 0, by = polNumb]# payment made in the same year in which the claim is reported
microDataBISub[, Open06 := na.locf(Open06, na.rm = FALSE), by = polNumb]

microDataBISub[devPeriod == 7, Open07 := ifelse(is.na(closedDate),1,as.integer(year(closedDate) > year(bookDate))), by = polNumb]# payment made in the same year in which the claim is reported
#microDataBISub[devPeriod >=6 & deltRep>=7, Open06 := 0, by = polNumb]# payment made in the same year in which the claim is reported
microDataBISub[, Open07 := na.locf(Open07, na.rm = FALSE), by = polNumb]

microDataBISub[devPeriod == 8, Open08 := ifelse(is.na(closedDate),1,as.integer(year(closedDate) > year(bookDate))), by = polNumb]# payment made in the same year in which the claim is reported
#microDataBISub[devPeriod >=6 & deltRep>=7, Open06 := 0, by = polNumb]# payment made in the same year in which the claim is reported
microDataBISub[, Open08 := na.locf(Open08, na.rm = FALSE), by = polNumb]

microDataBISub[devPeriod == 9, Open09 := ifelse(is.na(closedDate),1,as.integer(year(closedDate) > year(bookDate))), by = polNumb]# payment made in the same year in which the claim is reported
#microDataBISub[devPeriod >=6 & deltRep>=7, Open06 := 0, by = polNumb]# payment made in the same year in which the claim is reported
microDataBISub[, Open09 := na.locf(Open09, na.rm = FALSE), by = polNumb]

microDataBISub[devPeriod == 10, Open10 := ifelse(is.na(closedDate),1,as.integer(year(closedDate) > year(bookDate))), by = polNumb]# payment made in the same year in which the claim is reported
#microDataBISub[devPeriod >=6 & deltRep>=7, Open06 := 0, by = polNumb]# payment made in the same year in which the claim is reported
microDataBISub[, Open10 := na.locf(Open10, na.rm = FALSE), by = polNumb]

microDataBISub[devPeriod == 11, Open11 := ifelse(is.na(closedDate),1,as.integer(year(closedDate) > year(bookDate))), by = polNumb]# payment made in the same year in which the claim is reported
#microDataBISub[devPeriod >=6 & deltRep>=7, Open06 := 0, by = polNumb]# payment made in the same year in which the claim is reported
microDataBISub[, Open11 := na.locf(Open11, na.rm = FALSE), by = polNumb]


microDataBISubUnique <- unique(microDataBISub, by = 'polNumb',fromLast = T)
microDataBISub$AY <- year(microDataBISub$accDate)
microDataBISub$RY <- year(microDataBISub$repDate)
microDataBISub$RepDel <- year(microDataBISub$repDate) -  year(microDataBISub$accDate)  
microDataBISub$ClNr <- microDataBISub$polNumb
microDataBISub$AQ <- substr(quarters(as.Date(microDataBISub$accDate)), 2, 2)
microDataBISub$SetYear <- year(microDataBISub$closedDate)

microDataBISubUnique <- unique(microDataBISub, by = 'polNumb',fromLast = T)

data <- as.data.frame(microDataBISubUnique[,c('ClNr','AY', 'AQ', 'RepDel', 'Pay00', 'Pay01', 'Pay02', 'Pay03', 'Pay04', 'Pay05', 'Pay06',
                                              'Pay07', 'Pay08',
                                              'Pay09', 'Pay10', 'Pay11', 'Open00', 'Open01',
                                              'Open02', 'Open03','Open04', 'Open05', 'Open06', 'Open07', 'Open08', 'Open09', 'Open10', 'Open11', 'SetYear','RY')])


# Chain Ladder 
microDataBISubUnique2 <- unique(microDataBISub, by = 'polNumb',fromLast = T)
microDataBISubUnique2 <- microDataBISubUnique2[,c('polNumb', 'deltRep', 'Pay00', 'Pay01', 'Pay02', 'Pay03', 'Pay04', 'Pay05', 'Pay06','Pay07', 'Pay08',
                                                  'Pay09', 'Pay10', 'Pay11', 'Open00', 'Open01',
                                                  'Open02', 'Open03','Open04', 'Open05', 'Open06', 'Open07', 'Open08', 'Open09', 'Open10', 'Open11',
                                                  'accDate', 'repDate', 'closedDate','bookDate')]
microDataBISubUnique2$AY <- year(microDataBISubUnique2$accDate)
microDataBISubUnique2$RY <- year(microDataBISubUnique2$repDate)
cum_CF <- round(ddply(microDataBISubUnique2, .(RY), summarise, CF00=sum(Pay00,na.rm=T),CF01=sum(Pay01,na.rm=T),CF02=sum(Pay02,na.rm=T),CF03=sum(Pay03,na.rm=T),CF04=sum(Pay04,na.rm=T),CF05=sum(Pay05,na.rm=T),CF06=sum(Pay06,na.rm=T),
                      CF07=sum(Pay07,na.rm=T), CF08=sum(Pay08,na.rm=T), CF09=sum(Pay09,na.rm=T), CF10=sum(Pay10,na.rm=T),  CF11=sum(Pay11,na.rm=T) )[,2:13])
for (j in 2:12){cum_CF[,j] <- cum_CF[,j-1] + cum_CF[,j]}
cum_CF

tail_fact <- F# 0 for False, 1 for True
#true reserves #we need cum_CF2 since here we also include those with devYear >6 
cum_CF2 <- round(ddply(microDataBISubUnique2, .(RY), summarise, CF00=sum(Pay00,na.rm=T),CF01=sum(Pay01,na.rm=T),CF02=sum(Pay02,na.rm=T),CF03=sum(Pay03,na.rm=T),CF04=sum(Pay04,na.rm=T),CF05=sum(Pay05,na.rm=T),CF06=sum(Pay06,na.rm=T),
                       CF07=sum(Pay07,na.rm=T), CF08=sum(Pay08,na.rm=T), CF09=sum(Pay09,na.rm=T), CF10=sum(Pay10,na.rm=T),  CF11=sum(Pay11,na.rm=T)         )[,2:(13+tail_fact)])
for (j in 2:(12+tail_fact)){cum_CF2[,j] <- cum_CF2[,j-1] + cum_CF2[,j]}
cum_CF2



true_reserves <- data.frame(array(0, dim=c(13+tail_fact,3)))
true_reserves <- setNames(true_reserves, c("true Res.","CL Res.","MSEP^(1/2)"))

for (i in 0:11){
  true_reserves[i+1,1] <- cum_CF2[i+1,12+tail_fact]-cum_CF2[i+1,12+tail_fact-(i+tail_fact)]
}

true_reserves[13,1] <- sum(true_reserves[1:12,1])
true_reserves
#small check: sum(microDataBISub[year(accDate)==2006 & payInd ==1 & year(bookDate)>2012,delt0Pay ]) is the true reserve for 2006 claims
### Mack chain-ladder analysis

tri_dat <- array(NA, dim(cum_CF))
for (i in 0:11){
  for (j in 0:(11-i)){tri_dat[i+1,j+1] <- cum_CF[i+1,j+1]}
}
tri_dat <- as.triangle(as.matrix(tri_dat))
#small check: sum(microDataBISub[year(accDate)==2006 & payInd ==1 & year(bookDate)<=2012,delt0Pay ]) 

dimnames(tri_dat)=list(origin=1:12, dev=1:12)

Mack <- MackChainLadder(tri_dat, est.sigma="Mack",tail=tail_fact)#tail should be true if we allow for extrapolation outside the rectangle
for (i in 0:11){true_reserves[i+1,2] <- round(Mack$FullTriangle[i+1,12+tail_fact]-Mack$FullTriangle[i+1,(12+tail_fact)-(i+tail_fact)])}
true_reserves[13,2] <- sum(true_reserves[1:12,2])
true_reserves[1:12,3] <- round(Mack$Mack.S.E[,12+tail_fact])
true_reserves[13,3] <- round(Mack$Total.Mack.S.E)
true_reserves                           # true reserves, chain-ladder reserves and square-rooted MSEP

