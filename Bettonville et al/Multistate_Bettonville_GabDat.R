library(data.table)
library(R2DT)
library(devFunc)
library(ggplot2)
library(plyr)

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
microDataBISub <- microDataBISub[LoB ==1,]
#delete data that is not needed anymore to free space 
rm(microDataBI)

# Fonctions: regroup small payments (to smooth the link ratios)

truncPaidPeriod <- function(threshold, inputDT){
  
  inputDT[, prevPayment := payment]
  
  indNonZero <- 1:(nrow(inputDT)-1)
  inputDT[, index := 1:nrow(inputDT)]
  inputDT[, maxInd := max(index), by = .(numClaims)]
  indNonZero <- setdiff(indNonZero, unique(inputDT$maxInd))
  inputDT[, maxInd := NULL]
  
  firstPay <- copy(inputDT[payment != 0, ])  
  firstPay <- firstPay[, firstPaym := first(payment), by = c('numClaims')]
  firstPay <- firstPay[, .(numClaims, firstPaym)]
  firstPay <- firstPay[!duplicated(firstPay[, .(numClaims)]), ]
  inputDT <- copy(merge(inputDT, firstPay, all.x = T, by = c('numClaims')))
  indexFirstPay <- inputDT[payment == firstPaym, .(numClaims, index)]
  indexFirstPay <- indexFirstPay[!duplicated(indexFirstPay[, .(numClaims)]), ]
  indexFirstPay[, indexFirstPaym := index]
  indexFirstPay[, index := NULL]
  inputDT <- copy(merge(inputDT, indexFirstPay, all.x = T, by = c('numClaims')))
  inputDT[, indexToKeep := 0]
  inputDT[index >= indexFirstPaym, indexToKeep := 1]
  indNonZero <- setdiff(indNonZero, which(inputDT$indexToKeep == 0))
  indNonZero <- setdiff(indNonZero, which(inputDT$maxNumbPayment == 1))
  
  for(iRow in indNonZero){
    if(inputDT[iRow, .(payment)][[1]] <= threshold){
      inputDT[(iRow + 1), payment := inputDT[iRow, .(payment)] + inputDT[(iRow + 1), .(payment)]]
      inputDT <- inputDT[iRow, payment := 0]
    }
  }
  inputDT[, paidCumPeriod := cumsum(payment), by = c('numClaims')]
  inputDT[, linkRatio := paidCumPeriod/shift(paidCumPeriod, n = 1, fill = NA, type = "lag"), by = c('numClaims')]
  inputDT[linkRatio == Inf, linkRatio := NA]
  inputDT[, paymentFlag := sapply(payment, function(x){if(x == 0 | is.na(x)){0}else{1}})]
  inputDT[, numbPayment := cumsum(paymentFlag), by = c('numClaims')]
  inputDT[, maxNumbPayment := max(numbPayment), by = c('numClaims')]
  inputDT[, index := NULL]
  inputDT[, firstPaym := NULL]
  inputDT[, indexFirstPaym := NULL]
  inputDT[, indexToKeep := NULL]
  return(inputDT)
}


microDataBISub$AY <- year(microDataBISub$accDate)
microDataBISub$RY <- year(microDataBISub$repDate)
microDataBISub$RepDel <- year(microDataBISub$repDate) -  year(microDataBISub$accDate)  
microDataBISub$ClNr <- microDataBISub$polNumb
microDataBISub$AQ <- substr(quarters(as.Date(microDataBISub$accDate)), 2, 2)
microDataBISub[,maxInd := max(ind), by = polNumb]
microDataBISub[, SetYear := RY + maxInd -1]

microDataPred <- microDataBISub[year(bookDate)>2005,]



data <- microDataBISub %>% select(polNumb,delt0Pay, AY, ind, RY, SetYear )

data <- as.data.table(data)
names(data) <- c('numClaims', 'payment', 'accidentYear', 'developmentYear', 'reportingYear', 'settlementYear')

data[settlementYear>2005, settlementYear := 0]
data[,calendarYear := reportingYear + developmentYear -1]
head(data)

sum(data[calendarYear>2005, payment])

data <- data[calendarYear<= 2005,]



# On retire l'ann?e d'accident 2014

asNumericDT(data, 'payment')

# On cr?e les variables paiements cumul?s, link ratio, nombre de paiements et temps pass? dans l'?tat

data[, paidCumPeriod := cumsum(payment), by = c('numClaims')]
data[, linkRatio := paidCumPeriod/shift(paidCumPeriod, n = 1, fill = NA, type = "lag"), by = c('numClaims')]
data[, paymentOrNot := sapply(payment, function(x){if(x == 0 | is.na(x)){0}else{1}})]
data[, numbPayment := cumsum(paymentOrNot), by = c('numClaims')]
data[, maxNumbPayment := max(numbPayment), by = c('numClaims')]
maxNumbPay <- max(data[, .(maxNumbPayment)])


# On shifte les variables nombre de paiement et temps pass? dans l'?tat pour avoir l'?tat pr?c?dent dans lequel on se trouve et le temps pass? dans cet ?tat pr?c?dent

data <- truncPaidPeriod(200, data)
data[, timeInState := seq(.N), by = c('numClaims', 'numbPayment')]
data[, numbPaymentShift := shift(numbPayment, fill = NA, type = "lag"), by = c('numClaims')]

data[, timeInStateShift := shift(timeInState, fill = NA, type = "lag"), by = c('numClaims')]
data[, paymentShift := shift(payment, fill = NA, type = "lag"), by = c('numClaims')]

dataOrigin <- data

# On retire la contribution des sinistres qui restent ouverts dans la DB
# On d?finit la variable binaire 0 si le sinistre reste dans l'?tat et 1 s'il transite vers l'?tat suivant

#IBNR <- data[accidentYear + developmentYear - 1 <= reportingYear, ]

data <- copy(dataOrigin)

data[, paymentShift2 := first(paymentShift), by = c('numClaims', 'numbPaymentShift')]
data[, paymentShift := paymentShift2]
data[, paymentShift2 := NULL]

data[, transType := 0]
for(iList in 1:maxNumbPay){
  data[(numbPayment == iList & timeInState == 1), transType := 1]
  data[(numbPayment == iList) & (settlementYear == accidentYear + developmentYear - 1), transType := 1]
}

dataOrigin[, transType := 0]
for(iList in 1:maxNumbPay){
  dataOrigin[(numbPayment == iList & timeInState == 1), transType := 1]
  dataOrigin[(numbPayment == iList) & (settlementYear == accidentYear + developmentYear - 1), transType := 1]
}

dataOrigin[, maxDevYear := max(developmentYear), by = c('numClaims')]
censoredClaim <- dataOrigin[(settlementYear == 0 & numbPayment == maxNumbPayment & developmentYear == maxDevYear)]
data <- data[! (settlementYear == 0 & numbPayment == maxNumbPayment & transType == 0)]

# IBNR

# IBNR[, repPeriod := reportingYear - accidentYear + 1]
# IBNR[is.na(timeInStateShift), timeInStateShift := 0]
# IBNR[, transType := 0]
# for(iList in 1:(max(unique(IBNR$numbPayment), na.rm = TRUE)+1)){
#   IBNR[(numbPayment == iList-1 & (accidentYear + developmentYear - 1 == reportingYear)), transType := 1]
#   IBNR[(numbPayment == iList-1) & (settlementYear == accidentYear + developmentYear - 1), transType := 1]
# }
# 
# 
# table(IBNR$transType)
# table(IBNR$transType, IBNR$accidentYear, IBNR$timeInStateShift)
# 
# IBNR[, timeInStateShift0 := 0]
# IBNR[timeInStateShift == 0, timeInStateShift0 := 1]
# IBNR[, timeInStateShift1 := 0]
# IBNR[timeInStateShift == 1, timeInStateShift1 := 1]
# IBNR[, timeInStateShift2 := 0]
# IBNR[timeInStateShift == 2, timeInStateShift2 := 1]
# IBNR[, timeInStateShift3 := 0]
# IBNR[timeInStateShift == 3, timeInStateShift3 := 1]
# 
# splitTrans1 <- glm(transType ~ -1 + timeInStateShift0 + timeInStateShift1 + timeInStateShift2, data = IBNR, family = binomial(link = logit))
# summary(splitTrans1)
# 
# IBNR[, timeInStateShiftMore1 := timeInStateShift1 + timeInStateShift2]
# 
# splitTrans2 <- glm(transType ~ -1 + timeInStateShift0 + timeInStateShiftMore1, data = IBNR, family = binomial(link = logit))
# summary(splitTrans2)
# 
# anova(splitTrans1, splitTrans2, test = "Chisq")
# 
# tabSplitTrans <- unique(data.table(IBNR$timeInStateShift, splitTrans2$fitted.values))
# names(tabSplitTrans) <- c("timeInStatePrev", "splitTrans")
# tabSplitTrans <- tabSplitTrans[order(timeInStatePrev),]
# tabSplitTrans[4,2] <- 1
# 
# IBNRTerm <- IBNR[transType == 1, ]
# IBNRTerm[, transTerm := 0]
# IBNRTerm[(settlementYear == accidentYear + developmentYear - 1), transTerm := 1]
# 
# table(IBNRTerm$transTerm, IBNRTerm$timeInStateShift)
# 
# splitTerm1 <- glm(transTerm ~ -1 + timeInStateShift2 + timeInStateShift3, data = IBNRTerm, family = binomial(link = logit))
# summary(splitTerm1)
# 
# IBNRTerm[, timeInStateShiftMore2 := timeInStateShift2 + timeInStateShift3]
# 
# splitTerm2 <- glm(transTerm ~ -1 + timeInStateShiftMore2, data = IBNRTerm, family = binomial(link = logit))
# summary(splitTerm2)
# 
# anova(splitTerm1, splitTerm2, test="Chisq")
# 
# tabSplitTerm <- unique(data.table(IBNRTerm$timeInStateShift, splitTerm2$fitted.values))
# names(tabSplitTerm) <- c("timeInStatePrev", "splitTerm")
# tabSplitTerm <- tabSplitTerm[order(timeInStatePrev),]
# tabSplitTerm[1:2,2] <- 0
# 
# IBNRClose <- IBNRTerm[transTerm == 1, ]
# table(IBNRClose$paymentFlag, IBNRClose$timeInStateShift)
# tabSplitClose <- as.data.table(matrix(0,2,2))
# tabSplitClose[, 1] <- 2:3
# tabSplitClose[, 2] <- c(1,1)
# names(tabSplitClose) <- c("timeInStatePrev", "splitClose")
# tabSplitClose <- tabSplitClose[order(timeInStatePrev),]
# 
# IBNRPay <- IBNRTerm[transTerm == 0, ]
# splitPay1 <- glm(numbPayment ~ -1 + timeInStateShift0 + timeInStateShift1 + timeInStateShift2 + timeInStateShift3, data = IBNRPay, family = binomial(link = logit))
# summary(splitPay1)
# 
# splitPay2 <- glm(numbPayment ~ -1 + timeInStateShift0 + timeInStateShift1 + timeInStateShiftMore2, data = IBNRPay, family = binomial(link = logit))
# summary(splitPay2)
# 
# anova(splitPay1, splitPay2, test="Chisq")
# 
# tabSplitPay <- unique(data.table(IBNRPay$timeInStateShift, splitPay2$fitted.values))
# names(tabSplitPay) <- c("timeInStatePrev", "splitPay")
# tabSplitPay <- tabSplitPay[order(timeInStatePrev),]
# 
# 
# library(gamlss.mx)
# IBNR[, linkRatio := NULL]
# # IBNR[, timeInStateShift := NULL]
# IBNR[, numbPaymentShift := NULL]
# IBNR[, paymentShift := NULL]
# payIBNR <- gamlssMXfits(n = 10, payment ~ 1, data = IBNR[payment != 0, ], K = 2,  family = c(LOGNO, LOGNO))
# summary(payIBNR$models[[1]])
# summary(payIBNR$models[[2]])
# 
# tabPayIBNRLogNo1 <- unique(data.table(IBNR[payment != 0, ]$timeInStateShift, round(exp(payIBNR$models[[1]]$mu.fv + 0.5*payIBNR$models[[1]]$sigma.fv^2),6)))
# names(tabPayIBNRLogNo1) <- c("timeInStatePrev", "payIBNR")
# tabPayIBNRLogNo1 <- tabPayIBNRLogNo1[order(timeInStatePrev),]
# 
# tabPayIBNRLogNo2 <- unique(data.table(IBNR[payment != 0, ]$timeInStateShift, round(exp(payIBNR$models[[2]]$mu.fv + 0.5*payIBNR$models[[2]]$sigma.fv^2),6)))
# names(tabPayIBNRLogNo2) <- c("timeInStatePrev", "payIBNR")
# tabPayIBNRLogNo2 <- tabPayIBNRLogNo2[order(timeInStatePrev),]

# On cat?gorise les variables explicatives "temps pass? dans l'?tat pr?c?dent" et "?tat pr?c?dent dans lequel on se trouve". On mod?lise ensuite la variable r?ponse transType avec un glm binomiale.

data[numbPaymentShift >= 4, numbPaymentShift := 4]
data[timeInStateShift >= 3, timeInStateShift := 3]
data[numbPayment >= 4, numbPayment := 4]
data[timeInState >= 3, timeInState := 3]
asFactorDT(data, "numbPaymentShift")
asFactorDT(data, "timeInStateShift")

data[, timeInStateShift1_numbPaymentShift0 := 0]
data[timeInStateShift == 1 & numbPaymentShift == 0, timeInStateShift1_numbPaymentShift0 := 1]
data[, timeInStateShift2_numbPaymentShift0 := 0]
data[timeInStateShift == 2 & numbPaymentShift == 0, timeInStateShift2_numbPaymentShift0 := 1]
data[, timeInStateShift3_numbPaymentShift0 := 0]
data[timeInStateShift == 3 & numbPaymentShift == 0, timeInStateShift3_numbPaymentShift0 := 1]
data[, timeInStateShift1_numbPaymentShift1 := 0]
data[timeInStateShift == 1 & numbPaymentShift == 1, timeInStateShift1_numbPaymentShift1 := 1]
data[, timeInStateShift2_numbPaymentShift1 := 0]
data[timeInStateShift == 2 & numbPaymentShift == 1, timeInStateShift2_numbPaymentShift1 := 1]
data[, timeInStateShift3_numbPaymentShift1 := 0]
data[timeInStateShift == 3 & numbPaymentShift == 1, timeInStateShift3_numbPaymentShift1 := 1]
data[, timeInStateShift1_numbPaymentShift2 := 0]
data[timeInStateShift == 1 & numbPaymentShift == 2, timeInStateShift1_numbPaymentShift2 := 1]
data[, timeInStateShift2_numbPaymentShift2 := 0]
data[timeInStateShift == 2 & numbPaymentShift == 2, timeInStateShift2_numbPaymentShift2 := 1]
data[, timeInStateShift3_numbPaymentShift2 := 0]
data[timeInStateShift == 3 & numbPaymentShift == 2, timeInStateShift3_numbPaymentShift2 := 1]
data[, timeInStateShift1_numbPaymentShift3 := 0]
data[timeInStateShift == 1 & numbPaymentShift == 3, timeInStateShift1_numbPaymentShift3 := 1]
data[, timeInStateShift2_numbPaymentShift3 := 0]
data[timeInStateShift == 2 & numbPaymentShift == 3, timeInStateShift2_numbPaymentShift3 := 1]
data[, timeInStateShift3_numbPaymentShift3 := 0]
data[timeInStateShift == 3 & numbPaymentShift == 3, timeInStateShift3_numbPaymentShift3 := 1]
data[, timeInStateShift1_numbPaymentShift4 := 0]
data[timeInStateShift == 1 & numbPaymentShift == 4, timeInStateShift1_numbPaymentShift4 := 1]
data[, timeInStateShift2_numbPaymentShift4 := 0]
data[timeInStateShift == 2 & numbPaymentShift == 4, timeInStateShift2_numbPaymentShift4 := 1]
data[, timeInStateShift3_numbPaymentShift4 := 0]
data[timeInStateShift == 3 & numbPaymentShift == 4, timeInStateShift3_numbPaymentShift4 := 1]


modeltrans1 <- glm(transType ~ -1 + timeInStateShift1_numbPaymentShift0 + timeInStateShift2_numbPaymentShift0 + timeInStateShift3_numbPaymentShift0 + timeInStateShift1_numbPaymentShift1 + timeInStateShift2_numbPaymentShift1 + timeInStateShift3_numbPaymentShift1 + timeInStateShift1_numbPaymentShift2 + timeInStateShift2_numbPaymentShift2 + timeInStateShift3_numbPaymentShift2 + timeInStateShift1_numbPaymentShift3 + timeInStateShift2_numbPaymentShift3 + timeInStateShift3_numbPaymentShift3 + timeInStateShift1_numbPaymentShift4 + timeInStateShift2_numbPaymentShift4 + timeInStateShift3_numbPaymentShift4, data = data, family = binomial(link = logit))
summary(modeltrans1)

data[, timeInStateShift2 := timeInStateShift2_numbPaymentShift1 + timeInStateShift2_numbPaymentShift2 + timeInStateShift2_numbPaymentShift3 + timeInStateShift2_numbPaymentShift4]


modeltrans2 <- glm(transType ~ -1 + timeInStateShift1_numbPaymentShift0 + timeInStateShift2_numbPaymentShift0 + timeInStateShift3_numbPaymentShift0 + timeInStateShift1_numbPaymentShift1 + timeInStateShift3_numbPaymentShift1 + timeInStateShift1_numbPaymentShift2 + timeInStateShift3_numbPaymentShift2 + timeInStateShift1_numbPaymentShift3 + timeInStateShift3_numbPaymentShift3 + timeInStateShift1_numbPaymentShift4 + timeInStateShift3_numbPaymentShift4 + timeInStateShift2, data = data, family = binomial(link = logit))
summary(modeltrans2)

anova(modeltrans1, modeltrans2, test="Chisq")

data[, timeInStateShift3_numbPaymentShift2and4 := timeInStateShift3_numbPaymentShift2 + timeInStateShift3_numbPaymentShift4]


modeltrans3 <- glm(transType ~ -1 + timeInStateShift1_numbPaymentShift0 + timeInStateShift2_numbPaymentShift0 + timeInStateShift3_numbPaymentShift0 + timeInStateShift1_numbPaymentShift1 + timeInStateShift3_numbPaymentShift1 + timeInStateShift1_numbPaymentShift2 + timeInStateShift3_numbPaymentShift2and4 + timeInStateShift1_numbPaymentShift3 + timeInStateShift3_numbPaymentShift3 + timeInStateShift1_numbPaymentShift4+ timeInStateShift2, data = data, family = binomial(link = logit))
summary(modeltrans3)

anova(modeltrans2, modeltrans3, test="Chisq")

data[, timeInStateShift3_numbPaymentShift2and4 := timeInStateShift3_numbPaymentShift2 + timeInStateShift3_numbPaymentShift4]


modeltrans3 <- glm(transType ~ -1 + timeInStateShift1_numbPaymentShift0 + timeInStateShift2_numbPaymentShift0 + timeInStateShift3_numbPaymentShift0 + timeInStateShift1_numbPaymentShift1 + timeInStateShift3_numbPaymentShift1 + timeInStateShift1_numbPaymentShift2 + timeInStateShift3_numbPaymentShift2and4 + timeInStateShift1_numbPaymentShift3 + timeInStateShift3_numbPaymentShift3 + timeInStateShift1_numbPaymentShift4+ timeInStateShift2, data = data, family = binomial(link = logit))
summary(modeltrans3)


data[, timeInStateShift1_numbPaymentShift3and4 := timeInStateShift1_numbPaymentShift3 + timeInStateShift1_numbPaymentShift4]


modeltrans4 <- glm(transType ~ -1 + timeInStateShift1_numbPaymentShift0 + timeInStateShift2_numbPaymentShift0 + timeInStateShift3_numbPaymentShift0 + timeInStateShift1_numbPaymentShift1 + timeInStateShift3_numbPaymentShift1 + timeInStateShift1_numbPaymentShift2 + timeInStateShift3_numbPaymentShift2and4 + timeInStateShift3_numbPaymentShift3 + timeInStateShift1_numbPaymentShift3and4+ timeInStateShift2, data = data, family = binomial(link = logit))
summary(modeltrans4)

anova(modeltrans3, modeltrans4, test="Chisq")

data[, timeInStateShift3_numbPaymentShiftMore2 := timeInStateShift3_numbPaymentShift3 + timeInStateShift3_numbPaymentShift2and4]


modeltrans5 <- glm(transType ~ -1 + timeInStateShift1_numbPaymentShift0 + timeInStateShift2_numbPaymentShift0 + timeInStateShift3_numbPaymentShift0 + timeInStateShift1_numbPaymentShift1 + timeInStateShift3_numbPaymentShift1 + timeInStateShift1_numbPaymentShift2 + timeInStateShift3_numbPaymentShiftMore2 + timeInStateShift1_numbPaymentShift3and4+ timeInStateShift2, data = data, family = binomial(link = logit))
summary(modeltrans5)


anova(modeltrans4, modeltrans5, test="Chisq")

data[, timeInStateShift2_1and3_numbPaymentShiftMore1_1 := timeInStateShift1_numbPaymentShift1 + timeInStateShift3_numbPaymentShift1 + timeInStateShift2]

modeltrans6 <- glm(transType ~ -1 + timeInStateShift1_numbPaymentShift0 + timeInStateShift2_numbPaymentShift0 + timeInStateShift3_numbPaymentShift0 + timeInStateShift2_1and3_numbPaymentShiftMore1_1 + timeInStateShift1_numbPaymentShift2 + timeInStateShift3_numbPaymentShiftMore2 + timeInStateShift1_numbPaymentShift3and4, data = data, family = binomial(link = logit))
summary(modeltrans6)

anova(modeltrans5, modeltrans6, test="Chisq")

data[, timeInStateShiftMore2_numbPaymentShift0 := timeInStateShift2_numbPaymentShift0 + timeInStateShift3_numbPaymentShift0]

modeltrans7 <- glm(transType ~ -1 + timeInStateShift1_numbPaymentShift0 + timeInStateShiftMore2_numbPaymentShift0 + timeInStateShift2_1and3_numbPaymentShiftMore1_1 + timeInStateShift1_numbPaymentShift2 + timeInStateShift3_numbPaymentShiftMore2 + timeInStateShift1_numbPaymentShift3and4, data = data, family = binomial(link = logit))
summary(modeltrans7)

anova(modeltrans6, modeltrans7, test="Chisq")

tabTransProbs <- unique(data.table(data$timeInStateShift, data$numbPaymentShift, modeltrans7$fitted.values))
names(tabTransProbs) <- c("timeInStatePrev", "paymentStatePrev", "transProba")
tabTransProbs <- tabTransProbs[order(paymentStatePrev, timeInStatePrev),]

# On mod?lise les probabilit?s de transiter soit vers l'?tat paiement suivant soit vers l'?tat terminal. Pour cela, on s?lectionne les lignes de la DB correspondant ? une transition (transType = 1) et on construit la variable binaire 0 si le sinistre ne se cloture pas et 1 s'il se cloture.

dataTerm <- data[transType == 1, ]
dataTerm[, transTerm := 0]
dataTerm[(settlementYear == accidentYear + developmentYear - 1), transTerm := 1]
dataTerm[, timeInStateShift1_numbPaymentShift0 := 0]
dataTerm[timeInStateShift == 1 & numbPaymentShift == 0, timeInStateShift1_numbPaymentShift0 := 1]
dataTerm[, timeInStateShift2_numbPaymentShift0 := 0]
dataTerm[timeInStateShift == 2 & numbPaymentShift == 0, timeInStateShift2_numbPaymentShift0 := 1]
dataTerm[, timeInStateShift3_numbPaymentShift0 := 0]
dataTerm[timeInStateShift == 3 & numbPaymentShift == 0, timeInStateShift3_numbPaymentShift0 := 1]
dataTerm[, timeInStateShift1_numbPaymentShift1 := 0]
dataTerm[timeInStateShift == 1 & numbPaymentShift == 1, timeInStateShift1_numbPaymentShift1 := 1]
dataTerm[, timeInStateShift2_numbPaymentShift1 := 0]
dataTerm[timeInStateShift == 2 & numbPaymentShift == 1, timeInStateShift2_numbPaymentShift1 := 1]
dataTerm[, timeInStateShift3_numbPaymentShift1 := 0]
dataTerm[timeInStateShift == 3 & numbPaymentShift == 1, timeInStateShift3_numbPaymentShift1 := 1]
dataTerm[, timeInStateShift1_numbPaymentShift2 := 0]
dataTerm[timeInStateShift == 1 & numbPaymentShift == 2, timeInStateShift1_numbPaymentShift2 := 1]
dataTerm[, timeInStateShift2_numbPaymentShift2 := 0]
dataTerm[timeInStateShift == 2 & numbPaymentShift == 2, timeInStateShift2_numbPaymentShift2 := 1]
dataTerm[, timeInStateShift3_numbPaymentShift2 := 0]
dataTerm[timeInStateShift == 3 & numbPaymentShift == 2, timeInStateShift3_numbPaymentShift2 := 1]
dataTerm[, timeInStateShift1_numbPaymentShift3 := 0]
dataTerm[timeInStateShift == 1 & numbPaymentShift == 3, timeInStateShift1_numbPaymentShift3 := 1]
dataTerm[, timeInStateShift2_numbPaymentShift3 := 0]
dataTerm[timeInStateShift == 2 & numbPaymentShift == 3, timeInStateShift2_numbPaymentShift3 := 1]
dataTerm[, timeInStateShift3_numbPaymentShift3 := 0]
dataTerm[timeInStateShift == 3 & numbPaymentShift == 3, timeInStateShift3_numbPaymentShift3 := 1]
dataTerm[, timeInStateShift1_numbPaymentShift4 := 0]
dataTerm[timeInStateShift == 1 & numbPaymentShift == 4, timeInStateShift1_numbPaymentShift4 := 1]
dataTerm[, timeInStateShift2_numbPaymentShift4 := 0]
dataTerm[timeInStateShift == 2 & numbPaymentShift == 4, timeInStateShift2_numbPaymentShift4 := 1]
dataTerm[, timeInStateShift3_numbPaymentShift4 := 0]
dataTerm[timeInStateShift == 3 & numbPaymentShift == 4, timeInStateShift3_numbPaymentShift4 := 1]

dataTerm[, test := dataTerm$timeInStateShift1_numbPaymentShift0 + dataTerm$timeInStateShift2_numbPaymentShift0+ dataTerm$timeInStateShift3_numbPaymentShift0 + dataTerm$timeInStateShift1_numbPaymentShift1 + dataTerm$timeInStateShift2_numbPaymentShift1 + dataTerm$timeInStateShift3_numbPaymentShift1 + dataTerm$timeInStateShift1_numbPaymentShift2+dataTerm$timeInStateShift2_numbPaymentShift2+dataTerm$timeInStateShift3_numbPaymentShift2 + dataTerm$timeInStateShift1_numbPaymentShift3+dataTerm$timeInStateShift2_numbPaymentShift3+dataTerm$timeInStateShift3_numbPaymentShift3 + dataTerm$timeInStateShift1_numbPaymentShift4 + dataTerm$timeInStateShift2_numbPaymentShift4 + dataTerm$timeInStateShift3_numbPaymentShift4]

asFactorDT(dataTerm, "numbPaymentShift")
asFactorDT(dataTerm, "timeInStateShift")
modelterm1 <- glm(transTerm ~ -1 + timeInStateShift1_numbPaymentShift0 + timeInStateShift2_numbPaymentShift0 + timeInStateShift3_numbPaymentShift0 + timeInStateShift1_numbPaymentShift1 + timeInStateShift2_numbPaymentShift1 + timeInStateShift3_numbPaymentShift1 + timeInStateShift1_numbPaymentShift2 + timeInStateShift2_numbPaymentShift2 + timeInStateShift3_numbPaymentShift2 + timeInStateShift1_numbPaymentShift3 + timeInStateShift2_numbPaymentShift3 + timeInStateShift3_numbPaymentShift3 + timeInStateShift1_numbPaymentShift4 + timeInStateShift2_numbPaymentShift4 + timeInStateShift3_numbPaymentShift4, data = dataTerm, family = binomial(link = logit))
summary(modelterm1)
modelterm2 <- glm(transTerm ~ -1 + timeInStateShift1_numbPaymentShift0 + timeInStateShift2_numbPaymentShift0 + timeInStateShift3_numbPaymentShift0 + timeInStateShift1_numbPaymentShift1 + timeInStateShift2_numbPaymentShift1 + timeInStateShift3_numbPaymentShift1 + timeInStateShift1_numbPaymentShift2 + timeInStateShift2_numbPaymentShift2 + timeInStateShift3_numbPaymentShift2 + timeInStateShift1_numbPaymentShift3 + timeInStateShift2_numbPaymentShift3 + timeInStateShift3_numbPaymentShift3 + 
                    timeInStateShift1_numbPaymentShift4 + timeInStateShift2_numbPaymentShift4 + timeInStateShift3_numbPaymentShift4, data = dataTerm, family = binomial(link = logit))
summary(modelterm2)

dataTerm[, timeInStateShift2_numbPaymentShiftMore3 := timeInStateShift2_numbPaymentShift3 + timeInStateShift2_numbPaymentShift4]

modelterm3 <- glm(transTerm ~ -1 + timeInStateShift1_numbPaymentShift0 + timeInStateShift2_numbPaymentShift0 + timeInStateShift3_numbPaymentShift0 + timeInStateShift1_numbPaymentShift1 + timeInStateShift2_numbPaymentShift1 + timeInStateShift3_numbPaymentShift1 + timeInStateShift1_numbPaymentShift2 + timeInStateShift2_numbPaymentShift2 + timeInStateShift3_numbPaymentShift2 + timeInStateShift1_numbPaymentShift3 + timeInStateShift2_numbPaymentShiftMore3 +  timeInStateShift3_numbPaymentShift3 + 
                    timeInStateShift1_numbPaymentShift4 + timeInStateShift3_numbPaymentShift4, data = dataTerm, family = binomial(link = logit))
summary(modelterm3)
anova(modelterm3, modelterm2, test="Chisq")

dataTerm[, timeInStateShift2_1_3_numbPaymentShiftMore3_3_4 := timeInStateShift2_numbPaymentShiftMore3 + timeInStateShift1_numbPaymentShift3 + timeInStateShift3_numbPaymentShift4]

modelterm4 <- glm(transTerm ~ -1 + timeInStateShift1_numbPaymentShift0 + timeInStateShift2_numbPaymentShift0 + timeInStateShift3_numbPaymentShift0 + timeInStateShift1_numbPaymentShift1 + timeInStateShift2_numbPaymentShift1 + timeInStateShift3_numbPaymentShift1 + timeInStateShift1_numbPaymentShift2 + timeInStateShift2_numbPaymentShift2 + timeInStateShift3_numbPaymentShift2 +  timeInStateShift3_numbPaymentShift3 + 
                    timeInStateShift1_numbPaymentShift4 + timeInStateShift2_1_3_numbPaymentShiftMore3_3_4, data = dataTerm, family = binomial(link = logit))
summary(modelterm4)
anova(modelterm4, modelterm3, test="Chisq")

dataTerm[, timeInStateShift1and3_numbPaymentShift4and3 := timeInStateShift1_numbPaymentShift4 + timeInStateShift3_numbPaymentShift3]

modelterm5 <- glm(transTerm ~ -1 + timeInStateShift1_numbPaymentShift0 + timeInStateShift2_numbPaymentShift0 + timeInStateShift3_numbPaymentShift0 + timeInStateShift1_numbPaymentShift1 + timeInStateShift2_numbPaymentShift1 + timeInStateShift3_numbPaymentShift1 + timeInStateShift1_numbPaymentShift2 + timeInStateShift2_numbPaymentShift2 + timeInStateShift3_numbPaymentShift2 + 
                    timeInStateShift1and3_numbPaymentShift4and3 + timeInStateShift2_1_3_numbPaymentShiftMore3_3_4, data = dataTerm, family = binomial(link = logit))
summary(modelterm5)
anova(modelterm5, modelterm4, test="Chisq")

dataTerm[, timeInStateShift1_numbPaymentShiftLess2 := timeInStateShift1_numbPaymentShift1 + timeInStateShift1_numbPaymentShift2]

modelterm6 <- glm(transTerm ~ -1 + timeInStateShift1_numbPaymentShift0 + timeInStateShift2_numbPaymentShift0 + timeInStateShift3_numbPaymentShift0 + timeInStateShift1_numbPaymentShiftLess2 + timeInStateShift2_numbPaymentShift1 + timeInStateShift3_numbPaymentShift1 + timeInStateShift2_numbPaymentShift2 + timeInStateShift3_numbPaymentShift2 + 
                    timeInStateShift1and3_numbPaymentShift4and3 + timeInStateShift2_1_3_numbPaymentShiftMore3_3_4, data = dataTerm, family = binomial(link = logit))
summary(modelterm6)
anova(modelterm6, modelterm5, test="Chisq")

dataTerm[, timeInStateShift3_numbPaymentShift0and2 := timeInStateShift3_numbPaymentShift0 + timeInStateShift3_numbPaymentShift2]

modelterm7 <- glm(transTerm ~ -1 + timeInStateShift1_numbPaymentShift0 + timeInStateShift2_numbPaymentShift0 + timeInStateShift3_numbPaymentShift0and2 + timeInStateShift1_numbPaymentShiftLess2 + timeInStateShift2_numbPaymentShift1 + timeInStateShift3_numbPaymentShift1 + timeInStateShift2_numbPaymentShift2 +timeInStateShift1and3_numbPaymentShift4and3 + timeInStateShift2_1_3_numbPaymentShiftMore3_3_4, data = dataTerm, family = binomial(link = logit))
summary(modelterm7)
anova(modelterm7, modelterm6, test="Chisq")

# dataTerm[, timeInStateShift2_1_3_numbPaymentShiftMore3 := timeInStateShift1and3_numbPaymentShift4and3 + timeInStateShift2_1_3_numbPaymentShiftMore3_3_4]
# 
# modelterm8 <- glm(transTerm ~ -1 + timeInStateShift2_numbPaymentShift0 + timeInStateShift3_numbPaymentShift0and2 + timeInStateShift1_numbPaymentShiftLess2 + timeInStateShift2_numbPaymentShift1 + timeInStateShift3_numbPaymentShift1 + timeInStateShift2_numbPaymentShift2 + timeInStateShift2_1_3_numbPaymentShiftMore3, data = dataTerm, family = binomial(link = logit))
# summary(modelterm8)
# anova(modelterm8, modelterm7, test="Chisq")
# 
dataTerm[, timeInStateShiftMore2_3_numbPaymentShift0_2 := timeInStateShift2_numbPaymentShift0 + timeInStateShift3_numbPaymentShift0and2]

modelterm8 <- glm(transTerm ~ -1 + timeInStateShift1_numbPaymentShift0 +  timeInStateShiftMore2_3_numbPaymentShift0_2 + timeInStateShift1_numbPaymentShiftLess2 + timeInStateShift2_numbPaymentShift1 + timeInStateShift3_numbPaymentShift1 + timeInStateShift2_numbPaymentShift2 +timeInStateShift1and3_numbPaymentShift4and3 + timeInStateShift2_1_3_numbPaymentShiftMore3_3_4, data = dataTerm, family = binomial(link = logit))
summary(modelterm8)
anova(modelterm8, modelterm7, test="Chisq")

# On affiche les r?sultats. La probabilit? pour un sinistre de se cl?turer apr?s avoir pass? moins d'un an dans S0 est nulle car il faut au minimum 2 ans avant qu'un sinistre puisse se cl?turer.

tabTerm <- unique(data.table(dataTerm$timeInStateShift, dataTerm$numbPaymentShift, modelterm8$fitted.values))
names(tabTerm) <- c("timeInStatePrev", "paymentStatePrev", "transProbaTerm")
tabTerm <- tabTerm[order(paymentStatePrev, timeInStatePrev),]

# Premier paiement 

library(gamlss)
library(gamlss.mx)

data[is.na(numbPaymentShift), numbPaymentShift := levels(data$numbPaymentShift )[1]]
data[is.na(timeInStateShift), timeInStateShift := 1]
data[is.na(paymentShift ), paymentShift := 0]

dataPay <- data[payment > 0,]
dataPay[,  firstPay := first(payment), by = c('numClaims')]
# dataFirstPayPareto <- dataPay[payment == firstPay & numbPayment == 1,]
# 
# dataSecondPayPareto <- dataPay[numbPayment == 2,]
#  library(evmix)
#  thres <- hillplot(dataFirstPayPareto$payment, y.alpha = TRUE, ylim = c(-2,10))
#  hillplot(dataFirstPayPareto$payment, y.alpha = TRUE, x.theta = TRUE, ylim = c(-2,10))
#  hillplot(dataFirstPayPareto$payment, y.alpha = TRUE, hill.type = "SmooHill", ylim = c(-2,10))
#  library(evir)
#  threshold <- hill(dataFirstPayPareto$payment)
# paret <- gpd(dataFirstPayPareto$payment, threshold = 6700)
# evir::meplot(dataFirstPayPareto$payment)
# par <- evir::rgpd(1000000, paret$par.ests[1], mu = 6700, beta = paret$par.ests[2])
# dataFirstPay <- dataPay[payment == firstPay & numbPayment == 1 & payment <= 6700, ]
# dataPareto <- dataPay[payment == firstPay & numbPayment == 1 & payment > 6700, ]
# dataPareto[, logPayment := log(payment)]
# dataPareto[, linkRatio := NULL]
# modPar <- gamlss(logPayment ~ -1 + as.factor(timeInStateShift), data = dataPareto, family = "GA")



dataFirstPay <- copy(dataPay)
dataFirstPay[, linkRatio := NULL]



asFactorDT(dataFirstPay, 'timeInStateShift')
firstPayMod1 <- gamlssMXfits(n = 10, payment~ -1 + timeInStateShift, data = dataFirstPay, family = c(LOGNO, LOGNO), K = 2)
summary(firstPayMod1$models[[1]])
summary(firstPayMod1$models[[2]])


dataFirstPay[, timeInStateShift1 := 0]
dataFirstPay[, timeInStateShift2 := 0]
dataFirstPay[, timeInStateShift3 := 0]
dataFirstPay[timeInStateShift == 1, timeInStateShift1 := 1]
dataFirstPay[timeInStateShift == 2, timeInStateShift2 := 1]
dataFirstPay[timeInStateShift == 3, timeInStateShift3 := 1]

dataFirstPay[, timeInStateShiftMore2 := timeInStateShift2 + timeInStateShift3]
firstPayMod3 <- gamlssMXfits(n = 10, list(payment~ -1 + timeInStateShift1 + timeInStateShiftMore2, payment~ -1 + timeInStateShift1 + timeInStateShiftMore2), data = dataFirstPay, family = c(LOGNO, LOGNO), K = 2)
summary(firstPayMod3$models[[1]])
summary(firstPayMod3$models[[2]])

tabFirstPayLogNo1 <- unique(data.table(dataFirstPay$timeInStateShift, round(exp(firstPayMod1$models[[1]]$mu.fv + 0.5*firstPayMod3$models[[1]]$sigma.fv^2),6)))
names(tabFirstPayLogNo1) <- c("timeInStatePrev", "firstPay")
tabFirstPayLogNo1 <- tabFirstPayLogNo1[order(timeInStatePrev),]


tabFirstPayLogNo2 <- unique(data.table(dataFirstPay$timeInStateShift, round(exp(firstPayMod1$models[[2]]$mu.fv + 0.5*firstPayMod3$models[[2]]$sigma.fv^2),6)))
names(tabFirstPayLogNo2) <- c("timeInStatePrev", "firstPay")
tabFirstPayLogNo2 <- tabFirstPayLogNo2[order(timeInStatePrev),]

# Link ratios

dataLR <- data[linkRatio > 0 & linkRatio != 1 & numbPayment >=2, ]

asFactorDT(dataLR, 'timeInStateShift')
asFactorDT(dataLR, 'numbPaymentShift')
dataListLR <- list()
for(i in 1:length(unique(dataLR$numbPaymentShift))){
  dataListLR[[i]] <- dataLR[numbPaymentShift == i, ]
}

dataListLR[[1]][, lowPay := 1]
dataListLR[[1]][paymentShift > 5000, lowPay := 0]
dataListLR[[1]][, timeInStateShift1_lowPay0 := 0]
dataListLR[[1]][timeInStateShift == 1 & lowPay == 0, timeInStateShift1_lowPay0 := 1]
dataListLR[[1]][, timeInStateShift2_lowPay0 := 0]
dataListLR[[1]][timeInStateShift == 2 & lowPay == 0, timeInStateShift2_lowPay0 := 1]
dataListLR[[1]][, timeInStateShift3_lowPay0 := 0]
dataListLR[[1]][timeInStateShift == 3 & lowPay == 0, timeInStateShift3_lowPay0 := 1]
dataListLR[[1]][, timeInStateShift1_lowPay1 := 0]
dataListLR[[1]][timeInStateShift == 1 & lowPay == 1, timeInStateShift1_lowPay1 := 1]
dataListLR[[1]][, timeInStateShift2_lowPay1 := 0]
dataListLR[[1]][timeInStateShift == 2 & lowPay == 1, timeInStateShift2_lowPay1 := 1]
dataListLR[[1]][, timeInStateShift3_lowPay1 := 0]
dataListLR[[1]][timeInStateShift == 3 & lowPay == 1, timeInStateShift3_lowPay1 := 1]


modelLR1_numbPayment1 <- gamlssMXfits(n = 10, list(linkRatio ~ -1 + timeInStateShift1_lowPay0 + timeInStateShift2_lowPay0 + timeInStateShift3_lowPay0 + timeInStateShift1_lowPay1 + timeInStateShift2_lowPay1 + timeInStateShift3_lowPay1, linkRatio ~ -1 + timeInStateShift1_lowPay0 + timeInStateShift2_lowPay0 + timeInStateShift3_lowPay0 + timeInStateShift1_lowPay1 + timeInStateShift2_lowPay1 + timeInStateShift3_lowPay1), data = dataListLR[[1]], family = c(LOGNO, LOGNO), K = 2)
summary(modelLR1_numbPayment1$models[[1]])
summary(modelLR1_numbPayment1$models[[2]])

modelLR2_numbPayment1 <- gamlssMXfits(n = 10, list(linkRatio ~ -1 + as.factor(lowPay), linkRatio ~ -1 + timeInStateShift1_lowPay0 + timeInStateShift2_lowPay0 + timeInStateShift3_lowPay0 + timeInStateShift1_lowPay1 + timeInStateShift2_lowPay1 + timeInStateShift3_lowPay1), data = dataListLR[[1]], family = c(LOGNO, LOGNO), K = 2)
summary(modelLR2_numbPayment1$models[[1]])
summary(modelLR2_numbPayment1$models[[2]])

# modelLR3_numbPayment1 <- gamlssMXfits(n = 10, list(linkRatio ~ 1, linkRatio ~ -1 + as.factor(timeInStateShift)), data = dataListLR[[1]], family = c(LOGNO, PARETO), K = 2)
# summary(modelLR3_numbPayment1$models[[1]])
# summary(modelLR3_numbPayment1$models[[2]])

tabLRNumbPay1LogNo <- unique(data.table(dataListLR[[1]]$timeInStateShift, dataListLR[[1]]$lowPay, round(exp(modelLR2_numbPayment1$models[[1]]$mu.fv + 0.5*modelLR2_numbPayment1$models[[1]]$sigma.fv^2),6)))
names(tabLRNumbPay1LogNo) <- c("timeInStatePrev", "paymentPrev", "LR")
tabLRNumbPay1LogNo <- tabLRNumbPay1LogNo[order(timeInStatePrev, paymentPrev),]

tabLRNumbPay1Pareto <- unique(data.table(dataListLR[[1]]$timeInStateShift, dataListLR[[1]]$lowPay, round(exp(modelLR2_numbPayment1$models[[2]]$mu.fv + 0.5*modelLR2_numbPayment1$models[[2]]$sigma.fv^2),6)))
names(tabLRNumbPay1Pareto) <- c("timeInStatePrev", "paymentPrev", "LR")
tabLRNumbPay1Pareto <- tabLRNumbPay1Pareto[order(timeInStatePrev, paymentPrev),]

dataListLR[[2]][, lowPay := 1]
dataListLR[[2]][paymentShift > 5000, lowPay := 0]
dataListLR[[2]][, timeInStateShift1_lowPay0 := 0]
dataListLR[[2]][timeInStateShift == 1 & lowPay == 0, timeInStateShift1_lowPay0 := 1]
dataListLR[[2]][, timeInStateShift2_lowPay0 := 0]
dataListLR[[2]][timeInStateShift == 2 & lowPay == 0, timeInStateShift2_lowPay0 := 1]
dataListLR[[2]][, timeInStateShift3_lowPay0 := 0]
dataListLR[[2]][timeInStateShift == 3 & lowPay == 0, timeInStateShift3_lowPay0 := 1]
dataListLR[[2]][, timeInStateShift1_lowPay1 := 0]
dataListLR[[2]][timeInStateShift == 1 & lowPay == 1, timeInStateShift1_lowPay1 := 1]
dataListLR[[2]][, timeInStateShift2_lowPay1 := 0]
dataListLR[[2]][timeInStateShift == 2 & lowPay == 1, timeInStateShift2_lowPay1 := 1]
dataListLR[[2]][, timeInStateShift3_lowPay1 := 0]
dataListLR[[2]][timeInStateShift == 3 & lowPay == 1, timeInStateShift3_lowPay1 := 1]

modelLR1_numbPayment2 <- gamlssMXfits(n = 10, list(linkRatio ~ -1 + timeInStateShift1_lowPay0 + timeInStateShift2_lowPay0 + timeInStateShift3_lowPay0 + timeInStateShift1_lowPay1 + timeInStateShift2_lowPay1 + timeInStateShift3_lowPay1, linkRatio ~ -1 + timeInStateShift1_lowPay0 + timeInStateShift2_lowPay0 + timeInStateShift3_lowPay0 + timeInStateShift1_lowPay1 + timeInStateShift2_lowPay1 + timeInStateShift3_lowPay1), data = dataListLR[[2]], family = c(LOGNO, LOGNO), K = 2)
summary(modelLR1_numbPayment2$models[[1]])
summary(modelLR1_numbPayment2$models[[2]])


dataListLR[[2]][, timeInStateShiftMore2_lowPay0 := timeInStateShift2_lowPay0 + timeInStateShift3_lowPay0]
dataListLR[[2]][, timeInStateShiftMore2_lowPay1 := timeInStateShift2_lowPay1 + timeInStateShift3_lowPay1]

modelLR2_numbPayment2 <- gamlssMXfits(n = 10, list(linkRatio ~ -1 + timeInStateShift1_lowPay0 + timeInStateShiftMore2_lowPay0 + timeInStateShift1_lowPay1 + timeInStateShift2_lowPay1 + timeInStateShift3_lowPay1, linkRatio ~ -1 + timeInStateShift1_lowPay0 + timeInStateShift2_lowPay0 + timeInStateShift3_lowPay0 + timeInStateShift1_lowPay1 + timeInStateShiftMore2_lowPay1), data = dataListLR[[2]], family = c(LOGNO, LOGNO), K = 2)
summary(modelLR2_numbPayment2$models[[1]])
summary(modelLR2_numbPayment2$models[[2]])

tabLRNumbPay2LogNo <- unique(data.table(dataListLR[[2]]$timeInStateShift, dataListLR[[2]]$lowPay, round(exp(modelLR2_numbPayment2$models[[1]]$mu.fv + 0.5*modelLR2_numbPayment2$models[[1]]$sigma.fv^2),6)))
names(tabLRNumbPay2LogNo) <- c("timeInStatePrev", "paymentPrev", "LR")
tabLRNumbPay2LogNo <- tabLRNumbPay2LogNo[order(timeInStatePrev, paymentPrev),]

tabLRNumbPay2Pareto <- unique(data.table(dataListLR[[2]]$timeInStateShift, dataListLR[[2]]$lowPay, round(exp(modelLR2_numbPayment2$models[[2]]$mu.fv + 0.5*modelLR2_numbPayment2$models[[2]]$sigma.fv^2),6)))
names(tabLRNumbPay2Pareto) <- c("timeInStatePrev", "paymentPrev", "LR")
tabLRNumbPay2Pareto <- tabLRNumbPay2Pareto[order(timeInStatePrev, paymentPrev),]

dataListLR[[3]][, lowPay := 1]
dataListLR[[3]][paymentShift > 5000, lowPay := 0]
dataListLR[[3]][, timeInStateShift1_lowPay0 := 0]
dataListLR[[3]][timeInStateShift == 1 & lowPay == 0, timeInStateShift1_lowPay0 := 1]
dataListLR[[3]][, timeInStateShift2_lowPay0 := 0]
dataListLR[[3]][timeInStateShift == 2 & lowPay == 0, timeInStateShift2_lowPay0 := 1]
dataListLR[[3]][, timeInStateShift3_lowPay0 := 0]
dataListLR[[3]][timeInStateShift == 3 & lowPay == 0, timeInStateShift3_lowPay0 := 1]
dataListLR[[3]][, timeInStateShift1_lowPay1 := 0]
dataListLR[[3]][timeInStateShift == 1 & lowPay == 1, timeInStateShift1_lowPay1 := 1]
dataListLR[[3]][, timeInStateShift2_lowPay1 := 0]
dataListLR[[3]][timeInStateShift == 2 & lowPay == 1, timeInStateShift2_lowPay1 := 1]
dataListLR[[3]][, timeInStateShift3_lowPay1 := 0]
dataListLR[[3]][timeInStateShift == 3 & lowPay == 1, timeInStateShift3_lowPay1 := 1]

modelLR1_numbPayment3 <- gamlssMXfits(n = 10, list(linkRatio ~ -1 + timeInStateShift1_lowPay0 + timeInStateShift2_lowPay0 + timeInStateShift3_lowPay0 + timeInStateShift1_lowPay1 + timeInStateShift2_lowPay1 + timeInStateShift3_lowPay1, linkRatio ~ -1 + timeInStateShift1_lowPay0 + timeInStateShift2_lowPay0 + timeInStateShift3_lowPay0 + timeInStateShift1_lowPay1 + timeInStateShift2_lowPay1 + timeInStateShift3_lowPay1), data = dataListLR[[3]], family = c(LOGNO, LOGNO), K = 2)
summary(modelLR1_numbPayment3$models[[1]])
summary(modelLR1_numbPayment3$models[[2]])

dataListLR[[3]][, timeInStateShiftLess2_lowPay0 := timeInStateShift1_lowPay0 + timeInStateShift2_lowPay0]
dataListLR[[3]][, timeInStateShiftLess2_lowPay1 := timeInStateShift1_lowPay1 + timeInStateShift2_lowPay1]
dataListLR[[3]][, timeInStateShift3 := timeInStateShift3_lowPay0 + timeInStateShift3_lowPay1]

modelLR2_numbPayment3 <- gamlssMXfits(n = 10, list(linkRatio ~ -1 + timeInStateShift1_lowPay0 + timeInStateShift2_lowPay0 + timeInStateShift3_lowPay0 + timeInStateShift1_lowPay1 + timeInStateShift2_lowPay1 + timeInStateShift3_lowPay1, linkRatio ~ -1 + timeInStateShiftLess2_lowPay0 + timeInStateShiftLess2_lowPay1 + timeInStateShift3), data = dataListLR[[3]], family = c(LOGNO, LOGNO), K = 2)
summary(modelLR2_numbPayment3$models[[1]])
summary(modelLR2_numbPayment3$models[[2]])


tabLRNumbPay3LogNo <- unique(data.table(dataListLR[[3]]$timeInStateShift, dataListLR[[3]]$lowPay, round(exp(modelLR2_numbPayment3$models[[1]]$mu.fv + 0.5*modelLR2_numbPayment3$models[[1]]$sigma.fv^2),6)))
names(tabLRNumbPay3LogNo) <- c("timeInStatePrev", "paymentPrev", "LR")
tabLRNumbPay3LogNo <- tabLRNumbPay3LogNo[order(timeInStatePrev, paymentPrev),]
if(nrow(tabLRNumbPay3LogNo) < 6){
tabLRNumbPay3LogNo <- rbind(tabLRNumbPay3LogNo ,data.table(timeInStatePrev = "3",paymentPrev = "1",LR = 1))# Jordy: sometimes you can have that one level is missing for lowPay, then you need to add it 
}

tabLRNumbPay3Pareto <- unique(data.table(dataListLR[[3]]$timeInStateShift, dataListLR[[3]]$lowPay, round(exp(modelLR2_numbPayment3$models[[2]]$mu.fv + 0.5*modelLR2_numbPayment3$models[[2]]$sigma.fv^2),6)))
names(tabLRNumbPay3Pareto) <- c("timeInStatePrev", "paymentPrev", "LR")
tabLRNumbPay3Pareto <- tabLRNumbPay3Pareto[order(timeInStatePrev, paymentPrev),]

if(nrow(tabLRNumbPay3Pareto) <6 ){
tabLRNumbPay3Pareto <- rbind(tabLRNumbPay3Pareto,data.table(timeInStatePrev = "3",paymentPrev = "1",LR = 1) )
}

dataListLR[[4]][, lowPay := 1]
dataListLR[[4]][paymentShift > 5000, lowPay := 0]
dataListLR[[4]][, timeInStateShift1_lowPay0 := 0]
dataListLR[[4]][timeInStateShift == 1 & lowPay == 0, timeInStateShift1_lowPay0 := 1]
dataListLR[[4]][, timeInStateShift2_lowPay0 := 0]
dataListLR[[4]][timeInStateShift == 2 & lowPay == 0, timeInStateShift2_lowPay0 := 1]
dataListLR[[4]][, timeInStateShift3_lowPay0 := 0]
dataListLR[[4]][timeInStateShift == 3 & lowPay == 0, timeInStateShift3_lowPay0 := 1]
dataListLR[[4]][, timeInStateShift1_lowPay1 := 0]
dataListLR[[4]][timeInStateShift == 1 & lowPay == 1, timeInStateShift1_lowPay1 := 1]
dataListLR[[4]][, timeInStateShift2_lowPay1 := 0]
dataListLR[[4]][timeInStateShift == 2 & lowPay == 1, timeInStateShift2_lowPay1 := 1]
dataListLR[[4]][, timeInStateShift3_lowPay1 := 0]
dataListLR[[4]][timeInStateShift == 3 & lowPay == 1, timeInStateShift3_lowPay1 := 1]

modelLR1_numbPayment4 <- gamlssMXfits(n = 10, list(linkRatio ~ -1 + timeInStateShift1_lowPay0 + timeInStateShift2_lowPay0 + timeInStateShift3_lowPay0 + timeInStateShift1_lowPay1 + timeInStateShift2_lowPay1 + timeInStateShift3_lowPay1, linkRatio ~ -1 + timeInStateShift1_lowPay0 + timeInStateShift2_lowPay0 + timeInStateShift3_lowPay0 + timeInStateShift1_lowPay1 + timeInStateShift2_lowPay1 + timeInStateShift3_lowPay1), data = dataListLR[[4]], family = c(LOGNO, LOGNO), K = 2)
summary(modelLR1_numbPayment4$models[[1]])
summary(modelLR1_numbPayment4$models[[2]])

dataListLR[[4]][, timeInStateShift3and2_lowPay0and1 := timeInStateShift3_lowPay0 + timeInStateShift2_lowPay1]
dataListLR[[4]][, timeInStateShiftLess2_lowPay0 := timeInStateShift1_lowPay0 + timeInStateShift2_lowPay0]

modelLR2_numbPayment4 <- gamlssMXfits(n = 10, list(linkRatio ~ -1 + timeInStateShiftLess2_lowPay0 + timeInStateShift3_lowPay0 + timeInStateShift1_lowPay1 + timeInStateShift2_lowPay1 + timeInStateShift3_lowPay1, linkRatio ~ -1 + timeInStateShift1_lowPay0 + timeInStateShift2_lowPay0 + timeInStateShift1_lowPay1 + timeInStateShift3and2_lowPay0and1 + timeInStateShift3_lowPay1), data = dataListLR[[4]], family = c(LOGNO, LOGNO), K = 2)
summary(modelLR2_numbPayment4$models[[1]])
summary(modelLR2_numbPayment4$models[[2]])

dataListLR[[4]][, timeInStateShift3and1and2_lowPay0and0and1 := timeInStateShift3_lowPay0 + timeInStateShift1_lowPay0 + timeInStateShift2_lowPay1]

modelLR3_numbPayment4 <- gamlssMXfits(n = 10, list(linkRatio ~ -1 + timeInStateShiftLess2_lowPay0 + timeInStateShift3_lowPay0 + timeInStateShift1_lowPay1 + timeInStateShift2_lowPay1 + timeInStateShift3_lowPay1, linkRatio ~ -1 + timeInStateShift2_lowPay0 + timeInStateShift1_lowPay1 + timeInStateShift3and1and2_lowPay0and0and1 + timeInStateShift3_lowPay1), data = dataListLR[[4]], family = c(LOGNO, LOGNO), K = 2)
summary(modelLR3_numbPayment4$models[[1]])
summary(modelLR3_numbPayment4$models[[2]])

tabLRNumbPay4LogNo <- unique(data.table(dataListLR[[4]]$timeInStateShift, dataListLR[[4]]$lowPay, round(exp(modelLR3_numbPayment4$models[[1]]$mu.fv + 0.5*modelLR3_numbPayment4$models[[1]]$sigma.fv^2),6)))
names(tabLRNumbPay4LogNo) <- c("timeInStatePrev", "paymentPrev", "LR")
tabLRNumbPay4LogNo <- tabLRNumbPay4LogNo[order(timeInStatePrev, paymentPrev),]

if(nrow(tabLRNumbPay4LogNo) < 6){
  tabLRNumbPay4LogNo <- rbind(tabLRNumbPay4LogNo,data.table(timeInStatePrev = "3",paymentPrev = "1",LR = 1) )
}


tabLRNumbPay4Pareto <- unique(data.table(dataListLR[[4]]$timeInStateShift, dataListLR[[4]]$lowPay, round(exp(modelLR3_numbPayment4$models[[2]]$mu.fv + 0.5*modelLR3_numbPayment4$models[[2]]$sigma.fv^2),6)))
names(tabLRNumbPay4Pareto) <- c("timeInStatePrev", "paymentPrev", "LR")
tabLRNumbPay4Pareto <- tabLRNumbPay4Pareto[order(timeInStatePrev, paymentPrev),]

if(nrow(tabLRNumbPay4Pareto) <6){
  tabLRNumbPay4Pareto <- rbind(tabLRNumbPay4Pareto,data.table(timeInStatePrev = "3",paymentPrev = "1",LR = 1) )
}

modelLRList <- list()
modelLRList[[1]] <- modelLR2_numbPayment1
modelLRList[[2]] <- modelLR2_numbPayment2
modelLRList[[3]] <- modelLR2_numbPayment3
modelLRList[[4]] <- modelLR3_numbPayment4

tabLRLogNo <- list()
tabLRLogNo[[1]] <- tabLRNumbPay1LogNo
tabLRLogNo[[2]] <- tabLRNumbPay2LogNo
tabLRLogNo[[3]] <- tabLRNumbPay3LogNo
tabLRLogNo[[4]] <- tabLRNumbPay4LogNo

tabLRPareto <- list()
tabLRPareto[[1]] <- tabLRNumbPay1Pareto
tabLRPareto[[2]] <- tabLRNumbPay2Pareto
tabLRPareto[[3]] <- tabLRNumbPay3Pareto
tabLRPareto[[4]] <- tabLRNumbPay4Pareto

dataProbNoPay <- dataTerm[transTerm == 1, ]
probNoPay1 <- glm(paymentFlag ~ -1 + timeInStateShift1_numbPaymentShift1 + timeInStateShift2_numbPaymentShift1 + timeInStateShift3_numbPaymentShift1 + timeInStateShift1_numbPaymentShift2 + timeInStateShift2_numbPaymentShift2 + timeInStateShift3_numbPaymentShift2 + timeInStateShift1_numbPaymentShift3 + timeInStateShift2_numbPaymentShift3 + timeInStateShift3_numbPaymentShift3 + timeInStateShift1_numbPaymentShift4 + timeInStateShift2_numbPaymentShift4 + timeInStateShift3_numbPaymentShift4, data = dataProbNoPay, family = binomial(link = "logit"))
summary(probNoPay1)

dataProbNoPay[, timeInStateShiftMore2_numbPaymentShift4 := timeInStateShift3_numbPaymentShift4 + timeInStateShift2_numbPaymentShift4]
dataProbNoPay[, timeInStateShiftMore2_numbPaymentShift3 := timeInStateShift3_numbPaymentShift3 + timeInStateShift2_numbPaymentShift3]
probNoPay2 <- glm(paymentFlag ~ -1 + timeInStateShift1_numbPaymentShift1 + timeInStateShift2_numbPaymentShift1 + timeInStateShift3_numbPaymentShift1 + timeInStateShift1_numbPaymentShift2 + timeInStateShift2_numbPaymentShift2 + timeInStateShift3_numbPaymentShift2 + timeInStateShift1_numbPaymentShift3 + timeInStateShiftMore2_numbPaymentShift3 + timeInStateShift1_numbPaymentShift4 + timeInStateShiftMore2_numbPaymentShift4, data = dataProbNoPay, family = binomial(link = "logit"))
summary(probNoPay2)

anova(probNoPay2, probNoPay1, test="Chisq")

dataProbNoPay[, timeInStateShiftMore2_numbPaymentShift2 := timeInStateShift3_numbPaymentShift2 + timeInStateShift2_numbPaymentShift2]
probNoPay3 <- glm(paymentFlag ~ -1 + timeInStateShift1_numbPaymentShift1 + timeInStateShift2_numbPaymentShift1 + timeInStateShift3_numbPaymentShift1 + timeInStateShift1_numbPaymentShift2 + timeInStateShiftMore2_numbPaymentShift2 + timeInStateShift1_numbPaymentShift3 + timeInStateShiftMore2_numbPaymentShift3 + timeInStateShift1_numbPaymentShift4 + timeInStateShiftMore2_numbPaymentShift4, data = dataProbNoPay, family = binomial(link = "logit"))
summary(probNoPay3)

anova(probNoPay3, probNoPay2, test="Chisq")

dataProbNoPay[, numbPaymentShift2 := timeInStateShift1_numbPaymentShift2 + timeInStateShiftMore2_numbPaymentShift2]
probNoPay4 <- glm(paymentFlag ~ -1 + timeInStateShift1_numbPaymentShift1 + timeInStateShift2_numbPaymentShift1 + timeInStateShift3_numbPaymentShift1 + numbPaymentShift2 + timeInStateShift1_numbPaymentShift3 + timeInStateShiftMore2_numbPaymentShift3 + timeInStateShift1_numbPaymentShift4 + timeInStateShiftMore2_numbPaymentShift4, data = dataProbNoPay, family = binomial(link = "logit"))
summary(probNoPay4)

anova(probNoPay4, probNoPay3, test="Chisq")

dataProbNoPay[, numbPaymentShift3 := timeInStateShift1_numbPaymentShift3 + timeInStateShiftMore2_numbPaymentShift3]
probNoPay5 <- glm(paymentFlag ~ -1 + timeInStateShift1_numbPaymentShift1 + timeInStateShift2_numbPaymentShift1 + timeInStateShift3_numbPaymentShift1 + numbPaymentShift2 + numbPaymentShift3 + timeInStateShift1_numbPaymentShift4 + timeInStateShiftMore2_numbPaymentShift4, data = dataProbNoPay, family = binomial(link = "logit"))
summary(probNoPay5)

anova(probNoPay5, probNoPay4, test="Chisq")


tabProbNoPay <- unique(data.table(dataProbNoPay$timeInStateShift, dataProbNoPay$numbPaymentShift, probNoPay5$fitted.values))
names(tabProbNoPay) <- c("timeInStatePrev", "paymentStatePrev", "ProbaNoPayTerm")
tabProbNoPay <- tabProbNoPay[order(paymentStatePrev, timeInStatePrev),]
tabProbNoPay[1:2,3] <- c(1,1)




# Censored claims

# library(ChainLadder)
# IBNR[, count := rep(1, nrow(IBNR))]
# IBNR[, repPeriod := reportingYear - accidentYear + 1]
# triangle <- as.triangle(IBNR, origin = "accidentYear", dev = "repPeriod", "count")
# 
# IBNRDataMod <-  as.data.frame(triangle, na.rm=TRUE)
# 
# regPoisson <- glm(IBNRDataMod$value ~ as.factor(IBNRDataMod$accidentYear) + as.factor(IBNRDataMod$repPeriod), family = poisson(link = log)) 
# summary(regPoisson)
# 
# nCalendar <- 10 
# coeffBeta <- regPoisson$coefficients[(nCalendar + 1):(nCalendar + 3)]
# const <- log(1/(1 + sum(exp(coeffBeta))))
# coeffBetaTranslate <- c(const, coeffBeta + const)
# coeffAlphaTranslate <- regPoisson$coefficients[1:nCalendar] - const
# 
# coeffBetaNorm <- exp(coeffBetaTranslate)
# sum(coeffBetaNorm) == 1
# 
# plot(coeffBetaNorm, type = "l", col ="blue", xlab = "reporting delay", ylab = "beta cofficients", main = "Beta coefficients as a function of the reporting delay")
# 
# coeffAlpha <- exp(coeffAlphaTranslate)
# coeffAlpha <- coeffAlpha*c(1, rep(coeffAlpha[1], (length(coeffAlpha) - 1)))
# 
# coeffBetaNorm2 <- c(coeffBetaNorm, rep(0,6))
# IBNRTriangle <- matrix(NA, nrow = nCalendar, ncol = nCalendar)
# 
# for(i in 1:nCalendar){
#   for(j in 1:nCalendar){
#     IBNRTriangle[i,j] <- coeffAlpha[i]*coeffBetaNorm2[j]
#   }
# }
# 
# rownames(IBNRTriangle) <- rownames(triangle)
# colnames(IBNRTriangle) <- c(colnames(triangle), 5:10)
# 
# repTriangleComplete <- triangle
# 
# for(i in 1:nCalendar){
#   for(j in 1:4){
#     if(is.na(repTriangleComplete[i,j])){
#       repTriangleComplete[i,j] <- IBNRTriangle[i,j]
#     }
#   }
# }
# 
# IBNRTriangle2 <- IBNRTriangle
# 
# for(i in 1:nCalendar){
#   for(j in 1:4){
#     if(i + j <= nCalendar + 1){
#       IBNRTriangle2[i,j] <- 0
#     }
#   }
# }
# 
# 
# numbIBNR <- round(sum(IBNRTriangle2))
# 
# trainDataBegin <- copy(IBNR)
# trainDataBegin <- trainDataBegin[sample(1:nrow(trainDataBegin), numbIBNR, replace = TRUE), ]
# trainDataBegin[, timeInState := 0]
# trainDataBegin[, payment := 0]
# trainDataBegin[, devPeriodCount := 0]
# dataIBNR <- data.table()
# 
# while(nrow(trainDataBegin) != 0){
#   trainDataBegin[, probSplit := sapply(1:nrow(trainDataBegin), function(xx) tabSplitTrans[timeInStatePrev == trainDataBegin$timeInState[xx], .(splitTrans)][[1]])]# assign the transition probabilities according to the time spent in the state
#   trainDataBegin[, predTrans := rbinom(nrow(trainDataBegin), 1, trainDataBegin$probSplit)]# simulate an indicator of transition 
#   
#   if(nrow(trainDataBegin[predTrans == 1, ]) > 0){# if there are claims that had a transition 
#     trainDataBegin[predTrans == 1, probTerm := sapply(1:nrow(trainDataBegin[predTrans == 1,]), function(xx) tabSplitTerm[timeInStatePrev == trainDataBegin[predTrans == 1,]$timeInState[xx], .(splitTerm)][[1]])]# assign probabilities to transition to a terminal state 
#     trainDataBegin[predTrans == 1, predTerm := rbinom(nrow(trainDataBegin[predTrans == 1,]), 1, trainDataBegin[predTrans == 1,]$probTerm)]# simulate indicator to transition to a terminal state 
#     if(nrow(trainDataBegin[predTerm == 0, ]) > 0){# if there are claims that did no go to a terminal state
#       trainDataBegin[predTrans == 1 & predTerm == 0, probPay := sapply(1:nrow(trainDataBegin[predTrans == 1 & predTerm == 0,]), function(xx) tabSplitPay[timeInStatePrev == trainDataBegin[predTrans == 1 & predTerm == 0,]$timeInState[xx], .(splitPay)][[1]])]# assign probabilities of habing a payment 
#       trainDataBegin[predTrans == 1 & predTerm == 0, predState := rbinom(nrow(trainDataBegin[predTrans == 1 & predTerm == 0,]), 1, trainDataBegin[predTrans == 1 & predTerm == 0,]$probPay)]# simulate indicator of if there was a transition to a state with payment
#       if(nrow(trainDataBegin[predState == 1, ]) > 0){# if there were claims that went to a next state with payment then we need to simulate payments 
#         trainDataBegin[predTrans == 1 & predTerm == 0 & predState == 1, predProbGam := rbinom(nrow(trainDataBegin[predTrans == 1 & predTerm == 0 & predState == 1,]), 1, payIBNR$prob[1])]# simulate indicator of payment 
#         if(nrow(trainDataBegin[predTrans == 1 & predTerm == 0 & predState == 1 & predProbGam == 1, ]) > 0){# if payments need to be simulated, simulate them 
#           trainDataBegin[predTrans == 1 & predTerm == 0 & predState == 1 & predProbGam == 1, payment := sapply(1:nrow(trainDataBegin[predTrans == 1 & predTerm == 0 & predState == 1 & predProbGam == 1,]), function(xx) tabPayIBNRLogNo1[timeInStatePrev == trainDataBegin[predTrans == 1 & predTerm == 0 & predState == 1 & predProbGam == 1,]$timeInState[xx], .(payIBNR)][[1]])]
#         }
#         if(nrow(trainDataBegin[predTrans == 1 & predTerm == 0 & predState == 1 & predProbGam == 0, ]) > 0){
#           trainDataBegin[predTrans == 1 & predTerm == 0 & predState == 1 & predProbGam == 0, payment := sapply(1:nrow(trainDataBegin[predTrans == 1 & predTerm == 0 & predState == 1 & predProbGam == 0,]), function(xx) tabPayIBNRLogNo2[timeInStatePrev == trainDataBegin[predTrans == 1 & predTerm == 0 & predState == 1 & predProbGam == 0,]$timeInState[xx], .(payIBNR)][[1]])]
#         }
#       }
#     } 
#     if(nrow(trainDataBegin[predTerm == 1, ]) > 0){# if there was a transition to a terminal state 
#       trainDataBegin[predTrans == 1 & predTerm == 1, probClose := sapply(1:nrow(trainDataBegin[predTrans == 1 & predTerm == 1,]), function(xx) tabSplitClose[timeInStatePrev == trainDataBegin[predTrans == 1 & predTerm == 1,]$timeInState[xx], .(splitClose)][[1]])]
#       trainDataBegin[predTrans == 1 & predTerm == 1, predClose := rbinom(nrow(trainDataBegin[predTrans == 1 & predTerm == 1,]), 1, trainDataBegin[predTrans == 1 & predTerm == 1,]$probClose)]
#       if(nrow(trainDataBegin[predClose == 1, ]) > 0){# if there was a closure with payment 
#         trainDataBegin[predTrans == 1 & predTerm == 1 & predClose == 1, predProbGam := rbinom(nrow(trainDataBegin[predTrans == 1 & predTerm == 1 & predClose == 1,]), 1, payIBNR$prob[1])]
#         if(nrow(trainDataBegin[predTrans == 1 & predTerm == 1 & predClose == 1 & predProbGam == 1, ]) > 0){
#           trainDataBegin[predTrans == 1 & predTerm == 1 & predClose == 1 & predProbGam == 1, payment := sapply(1:nrow(trainDataBegin[predTrans == 1 & predTerm == 1 & predClose == 1 & predProbGam == 1,]), function(xx) tabPayIBNRLogNo1[timeInStatePrev == trainDataBegin[predTrans == 1 & predTerm == 1 & predClose == 1 & predProbGam == 1,]$timeInState[xx], .(payIBNR)][[1]])]
#         }
#         if(nrow(trainDataBegin[predTrans == 1 & predTerm == 1 & predClose == 1 & predProbGam == 0, ]) > 0){
#           trainDataBegin[predTrans == 1 & predTerm == 1 & predClose == 1 & predProbGam == 0, payment := sapply(1:nrow(trainDataBegin[predTrans == 1 & predTerm == 1 & predClose == 1 & predProbGam == 0,]), function(xx) tabPayIBNRLogNo2[timeInStatePrev == trainDataBegin[predTrans == 1 & predTerm == 1 & predClose == 1 & predProbGam == 0,]$timeInState[xx], .(payIBNR)][[1]])]
#         }
#       }
#     }
#     
#   }
#   dataIBNR <- rbindlist(list(dataIBNR, trainDataBegin[predTrans == 1, ]), fill = TRUE, use.names=TRUE)
#   if(nrow(trainDataBegin[predTrans == 1, ]) > 0){
#     trainDataBegin <- trainDataBegin[predTrans != 1, ]
#   }
#   
#   trainDataBegin[, timeInState := timeInState + 1]
# }
# 
# dataIBNR[, numbPayment := 0]
# dataIBNR[payment != 0, numbPayment := 1]
# dataIBNR[, paidCumPeriod := cumsum(payment), by = c('numClaims')]
# dataIBNR[, timeInState := 1]
# dataIBNRTerm <- dataIBNR[predTerm == 1, ]
# dataIBNR <- dataIBNR[predTerm != 1, ]
# 
# dataIBNR <- dataIBNR[, c(1:7,9, 13)]
censoredClaim2 <- censoredClaim[, c(1:6, 8,11, 15)]

totalCensoredClaim <- copy(censoredClaim2)




# Projection cashflows

cashFlowMatrixAggreg <- numeric(50)
cashFlowMatrixAggregIndiv <- matrix(NA, nrow = nrow(totalCensoredClaim), ncol = 50)
iIndex <- 1
for(iIndex in 1:50){# 50MC repetitions
  cashFlowMatrix <- matrix(NA, nrow = nrow(totalCensoredClaim), ncol = 100)# each MC repetition allows for max 100 payments
  newdata <- copy(totalCensoredClaim[, .SD, .SDcol = c('timeInState', 'numbPayment', 'paidCumPeriod', 'payment')])
  newdata[, index := 1:nrow(newdata)]
  newdata[, lastCumPay := paidCumPeriod]
  j <- 1
  
  while(nrow(newdata) != 0){
    newdata[numbPayment >= 4, numbPayment := 4]
    newdata[timeInState >= 3, timeInState := 3]
    newdata[, lowPay := 1]
    newdata[payment > 5000, lowPay := 0]
    newdata[, prob := sapply(1:nrow(newdata), function(xx) tabTransProbs[timeInStatePrev == newdata$timeInState[xx] & paymentStatePrev == newdata$numbPayment[xx], .(transProba)][[1]])]# probability of transition out of state 
    newdata[, pred := rbinom(nrow(newdata), 1, newdata$prob)]# indicator of transition out of the state
    
    if(nrow(newdata[pred == 0, ])){
      if(nrow(newdata[pred == 0 & numbPayment == 0, ]) > 0){
        newdata[pred == 0 & numbPayment == 0, firstPay := 0]
      }
      if(nrow(newdata[pred == 0 & numbPayment != 0, ]) > 0){
        newdata[pred == 0 & numbPayment != 0, firstPay := 1]
      }
    }
    
    if(nrow(newdata[pred == 1, ]) > 0){
      newdata[pred == 1, probTerm := sapply(1:nrow(newdata[pred == 1,]), function(xx) tabTerm[timeInStatePrev == newdata[pred == 1,]$timeInState[xx] & paymentStatePrev == newdata[pred == 1,]$numbPayment[xx], .(transProbaTerm)][[1]])]
      newdata[pred == 1, predTerm := rbinom(nrow(newdata[pred == 1,]), 1, newdata[pred == 1,]$probTerm)]
      if(nrow(newdata[predTerm == 0 & numbPayment == 0, ]) > 0){
        newdata[predTerm == 0 & numbPayment == 0, predProbPareto := rbinom(nrow(newdata[predTerm == 0 & numbPayment == 0,]), 1, 1-1)]#Jordy: replaced "paret$p.less.thresh" by 1 to change the probability to be in the pareto part to 0 as I only want to consider the mixture of lognormals 
        if(nrow(newdata[predTerm == 0 & numbPayment == 0 & predProbPareto == 0, ]) > 0){
          newdata[predTerm == 0 & numbPayment == 0 & predProbPareto == 0, predProbGam := rbinom(nrow(newdata[predTerm == 0 & numbPayment == 0 & predProbPareto == 0,]), 1, firstPayMod3$prob[1])]
          if(nrow(newdata[predTerm == 0 & numbPayment == 0 & predProbPareto == 0 & predProbGam == 1, ]) > 0){
            newdata[predTerm == 0 & numbPayment == 0 & predProbPareto == 0 & predProbGam == 1, firstPay := sapply(1:nrow(newdata[predTerm == 0 & numbPayment == 0 & predProbPareto == 0 & predProbGam == 1,]), function(xx) tabFirstPayLogNo1[timeInStatePrev == newdata[predTerm == 0 & numbPayment == 0 & predProbPareto == 0 & predProbGam == 1,]$timeInState[xx], .(firstPay)][[1]])]
          }
          if(nrow(newdata[predTerm == 0 & numbPayment == 0 & predProbPareto == 0 & predProbGam == 0, ]) > 0){
            newdata[predTerm == 0 & numbPayment == 0 & predProbPareto == 0 & predProbGam == 0, firstPay := sapply(1:nrow(newdata[predTerm == 0 & numbPayment == 0 & predProbPareto == 0 & predProbGam == 0,]), function(xx) tabFirstPayLogNo2[timeInStatePrev == newdata[predTerm == 0 & numbPayment == 0 & predProbPareto == 0 & predProbGam == 0,]$timeInState[xx], .(firstPay)][[1]])]
          }
        }
        if(nrow(newdata[predTerm == 0 & numbPayment == 0 & predProbPareto == 1, ]) > 0){
          newdata[predTerm == 0 & numbPayment == 0 & predProbPareto == 1, firstPay := sapply(1:nrow(newdata[predTerm == 0 & numbPayment == 0 & predProbPareto == 1,]), function(xx) mean(evir::rgpd(1000000, paret$par.ests[1], mu = 6700, beta = paret$par.ests[2])))]
        }
      }
      for(i in 1:length(modelLRList)){
        if(nrow(newdata[predTerm == 0 & numbPayment == i, ]) > 0){
          newdata[predTerm == 0 & numbPayment == i, predProbGam := rbinom(nrow(newdata[predTerm == 0 & numbPayment == i,]), 1, modelLRList[[i]]$prob[1])]
          if(nrow(newdata[predTerm == 0 & numbPayment == i & predProbGam == 1, ]) > 0){
            newdata[predTerm == 0 & numbPayment == i & predProbGam == 1, firstPay := sapply(1:nrow(newdata[predTerm == 0 & numbPayment == i & predProbGam == 1,]), function(xx) tabLRLogNo[[i]][timeInStatePrev == newdata[predTerm == 0 & numbPayment == i & predProbGam == 1,]$timeInState[xx] & paymentPrev == newdata[predTerm == 0 & numbPayment == i & predProbGam == 1,]$lowPay[xx], .(LR)][[1]])]
          }
          if(nrow(newdata[predTerm == 0 & numbPayment == i & predProbGam == 0, ]) > 0){
            newdata[predTerm == 0 & numbPayment == i & predProbGam == 0, firstPay := sapply(1:nrow(newdata[predTerm == 0 & numbPayment == i & predProbGam == 0,]), function(xx) tabLRPareto[[i]][timeInStatePrev == newdata[predTerm == 0 & numbPayment == i & predProbGam == 0,]$timeInState[xx] & paymentPrev == newdata[predTerm == 0 & numbPayment == i & predProbGam == 0,]$lowPay[xx], .(LR)][[1]])]
          }
        }
      }
      if(nrow(newdata[predTerm == 1, ]) > 0){
        newdata[predTerm == 1, probPayTerm := sapply(1:nrow(newdata[predTerm == 1,]), function(xx) tabProbNoPay[timeInStatePrev == newdata[predTerm == 1,]$timeInState[xx] & paymentStatePrev == newdata[predTerm == 1,]$numbPayment[xx], .(ProbaNoPayTerm)][[1]])]
        
        newdata[predTerm == 1, predPayTerm := rbinom(nrow(newdata[predTerm == 1,]), 1, unlist(newdata[predTerm == 1,]$probPayTerm))]
        if(nrow(newdata[predTerm == 1 & predPayTerm == 0 & numbPayment == 0, ]) > 0){
          newdata[predTerm == 1 & predPayTerm == 0 & numbPayment == 0, firstPay := 0]
        }
        if(nrow(newdata[predTerm == 1 & predPayTerm == 0 & numbPayment != 0, ]) > 0){
          newdata[predTerm == 1 & predPayTerm == 0 & numbPayment != 0, firstPay := 1]
        }
        if(nrow(newdata[predTerm == 1 & predPayTerm == 1 & numbPayment == 0, ]) > 0){
          newdata[predTerm == 1 & predPayTerm == 1 & numbPayment == 0, predProbPareto := rbinom(nrow(newdata[predTerm == 1 & predPayTerm == 1 & numbPayment == 0,]), 1, 1-1)]# indicator of if we should use the pareto component 
          if(nrow(newdata[predTerm == 1 & predPayTerm == 1 & numbPayment == 0 & predProbPareto == 0, ]) > 0){ # if we do not use the pareto component 
            newdata[predTerm == 1 & predPayTerm == 1 & numbPayment == 0 & predProbPareto == 0, predProbGam := rbinom(nrow(newdata[predTerm == 1 & predPayTerm == 1 & numbPayment == 0 & predProbPareto == 0,]), 1, firstPayMod3$prob[1])]# indicator of if we should use the first component of the fitted mxture model 
            if(nrow(newdata[predTerm == 1 & predPayTerm == 1 & numbPayment == 0 & predProbPareto == 0 & predProbGam == 1, ]) > 0){
              newdata[predTerm == 1 & predPayTerm == 1 & numbPayment == 0 & predProbPareto == 0 & predProbGam == 1, firstPay := sapply(1:nrow(newdata[predTerm == 1 & predPayTerm == 1 & numbPayment == 0 & predProbPareto == 0 & predProbGam == 1,]), function(xx) tabFirstPayLogNo1[timeInStatePrev == newdata[predTerm == 1 & predPayTerm == 1 & numbPayment == 0 & predProbPareto == 0 & predProbGam == 1,]$timeInState[xx], .(firstPay)][[1]])] 
            }
            if(nrow(newdata[predTerm == 1 & predPayTerm == 1 & numbPayment == 0 & predProbPareto == 0 & predProbGam == 0, ]) > 0){# if we should use the pareto component 
              newdata[predTerm == 1 & predPayTerm == 1 & numbPayment == 0 & predProbPareto == 0 & predProbGam == 0, firstPay := sapply(1:nrow(newdata[predTerm == 1 & predPayTerm == 1 & numbPayment == 0 & predProbPareto == 0 & predProbGam == 0,]), function(xx) tabFirstPayLogNo2[timeInStatePrev == newdata[predTerm == 1 & predPayTerm == 1 & numbPayment == 0 & predProbPareto == 0 & predProbGam == 0,]$timeInState[xx], .(firstPay)][[1]])]
            }
          }
          if(nrow(newdata[predTerm == 1 & predPayTerm == 1 & numbPayment == 0 & predProbPareto == 1, ]) > 0){
            newdata[predTerm == 1 & predPayTerm == 1 & numbPayment == 0 & predProbPareto == 1, firstPay := sapply(1:nrow(newdata[predTerm == 1 & predPayTerm == 1 & numbPayment == 0 & predProbPareto == 1,]), function(xx) mean(evir::rgpd(1000000, paret$par.ests[1], mu = 6700, beta = paret$par.ests[2])))]
          }
        }
      }
      
      for(i in 1:length(modelLRList)){
        if(nrow(newdata[predTerm == 1, ]) > 0){
          if(nrow(newdata[predTerm == 1 & numbPayment == i & predPayTerm == 1, ]) > 0){
            newdata[predTerm == 1 & numbPayment == i & predPayTerm == 1, predProbGam := rbinom(nrow(newdata[predTerm == 1 & numbPayment == i & predPayTerm == 1,]), 1, modelLRList[[i]]$prob[1])]
            if(nrow(newdata[predTerm == 1 & numbPayment == i & predPayTerm == 1 & predProbGam == 1, ]) > 0){
              newdata[predTerm == 1 & numbPayment == i & predPayTerm == 1 & predProbGam == 1, firstPay := sapply(1:nrow(newdata[predTerm == 1 & numbPayment == i & predPayTerm == 1 & predProbGam == 1,]), function(xx) tabLRLogNo[[i]][timeInStatePrev == newdata[predTerm == 1 & numbPayment == i & predPayTerm == 1 & predProbGam == 1,]$timeInState[xx] & paymentPrev == newdata[predTerm == 1 & numbPayment == i & predPayTerm == 1 & predProbGam == 1,]$lowPay[xx], .(LR)][[1]])]
            }
            if(nrow(newdata[predTerm == 1 & numbPayment == i & predPayTerm == 1 & predProbGam == 0, ]) > 0){
              newdata[predTerm == 1 & numbPayment == i & predPayTerm == 1 & predProbGam == 0, firstPay := sapply(1:nrow(newdata[predTerm == 1 & numbPayment == i & predPayTerm == 1 & predProbGam == 0,]), function(xx) tabLRPareto[[i]][timeInStatePrev == newdata[predTerm == 1 & numbPayment == i & predPayTerm == 1 & predProbGam == 0,]$timeInState[xx] & paymentPrev == newdata[predTerm == 1 & numbPayment == i & predPayTerm == 1 & predProbGam == 0,]$lowPay[xx], .(LR)][[1]])]
            }
          }
        }
      } 
    }
    newdata[, predPay := firstPay]
    newdata[numbPayment != 0, predPay := lastCumPay*firstPay]
    cashFlowMatrix[newdata[numbPayment != 0, ]$index, j] <- newdata[numbPayment != 0, ]$predPay - newdata[numbPayment != 0, ]$lastCumPay
    cashFlowMatrix[newdata[numbPayment == 0, ]$index, j] <- newdata[numbPayment == 0, ]$predPay
    newdata[, payment := cashFlowMatrix[newdata$index, j]]
    if(nrow(newdata[pred == 1, ]) > 0){
      newdata <- newdata[predTerm != 1, ]
    }
    newdata[, lastCumPay := predPay]
    newdata[, numbPayment := numbPayment + 1]
    newdata[, timeInState := timeInState + 1]
    newdata <- copy(newdata[, .SD, .SDcol = c('timeInState', 'numbPayment', 'lastCumPay', 'index', 'payment')])
    
    j <- j + 1
    
  }
  print(paste0('index is ', iIndex))
  cashFlowMatrixAggreg[iIndex] <- sum(cashFlowMatrix, na.rm = TRUE)
  cashFlowMatrixAggregIndiv[,iIndex] <- rowSums(cashFlowMatrix, na.rm = TRUE)
  iIndex <- iIndex + 1
}

true_reserves_bettonville <- microDataBISub[year(bookDate)>2005,] %>% group_by(polNumb)%>% summarise(Total = sum(delt0Pay))
reserves_bettonville <- rowMeans(cashFlowMatrixAggregIndiv)

