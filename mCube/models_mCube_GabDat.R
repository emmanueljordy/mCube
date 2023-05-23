rm(list=ls()) 
library(MASS)
library(stringr)
#library(microView)
#library(microPrep)
#library(microModel)
library(data.table)
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
library(tpSuite)
library(R2DT)
library(stringr)
library(devFunc)
library(QuantifQuantile)
library(dplyr)
library(ChainLadder)
library(flexmix)
library(lubridate)
library(evmix)
library(truncnorm)

output <- as.data.table(read.table("Gabrielli_dat_AZ.txt", sep=";", header = T))
output_micro <- copy(output[LoB == 2,])
rm(output)

Date_cols <- which(grepl( "Date" , names( output_micro)))
date_cols <- c(Date_cols,which(grepl( "date" , names( output_micro))))


output_micro <- output_micro %>%
  mutate_at(vars(date_cols), funs(as.Date(., "%Y-%m-%d")))

output_micro[,transType := as.factor(transType)]


sum(output_micro[year(bookDate)>2005  & year(repDate)<=2005 & payInd ==1,delt0Pay],na.rm=T)
sum(output_micro[year(accDate)==1994 & year(bookDate) == 1995 & payInd == 1,delt0Pay],na.rm=T)
sum(output_micro[year(bookDate)>2005,delt0Pay],na.rm=T)





####################################################
###         define test set                      ###
####################################################

refYearTest <- 2005
output_micro_test_check <- copy(output_micro[year(bookDate)>refYearTest & year(accDate)>1994,])# we assume that the claims for 1994 ae not in the test set. In order to have a first accident year that is "complete" in the Chain-ladder square
sum(output_micro_test_check[payInd == 1,]$delt0Pay,na.rm = T)# will be the reserve (RBNS + IBNR) to predict check that it is the same as "true Res." in the object reserves
output_micro_test <- copy(output_micro[polNumb %in% output_micro_test_check$polNumb & year(repDate) < (refYearTest+1) ,])
length(unique(output_micro_test$polNumb))
output_micro_test_check[payInd == 0| is.na(payInd), delt0Pay := 0]# change delt0Pay when there is no payment. Makes it easier in order to compute the reserves and does not impact the modelling
obs_res_test_RBNS <- copy(output_micro_test_check[year(repDate) < (refYearTest+1) , lapply(.SD,sum), by=polNumb,.SDcol = c('delt0Pay')])# Reserve for all RBNS claims
length(unique(obs_res_test_RBNS$polNumb))#5624
sum(obs_res_test_RBNS$delt0Pay)
obs_res_test_IBNR <-  copy(output_micro_test_check[year(repDate) >refYearTest & payInd ==1 , lapply(.SD,sum), by=polNumb,.SDcol = c('delt0Pay')])
sum(obs_res_test_IBNR$delt0Pay)
length(unique(obs_res_test_IBNR$polNumb))#274

# define calibration set 
set.seed(9*2020)
output_micro_train <-  copy(output_micro[!(year(bookDate)>refYearTest),])
output_micro_train[,transInd := transType%in% c('P', 'TP', 'TN')]
output_micro_train[,maxNumTrans := sum(transInd), by = 'polNumb' ]

cumsum(table(output_micro_train[ind==1, maxNumTrans])/nrow(output_micro_train[ind==1, ]))


#######################################################
### Prepare data for time/payment process modelling ###
#######################################################

source("microPrep_scand_simMachine.R")
source("microModel_scand_simMachine.R")

maxLag <- 1
perLen <- 365
nTrans <- 3

transDataSimMachineTrain <- transPrep(output_micro_train, nTrans,whichTrans='P',verbose=TRUE)
transDataSimMachineTest <- transPrep(output_micro_test, nTrans,whichTrans='P',verbose=TRUE)


# small check to see if all payments are still correctly defined 
# totcostTrain <- c()
# totcostTest <- c()
# for (i in 1:nTrans){
#   totcostTrain[i] <- sum(transDataSimMachineTrain[[i]][transStat == 1,  outComeT])
#   totcostTest[i] <- sum(transDataSimMachineTest[[i]][transStat == 1  &year(repDate)<2006& year(bookDate)>2005,  outComeT])
# }
# 
# sum(totcostTrain)
# sum(totcostTest)





#### Run varprep  on the train data 

nMinMod <- 500; nMinTimeLev <- 30; nMinLev <- 30; nMaxNoMod <- 250; nMaxLevInState <- 3; nMaxLevInStateLast <- 3 ;nMaxLevdeltRep <- 4; nMaxLevInProc <- 12; nMaxLevInProcLast <- 12; nGroupsTr = 10; nGroupsFin <- 15; modVars <- c('fastRep'); modVarsType <- c('fac')
transDataPrepSimMachineTrain <- varPrep(transDataSimMachineTrain, nMinMod=nMinMod, nMaxNoMod=nMaxNoMod,nMaxLevdeltRep = nMaxLevdeltRep, nMaxLevInState=nMaxLevInState, nMaxLevInStateLast=nMaxLevInStateLast, nMaxLevInProc=nMaxLevInProc, nMaxLevInProcLast=nMaxLevInProcLast, nMinTimeLev=nMinTimeLev, nMinLev=nMinLev, modVars=modVars, modVarsType=modVarsType, nGroupsFin=nGroupsFin, nGroupsTr=nGroupsTr,noIncur = TRUE, binning = F)
rm(transDataSimMachineTrain)





## make sure that in the test set, the time vars are updated to reflect the 1st January after the reference year

trans_TestBis = list()
for(i in  1:nTrans){
  trans_TestBis[[i]] = data.table(transDataSimMachineTest[[i]])
}

#verwijder volgende
for(i in 1:2){
  trans_TestBis[[i]] = trans_TestBis[[i]][year(bookDate)>2005,]
  polNumbs = unique(trans_TestBis[[i]]$polNumb)
  for(k in (i+1):nTrans){
    trans_TestBis[[k]] =  trans_TestBis[[k]][!(polNumb %in% polNumbs),]
  }
}
#verwijder te vroege observaties in de nTranse dataset
trans_TestBis[[nTrans]] =  trans_TestBis[[nTrans]][year(bookDate)>2005,]


for(i in 1:nTrans){
  if(i==nTrans){
    #keep first bookdate after 2005
    listPol = unique(trans_TestBis[[i]]$polNumb)
    for(k in 1:length(listPol)){
      indInState1 = which(trans_TestBis[[i]]$polNumb==listPol[k]&trans_TestBis[[i]]$inStateTime==1)
      if(length(indInState1)>1){
        totRow = which(trans_TestBis[[i]]$polNumb==listPol[k])
        finRow = totRow[length(totRow)]
        trans_TestBis[[i]] = trans_TestBis[[i]][-c(indInState1[2]:finRow),]
      }
    }
  }
  trans_TestBis[[i]] = trans_TestBis[[i]][transStat==1,] #laatste lijn
  
  bookDate = trans_TestBis[[i]]$bookDate
  repDay = rep(NA, length(bookDate)) 
  for(l in 1:length(bookDate)){## each bookDate refers to the bookdate of a specific claim 
    repDate = output_micro_test[polNumb == trans_TestBis[[i]][l,]$polNumb & ind == 1,]$repDate# get the repDate of the claim
    repDay[l] = format(as.Date(repDate,format="%Y-%m-%d"), format = "%d")
  }
  bookDay = format(as.Date(bookDate,format="%Y-%m-%d"), format = "%d")
  refDate = as.Date(paste("01-", repDay,"-06", sep=""), format = "%m-%d-%y")
  # for(l in 1:length(bookDay)){
  #   if(as.numeric(repDay[l]) < as.numeric(bookDay[l])){# the repDay determines the day of the next months to constitute 1 month e.g. from 16 January to 16 february is 1 month. In this case, if the bookday is greater, then we are in the period of the next months, hence we need to add 1 month to the bookdate
  #     #next month
  #     bookDate[l] = bookDate[l] 
  #     #%m+% months(12)
  #   }
  # }
  newBookDate <- refDate
  day(bookDate) <- as.numeric(repDay)
  dif = pmax(as.numeric(ceiling((trans_TestBis[[i]]$bookDate - refDate)/perLen)),0)
  
  trans_TestBis[[i]][, bookDate := newBookDate]
  trans_TestBis[[i]][, finYear := year(bookDate)]
  trans_TestBis[[i]][, inStateTimeTrans := pmax(as.numeric(as.character(inStateTime)) - dif,1)]
  trans_TestBis[[i]][, inStateTime := inStateTimeTrans]
  maxInStateTime = max(as.numeric(as.character(transDataPrepSimMachineTrain[["data"]][[i]]$inStateTimeTrans)))
  minInStateTime = min(as.numeric(as.character(transDataPrepSimMachineTrain[["data"]][[i]]$inStateTimeTrans)))
  trans_TestBis[[i]][((inStateTimeTrans))>maxInStateTime, inStateTimeTrans := maxInStateTime]
  trans_TestBis[[i]][((inStateTimeTrans))<minInStateTime, inStateTimeTrans := minInStateTime]
  trans_TestBis[[i]][,maxInStateTime:= maxInStateTime]
  
  # trans_TestBis[[i]][, inStateTimeTransGrPay := inStateTimeTrans]
  # maxInStateTimeGrPay = max(as.numeric(as.character(transDataPrepSimMachineTrain[["data"]][[i]]$inStateTimeTransGrPay)))
  # minInStateTimeGrPay = max(1, min(as.numeric(as.character(transDataPrepSimMachineTrain[["data"]][[i]]$inStateTimeTransGrPay))))
  # trans_TestBis[[i]][as.numeric(as.character(inStateTimeTransGrPay))>maxInStateTimeGrPay, inStateTimeTransGrPay := maxInStateTimeGrPay]
  # trans_TestBis[[i]][as.numeric(as.character(inStateTimeTransGrPay))<minInStateTimeGrPay, inStateTimeTransGrPay := minInStateTimeGrPay]
  
  trans_TestBis[[i]][, inProcTimeTrans := pmax(as.numeric(as.character(inProcTime)) - dif,1)]
  trans_TestBis[[i]][, inProcTime := inProcTimeTrans]
  maxInProcTime = max(as.numeric(as.character(transDataPrepSimMachineTrain[["data"]][[i]]$inProcTimeTrans)))
  minInProcTime = min(as.numeric(as.character(transDataPrepSimMachineTrain[["data"]][[i]]$inProcTimeTrans)))
  trans_TestBis[[i]][((inProcTimeTrans))>maxInProcTime, inProcTimeTrans := maxInProcTime]
  trans_TestBis[[i]][((inProcTimeTrans))<minInProcTime, inProcTimeTrans := minInProcTime]
  trans_TestBis[[i]][,maxInProcTime:= maxInProcTime]
  
  trans_TestBis[[i]][, deltRepTrans := deltRep]
  maxdeltRep = max(as.numeric(as.character(transDataPrepSimMachineTrain[["data"]][[i]]$deltRepTrans)))
  mindeltRep = min(as.numeric(as.character(transDataPrepSimMachineTrain[["data"]][[i]]$deltRepTrans)))
  trans_TestBis[[i]][as.numeric(as.character(deltRepTrans))>maxdeltRep, deltRepTrans := maxdeltRep]
  trans_TestBis[[i]][as.numeric(as.character(deltRepTrans))<mindeltRep, deltRepTrans := mindeltRep]
  trans_TestBis[[i]][,maxdeltRep:= maxdeltRep]
  
  
  if(i>1){
    maxdelt1PayTime = max(as.numeric(as.character(transDataPrepSimMachineTrain[["data"]][[i]]$delt1PayTime)))
    mindelt1PayTime = min(as.numeric(as.character(transDataPrepSimMachineTrain[["data"]][[i]]$delt1PayTime)))
    trans_TestBis[[i]][, delt1PayTimeTrans := pmax(as.numeric(as.character(delt1PayTime)),1)]
    trans_TestBis[[i]][, delt1PayTime := delt1PayTimeTrans]
    trans_TestBis[[i]][((delt1PayTimeTrans))>maxdeltRep, delt1PayTimeTrans := maxdeltRep]
    trans_TestBis[[i]][((delt1PayTimeTrans))<mindeltRep, delt1PayTimeTrans := mindeltRep]
    trans_TestBis[[i]][,maxdelt1PayTime:= maxdelt1PayTime]
    asFactorDT(trans_TestBis[[i]], 'delt1PayTimeTrans')
  }
  
  #as factor
  # trans_TestBis[[i]][,fastRep := as.factor(fastRep)]
  # trans_TestBis[[i]][,deltRepTrans := as.factor(deltRepTrans)]
  # trans_TestBis[[i]][,inStateTimeTrans := as.factor(inStateTimeTrans)]
  # #trans_TestBis[[i]][,inStateTimeTransGrPay := as.factor(inStateTimeTransGrPay)]
  # trans_TestBis[[i]][,inProcTimeTrans := as.factor(inProcTimeTrans)]
  # trans_TestBis[[i]][,inStateTime := as.factor(inStateTime)]
  # trans_TestBis[[i]][,inProcTime := as.factor(inProcTime)]
  
  asFactorDT(trans_TestBis[[i]], 'deltRepTrans')
  asFactorDT(trans_TestBis[[i]], 'inStateTimeTrans')
  asFactorDT(trans_TestBis[[i]], 'inProcTimeTrans')
  
  
  
  var2Rem <- names(trans_TestBis[[i]])[grep('ncur', names(trans_TestBis[[i]]))]
  if(length(var2Rem)){
    trans_TestBis[[i]][, (var2Rem) := NULL][]
  }
  trans_TestBis[[i]] <- trans_TestBis[[i]][transType != 'I', ]
  trans_TestBis[[i]][,transMN := 'N']
  trans_TestBis[[i]][,transType := 'P']
}

# small check to see if all payments are still correctly defined 
# totcostTrain <- c()
# totcostTest <- c()
# 
# for (i in 1:nTrans){
#   totcostTrain[i] <- sum(transDataPrepSimMachineTrain[["data"]][[i]][transType %in% c('P', 'TP'),  outComeT])
#   totcostTest[i] <- sum(trans_TestBis[[i]][transType %in% c('P', 'TP')  &repYear<2006& year(bookDate)>2005,  outComeT])
# }
# 
# sum(totcostTrain)
# sum(totcostTest)



for(i in 1:nTrans){
  if(!is.null(transDataPrepSimMachineTrain[["split"]][[i]])){
    splits = transDataPrepSimMachineTrain[["split"]][[i]]
    names(splits) = c(  'delt0Pay','delt1Pay','cumDelt1Pay')
    transform2BinnedVar(trans_TestBis[[i]], splits, TRUE)
  }
}

set.seed(9*2020)
trainDT <- copy(transDataPrepSimMachineTrain[["data"]])
testDT <- copy(trans_TestBis)
# small check to see if all payments are still correctly defined 
# totcostTrain <- c()
# totcostTest <- c()
# for (i in 1:nTrans){
#   totcostTrain[i] <- sum(trainDT[[i]][transType %in% c('P', 'TP'),  outComeT])
#   totcostTest[i] <- sum(testDT[[i]][transType %in% c('P', 'TP')  &year(repDate)<2006& year(bookDate)>2005,  outComeT])
# }
# 
# sum(totcostTrain)
# sum(totcostTest)


########################################################################
### test set for simPaths   ###
########################################################################

testDT2 <- copy(testDT)
testDT2<-llply(testDT2, function(xx) xx[, transType := 'P'])
testDT2<-llply(testDT2, function(xx) xx[, transMN := 'N'])
########################################################################
### Adjust training set to prepare for modelling                     ###
########################################################################
trainDT <-llply(trainDT, function(xx) xx[is.na(transType),transType := 'P'])
trainDT <-llply(trainDT, function(xx) xx[is.na(transMN),transMN := 'P'])


for (i in (1:6)){
  varRem <- c("repYear", "linkRatio","delt0Pay","delt0PayGrouped")
  namesD <- colnames(trainDT[[i]])
  ncurNames <- namesD[grep('ncur', namesD)]
  varRem <- c(varRem, ncurNames)
  trainDT[[i]][,(varRem) := NULL]
}



########################################################################
### Fit Time model                                                   ###
########################################################################
nMinMod <- 500; nMinLev <- 30; nTimesParams <- 5; nMaxNoMod <- 250; notInModVars <- NULL; intVars <- NULL; nSamp <- 200000; nIterMax <- 1000
form1Time <- as.formula(transMN ~ inStateTimeTrans + fastRep + deltRepTrans) 
form2Time <- as.formula(transMN ~ inStateTimeTrans + inProcTimeTrans + fastRep + deltRepTrans+ delt1Pay)
form3Time <- as.formula(transMN ~ inStateTimeTrans + inProcTimeTrans + fastRep + deltRepTrans+ cumDelt1Pay +delt1Pay )
finTimeMods <- timeFit2(copy(trainDT), nMinMod=nMinMod, nMinLev=nMinLev, nTimesParams=nTimesParams, nMaxNoMod=nMaxNoMod, notInModVars=notInModVars, intVars=intVars, nSamp=nSamp, nIterMax=nIterMax,verbose = T,verboseConv = T)

########################################################################
### Fit Pay model                                                   ###
########################################################################
#payment fit + check
fitData <- copy(trainDT)
form1Pay <- as.formula(outComeTrans ~ fastRep + inProcTimeTrans+deltRepTrans+transType) 
form2Pay <- as.formula(outComeTrans ~ fastRep + deltRepTrans + inProcTimeTrans + delt1Pay +transType)
form3Pay <- as.formula(outComeTrans ~  fastRep + deltRepTrans+inProcTimeTrans  + cumDelt1Pay +delt1Pay +transType)
finPayMods <- fitSplicedPareto2(fitData,form1Pay,form2Pay,form3Pay, c(-1e8, 0 ,1e8))


########################################################################
### Simulate future trajectories                                     ###
########################################################################
memory.limit(size=30000)
fixedTimeMax <- rep(2,6); perLen <- 365; nSims <- 50; lastTrans <- 10; verbose <- TRUE #lasttrance 7/10

set.seed(9*2020)
simsTestDT <- simPathsJolien2(copy(finTimeMods), copy(finPayMods), copy(trainDT), timeSplits = NULL, copy(testDT2), fixedTimeMax, perLen, nSims, lastTrans, "splicedGPD",T,11)






########################################################################
### Extract Observed reserves                                        ###
########################################################################
#extracting BE and observed reserve
predSimCalibDT <- simsCalibDT$predSim
nSimCalibDT <- simsCalibDT$nSim

predSimTestDT <- simsTestDT$predSim
nSimTestDT <- simsTestDT$nSim

predSimIBNR <- simsIBNR$predSim
nSimIBNR <- simsIBNR$nSim


transDataCalibRes <- transPrep(output_micro_calib, nTrans,whichTrans='P',verbose=TRUE)
transDataTestRes <- transPrep(output_micro_test, nTrans,whichTrans='P',verbose=TRUE)






currentYear <- 2016
inflRates <- c(0,0,0)



obsResCheckCalibDT <- extractObsResJordy(predSimCalibDT, transDataCalibRes, inflRates, raw2 = FALSE, totalCost = FALSE,testSimsDT = calibDT2)
obsResVecCalibDT <- unlist(obsResCheckCalibDT$obsRes)
length(obsResVecCalibDT)
sum(obsResVecCalibDT)

polNumbs94 <- output_micro[year(accDate)==1994,polNumb]
transDataTestRes2 <- copy(llply(transDataTestRes, function(xx) xx[ year(bookDate)>2005,]))
obsResCheckTestDT <- extractObsResJordy(predSimTestDT, transDataTestRes2, inflRates, raw2 = F, totalCost = FALSE,testSimsDT = testDT2)
obsResVecTestDT <- unlist(obsResCheckTestDT$obsRes)
length(obsResVecTestDT)
sum(obsResVecTestDT)

obsResTestDT <- obs_res_test_RBNS
nrow(obsResTestDT )
sum(obsResTestDT$delt0Pay)




ObsresTestDF <- ldply(copy(obsResCheckTestDT[["obsRes"]]), data.table)


ObsresTestDF <- ObsresTestDF[order(ObsresTestDF $.id),]
ObsresTestDF[is.na(ObsresTestDF)] <- 0

View(ObsresTestDF[which(ObsresTestDF$outComeT != obsResTestDT$delt0Pay),])


########################################################################
### Extract Best estimates                                           ###
########################################################################

BECalibDT <- inflCorrBE(predSimCalibDT, inflRates, currentYear)
sum(unlist(lapply(BECalibDT,median)))/length((unlist(lapply(BECalibDT,median))))
sum(unlist(lapply(BECalibDT,mean)))/length((unlist(lapply(BECalibDT,mean))))

BETestDT <- inflCorrBE(predSimTestDT, inflRates, currentYear)
sum(unlist(lapply(BETestDT,median)))/length((unlist(lapply(BETestDT,median))))
sum(unlist(lapply(BETestDT,mean)))/length((unlist(lapply(BETestDT,mean))))

BEIBNRDT <- inflCorrBE(predSimIBNR, inflRates, currentYear)

sum(unlist(lapply(BETestDT,mean))) + sum(unlist(lapply(BEIBNRDT,mean)))
########################################################################
###                     RBNS reserves                                ###
########################################################################
resTestTry <- QuantifQuantileMean(unlist(lapply(BECalibDT,mean)), obsResVecCalibDT, x = unlist(lapply(BETestDT,mean)), alpha = seq(.01, .99,.01), testN = 200)
sampsQRTry <- resTestTry$fitted.values
testMeanPredTry <- sampsQRTry[1, ]
totResTry <- sum(testMeanPredTry)
totResTry

BETestDT = BETestDT[order(names(BETestDT))]
savePath = 'C:/Users/u0134144/Documents/Phd_Allianz/Micro_reserving/code/MicroPrepJordy/summariseOBSBEquantile1221Mn.RData'
resMix <- extractFinCalibTestDTNoMix(BECalibDT,obsResVecCalibDT,BETestDT,obsResTestDT$delt0Pay,savePath)


library(e1071)
round(c(summary(obsResTestDT$delt0Pay)[c(1,3,4,6)],IQR(obsResTestDT$delt0Pay),skewness(obsResTestDT$delt0Pay),kurtosis(obsResTestDT$delt0Pay)),2)
round(c(summary(unlist(lapply(BETestDT,mean)))[c(1,3,4,6)],IQR(unlist(lapply(BETestDT,mean))),skewness(unlist(lapply(BETestDT,mean))),kurtosis(unlist(lapply(BETestDT,mean)))),2)
round(c(summary(testMeanPredTry )[c(1,3,4,6)],IQR(testMeanPredTry ),skewness(testMeanPredTry ),kurtosis(testMeanPredTry )),2)


########################################################################
### IBNR reserves                                     ###
########################################################################
resTestTryIBNR <- QuantifQuantileMean(unlist(lapply(BECalibDT,mean)), obsResVecCalibDT, x = unlist(lapply(BEIBNRDT,mean)), alpha = seq(.01, .99,.01), testN = 200)
sampsQRTryIBNR <- resTestTryIBNR$fitted.values
testMeanPredTryIBNR <- sampsQRTryIBNR[1, ]
totResTryIBNR <- sum(testMeanPredTryIBNR)
totResTryIBNR

totResTryIBNR + totResTry
########################################################################
### IBNR + RBNS reserves bets estimate                               ###
########################################################################
calibJordyRBNS <- calibJordy(BECalibDT,obsResVecCalibDT,BETestDT,1000)
calibJordyIBNR <- calibJordy(BEIBNRDT,obsResVecCalibDT,BETestDT,1000)

hist(unlist(calibJordyRBNS[["totResBoot"]]) + unlist(calibJordyIBNR[["totResBoot"]]) , freq = T,breaks =50, main="Reserve BE",xlab="Amount",xlim=c(6e7,8.5e7),ylim=c(0,50))
abline(v = sum(microDataBISub_test_check[payInd == 1,]$delt0Pay,na.rm = T), col = 2, lwd = 6)
abline(v = sum(testMeanPredTry) + sum(testMeanPredTryIBNR), col = 4, lwd = 3,lty=2)
abline(v = reserves$`CL Res.`[8], col = 6, lwd = 3,lty=2)
legend("topleft", legend=c("Obs Res", "Mcube Res", "CL Res"),
       col=c(2,4,6), lty=c(1,2,2), cex=0.8)


