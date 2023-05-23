## todo: remove hardcoded parts from simPaths

# transNr <- 1
# maxPerc <- 0.99
# minFreq <- 500
# tabData <- table(transData[[transNr]]$inStateTime)
# maxPercInd <- min(which(cumsum(tabData)/nrow(transData[[transNr]]) > maxPerc))
# minFreqInd <- min(which(tabData < minFreq))
# maxVal <- min(c(maxPercInd, minFreqInd))
# transData[[transNr]][inStateTime > maxVal, inStateTime := maxVal]
# sum(transData[[transNr]]$inStateTime >= maxVal)
# length(table(transData[[transNr]]$inStateTime))
#
# transNr <- 2
# tabData <- table(transData[[transNr]]$inStateTime)
# maxPercInd <- min(which(cumsum(tabData)/nrow(transData[[transNr]]) > maxPerc))
# minFreqInd <- min(which(tabData < minFreq))
# maxVal <- min(c(maxPercInd, minFreqInd))
# transData[[transNr]][inStateTime > maxVal, inStateTime := maxVal]
# sum(transData[[transNr]]$inStateTime >= maxVal)
# length(table(transData[[transNr]]$inStateTime))
#
# transNr <- 3
# tabData <- table(transData[[transNr]]$inStateTime)
# maxPercInd <- min(which(cumsum(tabData)/nrow(transData[[transNr]]) > maxPerc))
# minFreqInd <- min(which(tabData < minFreq))
# maxVal <- min(c(maxPercInd, minFreqInd))
# transData[[transNr]][inStateTime > maxVal, inStateTime := maxVal]
# sum(transData[[transNr]]$inStateTime >= maxVal)
# length(table(transData[[transNr]]$inStateTime))
#
# transNr <- 4
# tabData <- table(transData[[transNr]]$inStateTime)
# maxPercInd <- min(which(cumsum(tabData)/nrow(transData[[transNr]]) > maxPerc))
# minFreqInd <- min(which(tabData < minFreq))
# maxVal <- min(c(maxPercInd, minFreqInd))
# transData[[transNr]][inStateTime > maxVal, inStateTime := maxVal]
# sum(transData[[transNr]]$inStateTime >= maxVal)
# length(table(transData[[transNr]]$inStateTime))


## modIBNR
#' TODO
#'
#' @param dataIBNR Dataset used in this function.
#' @param perLen Length of the timeframe expressed in number of days.
#' @param maxDiffYear Maximum number of years to consider before the latest/most recent year in \code{dataIBNR}.
#' @param verbose Boolean indicating whether output should be printed or not - default value is FALSE.
#'
#' @return  TODO
#
modIBNR <- function(dataIBNR, perLen, maxDiffYear, verbose = FALSE){
  # Check input parameters and certain variables in 'dataIBNR'.
  checkDT(dataIBNR, c('polNumb', 'accDate'))
  checkNumVec(list(perLen, maxDiffYear))
  checkLogicVec(list(verbose))
  checkLength(list(perLen, maxDiffYear, verbose), 1)
  checkRanges(list(perLen, maxDiffYear), list(c('>', 0), c('>', 0)))
  
  dataIBNR[, accYear := year(accDate)]
  dataIBNR[, repDelay := 0]
  
  # Define 'repDelay', the reporting delay in years.
  if(perLen == 30){
    dataIBNR[year(repDate) != accYear, repDelay := (year(repDate) - accYear - 1)*12 + month(repDate)]
    # dataIBNR[, repDelay := pmax(0, (year(repDate) - accYear - 1)*12 + month(repDate))]
  } else if(perLen == 365){
    dataIBNR[, repDelay := (year(repDate) - accYear)]
  } else if(perLen == 90){
    dataIBNR[year(repDate) != accYear, repDelay := (year(repDate) - accYear - 1)*4 + pmin(ceiling(month(repDate)/3), 4)]
    # dataIBNR[, repDelay := pmax(0, (year(repDate) - accYear - 1)*4 + pmin(ceiling(month(repDate)/3), 4))]
  }
  
  table(dataIBNR$repDelay)
  
  asNumericDT(dataIBNR, c('repDelay', 'accYear'))
  dataIBNR <- dataIBNR[!duplicated(dataIBNR[, .(polNumb, accYear, repDelay)]), .(polNumb, accYear, repDelay)]
  dataIBNR <- dataIBNR[order(accYear, repDelay), .(numbClaims = .N), by = .(accYear, repDelay)]
  
  # Define 'accEnd' as the latest/most recent accident year in 'dataIBNR'.
  accEnd <- max(dataIBNR[, .(accYear)])
  
  # Define 'oldestAccYear' as 'accEnd' minus 'maxDiffYear'.
  oldestAccYear <- accEnd - maxDiffYear
  if(verbose){
    print(paste0('Percentage of lines retained (effect maxDiffYear) ', round(sum(dataIBNR[accYear > oldestAccYear, numbClaims])/sum(dataIBNR[, numbClaims]), digits = 4)*100))
    print(paste0('Percentage of lines removed (effect maxDiffYear) ', round(sum(dataIBNR[accYear <= oldestAccYear, numbClaims])/sum(dataIBNR[, numbClaims]), digits = 4)*100))
  }
  
  # Throw away claims that are older than 'oldestAccYear'.
  dataIBNR <- dataIBNR[accYear > oldestAccYear, ]
  # dataIBNRSumm <- dataIBNR[order(accYear), .(sum(numbClaims)), by = .(accYear)]
  dataIBNR <- dataIBNR[order(accYear, repDelay), .(numbClaims = sum(numbClaims)), by = .(accYear, repDelay)]
  asFactorDT(dataIBNR, c('repDelay', 'accYear'))
  
  # Do a poisson regression for the number of claims that happened in each year by accident date and reporting delay .
  regPoisson <- glm(numbClaims ~ accYear + repDelay, family = poisson(link = log), data = dataIBNR)
  # summary(regPoisson)
  
  # dataIBNR[accYear != '2019' & repDelay == '1',]
  # dataIBNR[accYear != '2019' & repDelay == '0',]
  #
  # p1 <- dataIBNR[accYear != '2019' & repDelay == '1', (numbClaims = sum(numbClaims))]/dataIBNR[accYear != '2019' & repDelay == '0', (numbClaims = sum(numbClaims))]
  # p2 <- dataIBNR[!(accYear %in% c('2018', '2019')) & repDelay == '2', (numbClaims = sum(numbClaims))]/dataIBNR[!(accYear %in% c('2018', '2019')) & repDelay == '1', (numbClaims = sum(numbClaims))]
  # p3 <- dataIBNR[!(accYear %in% c('2017', '2018', '2019')) & repDelay == '3', (numbClaims = sum(numbClaims))]/dataIBNR[!(accYear %in% c('2017', '2018', '2019')) & repDelay == '2', (numbClaims = sum(numbClaims))]
  # p4 <- dataIBNR[!(accYear %in% c('2016', '2017', '2018', '2019')) & repDelay == '4', (numbClaims = sum(numbClaims))]/dataIBNR[!(accYear %in% c('2016', '2017', '2018', '2019')) & repDelay == '3', (numbClaims = sum(numbClaims))]
  # p5 <- dataIBNR[!(accYear %in% c('2015', '2016', '2017', '2018', '2019')) & repDelay == '5', (numbClaims = sum(numbClaims))]/dataIBNR[!(accYear %in% c('2015', '2016', '2017', '2018', '2019')) & repDelay == '4', (numbClaims = sum(numbClaims))]
  # p6 <- dataIBNR[!(accYear %in% c('2014', '2015', '2016', '2017', '2018', '2019')) & repDelay == '6', (numbClaims = sum(numbClaims))]/dataIBNR[!(accYear %in% c('2014', '2015', '2016', '2017', '2018', '2019')) & repDelay == '5', (numbClaims = sum(numbClaims))]
  #
  # p1*dataIBNR[accYear == '2019' & repDelay == '0', (numbClaims = sum(numbClaims))]
  # p1*p2*dataIBNR[accYear == '2019' & repDelay == '0', (numbClaims = sum(numbClaims))]
  # p1*p2*p3*dataIBNR[accYear == '2019' & repDelay == '0', (numbClaims = sum(numbClaims))]
  # p1*p2*p3*p4*dataIBNR[accYear == '2019' & repDelay == '0', (numbClaims = sum(numbClaims))]
  # p1*p2*p3*p4*p5*dataIBNR[accYear == '2019' & repDelay == '0', (numbClaims = sum(numbClaims))]
  # p1*p2*p3*p4*p5*p6*dataIBNR[accYear == '2019' & repDelay == '0', (numbClaims = sum(numbClaims))]
  #
  # p2*dataIBNR[accYear == '2018' & repDelay == '1', (numbClaims = sum(numbClaims))]
  # p2*p3*dataIBNR[accYear == '2018' & repDelay == '1', (numbClaims = sum(numbClaims))]
  # p2*p3*p4*dataIBNR[accYear == '2018' & repDelay == '1', (numbClaims = sum(numbClaims))]
  # p2*p3*p4*p5*dataIBNR[accYear == '2018' & repDelay == '1', (numbClaims = sum(numbClaims))]
  # p2*p3*p4*p5*p6*dataIBNR[accYear == '2018' & repDelay == '1', (numbClaims = sum(numbClaims))]
  #
  # p3*dataIBNR[accYear == '2017' & repDelay == '2', (numbClaims = sum(numbClaims))]
  # p3*p4*dataIBNR[accYear == '2017' & repDelay == '2', (numbClaims = sum(numbClaims))]
  # p3*p4*p5*dataIBNR[accYear == '2017' & repDelay == '2', (numbClaims = sum(numbClaims))]
  # p3*p4*p5*p6*dataIBNR[accYear == '2017' & repDelay == '2', (numbClaims = sum(numbClaims))]
  #
  # p4*dataIBNR[accYear == '2016' & repDelay == '3', (numbClaims = sum(numbClaims))]
  # p4*p5*dataIBNR[accYear == '2016' & repDelay == '3', (numbClaims = sum(numbClaims))]
  # p4*p5*p6*dataIBNR[accYear == '2016' & repDelay == '3', (numbClaims = sum(numbClaims))]
  #
  # p5*dataIBNR[accYear == '2015' & repDelay == '4', (numbClaims = sum(numbClaims))]
  # p5*p6*dataIBNR[accYear == '2015' & repDelay == '4', (numbClaims = sum(numbClaims))]
  #
  # p6*dataIBNR[accYear == '2014' & repDelay == '5', (numbClaims = sum(numbClaims))]
  #
  # temp2019 <- 0
  # temp2019 <- p1*dataIBNR[accYear == '2019' & repDelay == '0', (numbClaims = sum(numbClaims))]
  # temp2019 <- temp2019 + p1*p2*dataIBNR[accYear == '2019' & repDelay == '0', (numbClaims = sum(numbClaims))]
  # temp2019 <- temp2019 + p1*p2*p3*dataIBNR[accYear == '2019' & repDelay == '0', (numbClaims = sum(numbClaims))]
  # temp2019 <- temp2019 + p1*p2*p3*p4*dataIBNR[accYear == '2019' & repDelay == '0', (numbClaims = sum(numbClaims))]
  # temp2019 <- temp2019 + p1*p2*p3*p4*p5*dataIBNR[accYear == '2019' & repDelay == '0', (numbClaims = sum(numbClaims))]
  # temp2019 <- temp2019 + p1*p2*p3*p4*p5*p6*dataIBNR[accYear == '2019' & repDelay == '0', (numbClaims = sum(numbClaims))]
  #
  # temp2018 <- 0
  # temp2018 <- p2*dataIBNR[accYear == '2018' & repDelay == '1', (numbClaims = sum(numbClaims))]
  # temp2018 <- temp2018 + p2*p3*dataIBNR[accYear == '2018' & repDelay == '1', (numbClaims = sum(numbClaims))]
  # temp2018 <- temp2018 + p2*p3*p4*dataIBNR[accYear == '2018' & repDelay == '1', (numbClaims = sum(numbClaims))]
  # temp2018 <- temp2018 + p2*p3*p4*p5*dataIBNR[accYear == '2018' & repDelay == '1', (numbClaims = sum(numbClaims))]
  # temp2018 <- temp2018 + p2*p3*p4*p5*p6*dataIBNR[accYear == '2018' & repDelay == '1', (numbClaims = sum(numbClaims))]
  #
  # temp2017 <- 0
  # temp2017 <- temp2017 + p3*dataIBNR[accYear == '2017' & repDelay == '2', (numbClaims = sum(numbClaims))]
  # temp2017 <- temp2017 + p3*p4*dataIBNR[accYear == '2017' & repDelay == '2', (numbClaims = sum(numbClaims))]
  # temp2017 <- temp2017 + p3*p4*p5*dataIBNR[accYear == '2017' & repDelay == '2', (numbClaims = sum(numbClaims))]
  # temp2017 <- temp2017 + p3*p4*p5*p6*dataIBNR[accYear == '2017' & repDelay == '2', (numbClaims = sum(numbClaims))]
  #
  # temp2016 <- 0
  # temp2016 <- temp2016 + p4*dataIBNR[accYear == '2016' & repDelay == '3', (numbClaims = sum(numbClaims))]
  # temp2016 <- temp2016 + p4*p5*dataIBNR[accYear == '2016' & repDelay == '3', (numbClaims = sum(numbClaims))]
  # temp2016 <- temp2016 + p4*p5*p6*dataIBNR[accYear == '2016' & repDelay == '3', (numbClaims = sum(numbClaims))]
  #
  # temp2015 <- 0
  # temp2015 <- temp2015 + p5*dataIBNR[accYear == '2015' & repDelay == '4', (numbClaims = sum(numbClaims))]
  # temp2015 <- temp2015 + p5*p6*dataIBNR[accYear == '2015' & repDelay == '4', (numbClaims = sum(numbClaims))]
  #
  # temp2014 <- 0
  # temp2014 <- p6*dataIBNR[accYear == '2014' & repDelay == '5', (numbClaims = sum(numbClaims))]
  #
  # temp2019 + temp2018 + temp2017 + temp2016 + temp2015 + temp2014
  #
  # temp2019
  # temp2018
  # temp2017
  # temp2016
  # temp2015
  # temp2014
  #
  #
  # selYears <- 2013:2019
  # selDevs <- 0:6
  #
  # IBNRTriangle <- matrix(NA, nrow = length(selYears), ncol = length(selDevs))
  #
  # for(iRow in 1:length(selYears)){
  #   for(iCol in 1:length(selDevs)){
  #     if(iCol > abs(length(selYears) - iRow + 1)*round(365/perLen, digits = 0)){
  #       predData <- as.data.table(cbind(accYear = selYears[iRow], repDelay = selDevs[iCol]))
  #       asFactorDT(predData, c('accYear', 'repDelay'))
  #       IBNRTriangle[iRow, iCol] <- predict(regPoisson, newdata = predData, type = 'response')
  #     }
  #   }
  # }
  #
  # nIBNR <- round(sum(IBNRTriangle, na.rm = T), digits = 1)
  # library(plyr)
  # round(aaply(IBNRTriangle, 1, sum, na.rm = T), digits = 1)
  #
  # predData <- as.data.table(cbind(accYear = rep(2013:2019, 5), repDelay = rep(1:5, each  = 7)))
  # asFactorDT(predData, c('accYear', 'repDelay'))
  # predict(regPoisson, newdata = predData, type = 'response')
  # predict(regPoisson)
  
  nAccYears <- length(levels(dataIBNR$accYear))
  nDelays <- length(levels(dataIBNR$repDelay))
  
  coeffBeta <- regPoisson$coefficients[(nAccYears + 1):(nAccYears + nDelays - 1)]
  propDelay0 <- log(1/(1 + sum(exp(coeffBeta))))
  betas <- exp(c(propDelay0, coeffBeta + propDelay0))
  
  coeffAlphaTranslate <- regPoisson$coefficients[1:nAccYears] - propDelay0
  alphas <- exp(coeffAlphaTranslate)
  alphas <- alphas*c(1, rep(alphas[1], (length(alphas) - 1)))
  names(alphas)[1] <- paste("accYear", oldestAccYear + 1, sep = "")
  names(betas)[1] <- "repDelay0"
  
  # plot(cumsum(coeffBetaNorm), type = 'l')
  # abline(v = round(365/perLen, digits = 0)*1:length(coeffAlpha))
  
  obsDelays <- as.numeric(str_replace_all(names(betas), "repDelay","")[-1])
  
  betasMod <- rep(0, (maxDiffYear-1)*round(365/perLen, digits = 0))
  betasMod[obsDelays] <- betas[-1]
  
  IBNRTriangle <- matrix(NA, nrow = length(alphas), ncol = length(betasMod))
  
  for(iRow in 1:length(alphas)){
    for(iCol in 1:length(betasMod)){
      if(iCol > abs(length(alphas) - iRow)*round(365/perLen, digits = 0)){
        IBNRTriangle[iRow, iCol] <- alphas[iRow]*betasMod[iCol]
      }
    }
  }
  
  nIBNR <- round(sum(IBNRTriangle, na.rm = T), digits = 1)
  nIBNRAccYear <- as.data.table(cbind(accYear = str_replace_all(names(alphas), "accYear",""), nIBNR = round(aaply(IBNRTriangle, 1, sum, na.rm = T), digits = 1)))
  
  return(list(nIBNR = nIBNR, nIBNRAccYear = nIBNRAccYear, betas = c(betas[1], betasMod)))
}

## fitMN
#' fitMN is a function that returns a multinomial regression model using the given formula \code{form}.
#'
#' @param modDT Dataset used in this function.
#' @param form Formula for the multinomial regression model.
#' @param modVars Additional variables for the time process modelling - default value is NULL.
#' @param isFact Indicator function to see whether modVars are factor variables or not - default value is NULL.
#' @param nSamp The number of samples to draw from the dataset when fitting the multinomial regression model - default value is 10000.
#' @param nIterMax Maximum number of iterations in the fitting of the multinomial regression model - default value is 1000.
#' @param verboseConv Boolean: switch for tracing optimization of nnet:multinom - default value is FALSE.
#'
#' @return A multinomial regression model using the given formula \code{form}.
#
fitMN <- function(modDT, form, modVars = NULL, isFact = NULL, nSamp = 10000, nIterMax = 1000, verboseConv = FALSE){
  # Check input parameters and certain variables in 'modDT'.
  #changeOutcome(inputDT[[iList]], 'notT') #this needs to be tested
  checkDT(modDT, c('inStateTimeTrans', 'transMN', modVars))
  removeEmptyLevelsDT(modDT)
  checkEqualLength(list(modVars, isFact))
  if(!is.null(modVars)) checkCharVec(list(modVars))
  if(!is.null(isFact)) checkLogicVec(list(isFact))
  checkLength(list(form, nSamp, nIterMax), c(3, 1, 1))
  
  if(typeof(form) != 'language'){
    stop("The 'form' argument should be a language or formula object.")
  }
  
  checkWholeNumb(list(nSamp, nIterMax))
  
  # Only sample if the size of the full data set is too small (need to add something for this over here).
  if(nrow(modDT) >= nSamp){
    # The next line is added to make sure that each possible level is present in each sampled data set
    # (as to avoid models with a different number of parameters over the different samples).
    if(is.null(modVars)){
      dataUniq <- modDT[,.SD[sample(.N,min(.N, 2))], by = .(inStateTimeTrans, transMN)]
    } else {
      dataUniq <- modDT[,.SD[sample(.N,min(.N, 2))], by = .(get(modVars[isFact]), inStateTimeTrans, transMN)][, get := NULL]
    }
    # Sample nSamp-nrow(dataUniq) rows from modDT.
    selInds <- sample(1:nrow(modDT), pmax(nSamp - nrow(dataUniq), 0))
    
    # Fit a multinomial regression model using 'form'.
    finalMod <- nnet::multinom(formula = form, data = rbind(modDT[selInds,], dataUniq), MaxNWts = pmax(5000, nSamp), maxit = nIterMax, trace = verboseConv)
  } else {
    # Fit a multinomial regression model using 'form'.
    finalMod <- nnet::multinom(formula = form, data = modDT, MaxNWts = pmax(5000, nSamp), maxit = nIterMax, trace = verboseConv)
  }
  return(finalMod)
}

## fitRF
#' fitRF is a function that returns a random forest model using the given formula \code{form}.
#'
#' @param modDT Dataset used in this function.
#' @param form Formula for the random forest model.
#' @param modVars Additional variables for the time process modelling - default value is NULL.
#' @param isFact Indicator function to see whether modVars are factor variables or not - default value is NULL.
#' @param nSamp The number of samples to draw from the dataset when fitting the multinomial regression model - default value is 10000.
#' @param nMaxNodes Maximum number of terminal nodes trees in the forest can have - default value is 128.
#' @param nTrees Number of trees to grow in the random forest model. This should not be set to too small a number,
#'               to ensure that every input row gets predicted at least a few times - default value is 300.
#' @param verboseConv Boolean indicating whether output regarding convergence should be printed or not - default value is FALSE.
#'
#' @return A  random forest model using the given formula \code{form}.
#
#changed 150420 (the whole fitRF function)
fitRF <- function(modDT, form, modVars = NULL, isFact = NULL, nSamp = 10000, nMaxNodes = 128, nTrees = 300, verboseConv = FALSE){
  # Check input parameters and certain variables in 'modDT'.
  #changeOutcome(inputDT[[iList]], 'notT') #this needs to be tested
  checkDT(modDT, c('inStateTimeTrans', 'transMN', modVars))
  removeEmptyLevelsDT(modDT)
  checkEqualLength(list(modVars, isFact))
  if(!is.null(modVars)) checkCharVec(list(modVars))
  if(!is.null(isFact)) checkLogicVec(list(isFact))
  checkLength(list(form, nSamp), c(3, 1, 1))
  
  if(typeof(form) != 'language'){
    stop("The 'form' argument should be a language or formula object.")
  }
  
  checkWholeNumb(list(nSamp))
  
  # Only sample if the size of the full data set is too small (need to add something for this over here).
  if(nrow(modDT) >= nSamp){
    # The next line is added to make sure that each possible level is present in each sampled data set
    # (as to avoid models with a different number of parameters over the different samples).
    if(is.null(modVars)){
      dataUniq <- modDT[,.SD[sample(.N,min(.N, 2))], by = .(inStateTimeTrans, transMN)]
    } else {
      dataUniq <- modDT[,.SD[sample(.N,min(.N, 2))], by = .(get(modVars[isFact]), inStateTimeTrans, transMN)][, get := NULL]
    }
    # Sample nSamp-nrow(dataUniq) rows from modDT.
    selInds <- sample(1:nrow(modDT), pmax(nSamp - nrow(dataUniq), 0))
    
    # Fit a random forest model using 'form'.
    finalMod <- randomForest(formula = form, data = rbind(modDT[selInds,], dataUniq), maxnodes = nMaxNodes, ntree = nTrees, importance = TRUE, verbose = verboseConv)
  } else {
    # Fit a random forest model using 'form'.
    finalMod <- randomForest(formula = form, data = modDT, maxnodes = nMaxNodes, ntree = nTrees, importance = TRUE, verbose = verboseConv)
  }
  return(finalMod)
}

## fitOR
#' fitOR is a function that returns a proportional odds logistic regression model using the given formula \code{form}.
#'
#' @param modDT Dataset used in this function.
#' @param form Formula for the regression model.
#' @param modVars Additional variables for the time process modelling - default value is NULL.
#' @param isFact Indicator function to see whether modVars are factor variables or not - default value is NULL.
#' @param nSamp The number of samples to draw from the dataset when fitting the multinomial regression model - default value is 10000.
#' @param nIterMax Maximum number of iterations in the fitting of the multinomial regression model - default value is 1000.
#' @param verboseConv NOT USED IN THE FUNCTION!
#'
#' @return A proportional odds logistic regression model using the given formula \code{form}.
#
fitOR <- function(modDT, form, modVars = NULL, isFact = NULL, nSamp = 10000, nIterMax = 1000, verboseConv = FALSE){
  # Check input parameters and certain variables in 'modDT'.
  #changeOutcome(inputDT[[iList]], 'notT') #this needs to be tested
  checkDT(modDT, c('inStateTimeTrans', 'transMN', modVars))
  removeEmptyLevelsDT(modDT)
  checkEqualLength(list(modVars, isFact))
  if(!is.null(modVars)) checkCharVec(list(modVars))
  if(!is.null(isFact)) checkLogicVec(list(isFact))
  checkLength(list(form, nSamp, nIterMax), c(3, 1, 1))
  
  if(typeof(form) != 'language'){
    stop("The 'form' argument should be a language or formula object.")
  }
  
  checkWholeNumb(list(nSamp, nIterMax))
  
  # Only sample if the size of the full data set is too small (need to add something for this over here).
  if(nrow(modDT) >= nSamp){
    # The next line is added to make sure that each possible level is present in each sampled data set
    # (as to avoid models with a different number of parameters over the different samples).
    if(is.null(modVars)){
      dataUniq <- modDT[,.SD[sample(.N,min(.N, 2))], by = .(inStateTimeTrans, transMN)]
    } else {
      dataUniq <- modDT[,.SD[sample(.N,min(.N, 2))], by = .(get(modVars[isFact]), inStateTimeTrans, transMN)][, get := NULL]
    }
    # Sample nSamp-nrow(dataUniq) rows from modDT.
    selInds <- sample(1:nrow(modDT), pmax(nSamp - nrow(dataUniq), 0))
    
    # Fit a proportional odds logistic regression model using 'form'.
    finalMod <- polr(formula = form, data = rbind(modDT[selInds,], dataUniq))
  } else {
    # Fit a proportional odds logistic regression model using 'form'.
    finalMod <- polr(formula = form, data = modDT)
  }
  return(finalMod)
}

## newArgCheck
#' newArgCheck is a function that returns a new environment to which the following names with corresponding values are assigned.
#' - "n_warn" with value 0
#' - "warn_msg" with value NULL
#' - "n_error"  with value 0
#' - "error_msg" with value NULL
#' - "n_message"  with value 0
#' - "message_msg" with value NULL
#'
#' @return A new environment to which the names and values explained in the description are assigned.
#
newArgCheck <- function() {
  argcheck <- new.env()
  assign("n_warn", 0, envir = argcheck)
  assign("warn_msg", NULL, envir = argcheck)
  assign("n_error", 0, envir = argcheck)
  assign("error_msg", NULL, envir = argcheck)
  assign("n_message", 0, envir = argcheck)
  assign("message_msg", NULL, envir = argcheck)
  class(argcheck) <- c("ArgCheck", "environment")
  return(argcheck)
}

addError <- function(msg, argcheck){
  
  if(!"ArgCheck" %in% class(argcheck)) stop("'argcheck' must be an object of class 'ArgCheck'")
  assign("n_error", get("n_error", envir = argcheck) + 1, envir = argcheck)
  assign("error_msg", c(get("error_msg", envir = argcheck), msg), envir = argcheck)
}

finishArgCheck <- function(argcheck, justWarnings = FALSE){
  fn_call <- sys.call(-1)
  fn_call <- utils::capture.output(fn_call)
  if(!"ArgCheck" %in% class(argcheck)) stop("'argcheck' must be an object of class 'ArgCheck'")
  argcheck <- mget(ls(envir = argcheck), envir = argcheck)
  if(argcheck$n_warn > 0)
    warning(paste0(c("", fn_call, paste0(1:argcheck$n_warn, ": ", argcheck$warn_msg)), collapse = "\n"), call. = FALSE)
  if(argcheck$n_message > 0)
    message(paste0(c("", fn_call, paste0(1:argcheck$n_message, ": ", argcheck$message_msg)), collapse = "\n"))
  if(argcheck$n_error > 0){
    if(justWarnings == FALSE){
      stop(paste0(c("", fn_call, paste0(1:argcheck$n_error, ": ", argcheck$error_msg)), collapse = "\n"), call. = FALSE)
    } else {
      warning(paste0(c("", fn_call, paste0(1:argcheck$n_error, ": ", argcheck$error_msg)), collapse = "\n"), call. = FALSE)
    }
  }
}

checkEqualLength <- function(listObjects, ...){
  namesListObjects <- deparse(substitute(listObjects))
  if(str_count(namesListObjects, "list[(]") > 1){
    stop('All the objects to be checked need to be put in a list. A list of lists is not permitted by this function.')
  }
  if(nargs() > 1){
    stop("Just one separate argument needs to be provided for.")
  }
  if(length(listObjects) == 1){
    stop("The names of at least 2 different objects should be stored in the 'listObjects' argument.")
  }
  
  namesListObjects <- unlist(strsplit(namesListObjects, '[,]'))
  namesListObjects <- gsub("\\s", "", namesListObjects)
  namesListObjects <- str_replace_all(namesListObjects, fixed("list("), "")
  namesListObjects <- str_replace_all(namesListObjects, fixed(")"), "")
  isObject <- aaply(namesListObjects, 1, function(xx) exists(xx, envir = parent.frame()))
  
  checkList <- newArgCheck()
  
  lengthObjects <- laply(listObjects, length)
  nElements <- length(lengthObjects)
  comparison <- rep(NA, (nElements*(nElements - 1))/2)
  counter <- 1
  for(iLeft in 1:(nElements - 1)){
    for(iRight in (iLeft + 1):nElements){
      if(!identical(lengthObjects[iLeft], lengthObjects[iRight])){
        if(isObject[iLeft] & isObject[iRight]){
          addError(msg = paste(paste(paste(paste("The length of argument '", namesListObjects[iLeft], sep = ''), "' differs in length from the length of argument '", sep = ''), namesListObjects[iRight], sep = ''), "'.", sep = ''), argcheck = checkList)
        } else if(isObject[iLeft] & !isObject[iRight]){
          addError(msg = paste(paste(paste(paste("The length of argument '", namesListObjects[iLeft], sep = ''), "' differs in length from the length of the element with index ", sep = ''), iRight, sep = ''), '.', sep = ''), argcheck = checkList)
        } else if(!isObject[iLeft] & isObject[iRight]){
          addError(msg = paste(paste(paste(paste("The length of the element with index ", iLeft, sep = ''), " differs in length from the length of argument '", sep = ''), namesListObjects[iRight], sep = ''), "'.", sep = ''), argcheck = checkList)
        } else{
          addError(msg = paste(paste(paste(paste("The length of the element with index ", iLeft, sep = ''), " differs in length from the length of the element with index ", sep = ''), iRight, sep = ''), ".", sep = ''), argcheck = checkList)
        }
      }
    }
  }
  finishArgCheck(checkList)
}
## checkWholeNumb
#' Checking if all elements of \code{listNum} are all numeric or integer vectors with just whole numbers.
#'
#' @param listNum A list of the vectors of which one wishes to check if their data type is numeric.
#' @param namesListElements Character vector containing the names of the variables of which the data type is checked. Optional parameter, with as default value NULL. This argument should be used when the variable of which the data type is checked is not an object that was provided as an argument to the function, or when the list elements of the first argument do not have a name attached to it.
#'
#' @return No value is returned if all vectors have the numeric or integer data type, containing just whole numbers. If not, an error message is thrown for each element of the list that does not pertain to the numeric data type.
#'
#' @examples
#' arg1 <- 2
#' checkWholeNumb(list(arg1))
#' checkWholeNumb(list(2))
#'
#' arg2 <- 0.8
#' \donttest{checkWholeNumb(list(arg2))}
#' \donttest{checkWholeNumb(list(1, arg2))}
#
checkWholeNumb <- function(listNum, namesListElements = NULL){
  if(!is.list(listNum)) stop("The argument 'listNum' needs to be a list.")
  
  for(iList in 1:length(listNum)){
    if(is.list(listNum[[iList]])){
      stop(paste(paste('The data type of element with index ', iList, sep = ''), ' is NOT a vector. A list of lists is not permitted by this function.', sep = ''))
    }
  }
  
  checkList <- newArgCheck()
  argNames <- deparse(substitute(listNum))
  argNames <- unlist(strsplit(argNames, '[,]'))
  argNames <- gsub("\\s", "", argNames)
  argNames <- str_replace_all(argNames, fixed("list("), "")
  argNames <- str_replace_all(argNames, fixed(")"), "")
  isObject <- aaply(argNames, 1, function(xx) exists(xx, envir = parent.frame()))
  
  if(!is.null(namesListElements)){
    if(!is.character(namesListElements)) stop("The argument 'namesListElements' should be a character vector or string.")
    if(length(listNum) != length(namesListElements)) stop("The argument 'listNum' should have the same length as argument 'namesListElements'.")
  }
  
  if(is.list(listNum)){
    for(iList in 1:length(listNum)){
      checkNumOrIntVec(list(listNum[[iList]]))
      if(!all(listNum[[iList]] == round(listNum[[iList]])) & !is.null(listNum[[iList]])){
        if(isObject[iList]){
          addError(msg = paste(paste("The argument '", argNames[iList], sep = ''), "' does NOT consist of whole numbers.", sep = ''), argcheck = checkList)
        } else {
          if(is.null(namesListElements)){
            addError(msg = paste(paste('The element with index ', iList, sep = ''), ' does NOT consist of whole numbers.', sep = ''), argcheck = checkList)
          } else {
            addError(msg = paste(paste("The argument '", namesListElements[iList], sep = ''), "' does NOT consist of whole numbers.", sep = ''), argcheck = checkList)
          }
        }
      }
    }
  } else if(is.vector(listNum) & length(listNum) == 1){
    checkNumOrIntVec(list(listNum))
    if(!all(listNum == round(listNum)) & !is.null(listNum)){
      if(isObject){
        addError(msg = paste(paste("The argument '", argNames, sep = ''), "' does NOT consist of whole numbers.", sep = ''), argcheck = checkList)
      } else {
        if(is.null(namesListElements)){
          addError(msg = paste(paste('The element with index ', '1', sep = ''), ' does NOT consist of whole numbers.', sep = ''), argcheck = checkList)
        } else {
          addError(msg = paste(paste("The argument '", namesListElements, sep = ''), "' does NOT consist of whole numbers.", sep = ''), argcheck = checkList)
        }
      }
    }
  } else {
    addError(msg = 'The "listNum" argument should be of a list.', argcheck = checkList)
  }
  finishArgCheck(checkList)
}

#old version -> just holding onto it until the new version works perfectly

# transFit <- function(inputDT, nMinMod, nMinLev, nTimesParams, nMaxNoMod, nSamp = 10000, nTimes = 10, nIterMax = 1000, verbose = TRUE, verboseConv = FALSE){
#
#   if(!is.list(inputDT)) stop("The 'inputDT' argument needs to be a list of data.tables objects.")
#   checkWholeNumb(list(nMaxNoMod, nMaxNoMod))
#   checkWholeNumb(list(nSamp, nTimes, nIterMax))
#   checkLogicVec(list(verbose, verboseConv))
#   checkLength(list(nSamp, nTimes, nIterMax, verbose, verboseConv), 1)
#   checkLength(list(nMinMod, nMinLev, nTimesParams, nMaxNoMod), 1)
#   checkLength(list(nSamp, nTimes, nIterMax, verbose, verboseConv), 1)
#   llply(inputDT, function(xx) checkDT(xx, c('inStateTimeTrans', 'transType', 'inProcTimeTrans', 'transStat')))
#   finMods <- list()
#
#   for(iList in 1:length(inputDT)){
#
#     if(verbose) print(paste0('Fitting transition ', iList))
#     #constructing the default formula, expressed by the var form
#     if(length(unique(inputDT[[iList]]$inStateTimeTrans)) == 1){
#       form <- as.formula('transMN ~ 1')
#     } else {
#       form <- as.formula('transMN ~ inStateTimeTrans')
#     }
#
#     #the formula needs to be adapted in case that there are more than nMinMod observations and in case that there are modVars
#     if(nrow(inputDT[[iList]]) > nMinMod & length(modVars) > 0){
#
#       indVars <- !names(inputDT[[iList]]) %in% c('inStateTimeTrans', 'transType', 'inProcTimeTrans', 'transStat', 'transMN', 'polNumb')
#       modVars <- names(inputDT[[iList]])[indVars]
#
#       #computing the number of parameters (to gauge the model complexity), expresssed by the var nParamTot
#       isFact <- isFactorDT(inputDT[[iList]][, .SD, .SDcol = indVars])
#       nParam <- rep(1, length(modVars))
#       allVarsLevs <- list()
#       length(allVarsLevs) <- length(modVars) + 1
#       names(allVarsLevs) <- c(modVars, 'inStateTimeTrans')
#       for(iVar in 1:length(modVars)){
#         if(isFact[iVar]){
#           levs <- extractLevelDT(inputDT[[iList]], modVars[iVar])[[1]]
#           refLev <- extractRefLevelDT(inputDT[[iList]], modVars[iVar])[[1]]
#           levs <- levs[levs != refLev]
#           nParam[iVar] <- length(levs)
#           allVarsLevs[[iVar]] <- levs
#         }
#       }
#       inStateLevs <- extractLevelDT(inputDT[[iList]], 'inStateTimeTrans')[[1]]
#       refInStateLev <- extractRefLevelDT(inputDT[[iList]], 'inStateTimeTrans')[[1]]
#       inStateLevs <- inStateLevs[inStateLevs != refInStateLev]
#       allVarsLevs$inStateTimeTrans <- inStateLevs
#       intLev <- length(inStateLevs)
#
#       nParamTot <- intLev + sum(nParam) + sum(nParam*intLev) + 1
#       nSamp <- pmax(nSamp, nParamTot*nTimesParams)
#
#       #constructing the formula, expressed by the var form
#
#       if(nrow(inputDT[[iList]]) > nParamTot*nTimesParams){
#         explVar <- modVars
#         if(length(unique(inputDT[[iList]]$inStateTimeTrans)) != 1){
#           explVar <- c(modVars, 'inStateTimeTrans', paste(c(modVars, 'inProcTimeTrans'), 'inStateTimeTrans', sep = '*')) #interactions with inStateTimeTrans are added to relax the PH assumption
#         }
#         form <- as.formula(paste('transMN ~', paste(explVar, collapse = " + ")))
#       } else {
#         form <- as.formula('transMN ~ 1')
#         if(length(unique(inputDT[[iList]]$inStateTimeTrans)) != 1){
#           form <- as.formula('transMN ~ inStateTimeTrans')
#         }
#       }
#     } else {
#       modVars <- NULL
#       isFact <- NULL
#     }
#
#     #fitting the multinomial (MN) model, if there are more than nMaxNoMod observations
#     if(nrow(inputDT[[iList]]) > nMaxNoMod){
#       finMods[[iList]] <- fitMN(inputDT[[iList]], form, modVars, isFact, nSamp, nTimes, nIterMax, verboseConv)
#     }
#   }
#
#   return(finMods)
#
# }

## timeFit
#' timeFit is a function that returns a multinomial logistic regression model /
#' random forest model fitted for each dataset in \code{inputDT}.
#'
#' @param inputDT List of datasets used in this function.
#' @param nMinMod Minimum number of observations to model with covariates.
#' @param nMinLev Minimum number of observations for each level of the variables \code{inStateTimeTrans} and \code{inProcTimeTrans}.
#' @param nTimesParams Minimum number of observations of the data fed to the fitting procedure should exceed the number of model parameters times this value.
#' @param nMaxNoMod  Maximum number of observations to model without covariates.
#' @param notInModVars The variables that should not be modelled as a main effect - default value is NULL.
#' @param intVars The variables for which there is an interaction term with \code{inStateTime} in the multinomial logistic regression models - default value is NULL.
#' @param nSamp Number of samples to take from the data - default value is 10000.
#' @param nIterMax Maximum number of iterations for the multinomial logistic regression model to iterate - default value is 1000.
#' @param verbose Boolean indicating whether output should be printed or not - default value is TRUE.
#' @param verboseConv Boolean indicating whether output regarding convergence should be printed or not - default value is FALSE.
#' @param RF Boolean indicating whether the random forest model should be used. In case FALSE, the multinomial logistic regression model is used - default value is FALSE.
#'
#' @return Multinomial logistic regression model / random forest model fitted for each dataset in \code{inputDT}.
#
timeFit <- function(inputDT, nMinMod, nMinLev, nTimesParams, nMaxNoMod, notInModVars = NULL, intVars = NULL, nSamp = 10000, nIterMax = 1000, verbose = TRUE, verboseConv = FALSE, RF = FALSE){ #changed 150420
  # Check input parameters and certain variables in 'inputDT'.
  if(!is.list(inputDT)) stop("The 'inputDT' argument needs to be a list of data.tables objects.")
  if(!is.null(notInModVars)) checkCharVec(list(notInModVars))
  if(!is.null(intVars)) checkCharVec(list(intVars))
  checkLogicVec(list(verbose, verboseConv))
  checkLength(list(verbose, nSamp, nIterMax, verboseConv), c(1, 1, 1, 1))
  checkWholeNumb(list(nMinMod, nMinLev, nTimesParams, nMaxNoMod))
  checkLength(list(nMinMod, nMinLev, nTimesParams, nMaxNoMod), 1)
  checkRanges(list(nSamp, nIterMax), list(c('>', 0), c('>', 0)))
  checkRanges(list(nMinMod, nMinLev, nTimesParams, nMaxNoMod), list(c('>', 0), c('>', 0), c('>', 0), c('>', 0)))
  llply(inputDT, function(xx) checkDT(xx, c('inStateTimeTrans', 'transType', 'inProcTimeTrans', 'transStat')))
  result <- list()
  
  for(iList in 1:length(inputDT)){
    
    result[[iList]] <- list()
    
    if(iList == 1){ 
      notInModVarsOrig = notInModVars
      notInModVars = c(notInModVars, 'inProcTimeTrans')
    }else if(iList == 2){ 
      notInModVars = c(notInModVarsOrig, 'cumDelt1PayGrouped')
    }else{
      notInModVars = c(notInModVarsOrig)
    }
    if(verbose) print(paste0('Fitting transition ', iList))
    if(verbose) print('>>> Prep phase')
    # Constructing the default formula, expressed by the variable 'form'.
    if(length(unique(inputDT[[iList]]$inStateTimeTrans)) == 1){
      form <- as.formula('transMN ~ 1')
    } else {
      form <- as.formula('transMN ~ inStateTimeTrans')
    }
    
    # The formula needs to be adapted in case that there are more than 'nMinMod' observations and in case that there are 'modVars'.
    if(nrow(inputDT[[iList]]) > nMinMod & length(modVars) > 0){
      
      # This gives 'deltNames': the names of the variables of 'inputDT[[iList]]' with 'delt' in their name.
      deltNames <- names(inputDT[[iList]])[grep('delt', names(inputDT[[iList]]))]
      
      # This gives 'payNames': the names of the variables of 'inputDT[[iList]]' with 'delt' and 'Pay', but without 'Time' in their name.
      payNames <- deltNames[grep('Pay', deltNames)]
      payNames <- payNames[!grepl('Time', payNames)]
      # payNames <- payNames[!grepl('Grouped', payNames)]
      
      # This gives 'deltNoPayNames': the names of the variables of 'inputDT[[iList]]' with 'delt', but without 'Pay' in their name.
      deltNoPayNames <- deltNames[!(deltNames %in% payNames)]
      
      indVars <- !names(inputDT[[iList]]) %in% c('inStateTimeTrans', 'transType', 'inProcTimeTrans', 'transStat', 'transMN', 'polNumb', 'inStateTimeMax', 'bookDate', 'finYear', 'outComeT', 'delt1Pay', 'delt2Pay', 'cumDelt1Pay', 'cumDelt2Pay', payNames, deltNoPayNames)
      modVars <- names(inputDT[[iList]])[indVars]
      
      if(isFactorDT(inputDT[[iList]], 'inStateTimeTrans')) asNumericDT(inputDT[[iList]], 'inStateTimeTrans') #changed 050520
      if(isFactorDT(inputDT[[iList]], 'inProcTimeTrans')) asNumericDT(inputDT[[iList]], 'inProcTimeTrans') #changed 050520
      catMerge(inputDT[[iList]], 'inStateTimeTrans', nMinLev, nMinMod) #changed 050520
      catMerge(inputDT[[iList]], 'inProcTimeTrans', nMinLev, nMinMod) #changed 050520
      if(sum(names(inputDT[[iList]]) == 'inStateTimeTransTrans')){ #changed 050520
        inputDT[[iList]][, inStateTimeTrans := NULL] #changed 050520
        setnames(inputDT[[iList]], 'inStateTimeTransTrans', 'inStateTimeTrans') #changed 050520
      } #changed 050520
      if(sum(names(inputDT[[iList]]) == 'inProcTimeTransTrans')){ #changed 050520
        inputDT[[iList]][, inProcTimeTrans := NULL] #changed 050520
        setnames(inputDT[[iList]], 'inProcTimeTransTrans', 'inProcTimeTrans') #changed 050520
      } #changed 050520
      if(!isFactorDT(inputDT[[iList]], 'inStateTimeTrans')) asFactorDT(inputDT[[iList]], 'inStateTimeTrans') #changed 050520
      if(!isFactorDT(inputDT[[iList]], 'inProcTimeTrans')) asFactorDT(inputDT[[iList]], 'inProcTimeTrans') #changed 050520
      
      # Computing the number of parameters (to gauge the model complexity), expresssed by the variable 'nParamTot'.
      isFact <- isFactorDT(inputDT[[iList]][, .SD, .SDcol = indVars])
      nParam <- rep(1, length(modVars))
      allVarsLevs <- list()
      length(allVarsLevs) <- length(modVars) + 1
      names(allVarsLevs) <- c(modVars, 'inStateTimeTrans')
      nIntParams <- 0
      selInt <- c()
      for(iVar in 1:length(modVars)){
        if(isFact[iVar]){
          levs <- extractLevelDT(inputDT[[iList]], modVars[iVar])[[1]]
          refLev <- extractRefLevelDT(inputDT[[iList]], modVars[iVar])[[1]]
          levs <- levs[levs != refLev]
          nParam[iVar] <- length(levs)
          allVarsLevs[[iVar]] <- levs
          if(sum(intVars %in% modVars[iVar])){
            nIntParams <- nIntParams + length(levs)
            selInt <- c(selInt, modVars[iVar])
          }
        } else {
          if(sum(intVars %in% modVars[iVar])){
            nIntParams <- nIntParams + 1
            selInt <- c(selInt, modVars[iVar])
          }
        }
      }
      inStateLevs <- extractLevelDT(inputDT[[iList]], 'inStateTimeTrans')[[1]]
      refInStateLev <- extractRefLevelDT(inputDT[[iList]], 'inStateTimeTrans')[[1]]
      inStateLevs <- inStateLevs[inStateLevs != refInStateLev]
      allVarsLevs$inStateTimeTrans <- inStateLevs
      intLev <- length(inStateLevs)
      
      nParamTotMax <- intLev + sum(nParam) + sum(nIntParams*intLev) + 1
      nSamp <- pmax(nSamp, nParamTotMax*nTimesParams)
      
      # Constructing the formula, expressed by the variable 'form'.
      if(nrow(inputDT[[iList]]) > nParamTotMax*nTimesParams){
        explVar <- modVars
        if(length(unique(inputDT[[iList]]$inStateTimeTrans)) != 1){
          explVar <- c(modVars, payNames, deltNoPayNames, 'inStateTimeTrans', 'inProcTimeTrans') #interactions with inStateTimeTrans are added to relax the PH assumption
          explVar <- explVar[!explVar %in% c('delt1Pay', 'delt2Pay', 'cumDelt1Pay', 'cumDelt2Pay')]
          if(!is.null(notInModVars)) explVar <- explVar[!(explVar %in% notInModVars)]
          # if(!is.null(intVars)) explVar <- c(explVar, paste(c(intVars[intVars %in% names(inputDT[[iList]])]), 'inStateTimeTrans', sep = '*'))
          if(length(selInt) > 0) explVar <- c(explVar, paste(selInt, 'inStateTimeTrans', sep = '*'))
        }
        
        
        form <- as.formula(paste('transMN ~', paste(explVar, collapse = " + ")))
      } else {
        form <- as.formula('transMN ~ 1')
        if(length(unique(inputDT[[iList]]$inStateTimeTrans)) != 1){
          form <- as.formula('transMN ~ inStateTimeTrans') #just the baseline hazard
        }
      }
    } else {
      modVars <- NULL
      isFact <- NULL
    }
    
    # Fitting the multinomial (MN) model or Random Forest (RF) model, if there are more than 'nMaxNoMod' observations.
    if(nrow(inputDT[[iList]]) > nMaxNoMod){
      if(verbose) print('>>> Fitting phase')
      if(!RF){ #changed 150420
        result[[iList]]$fitTime <- fitMN(inputDT[[iList]], form, modVars, isFact, nSamp, nIterMax, verboseConv)
      } else { #changed 150420
        result[[iList]]$fitTime <- fitRF(inputDT[[iList]], form, modVars, isFact, nSamp, 128, 300, verboseConv) #changed 150420
      } #changed 150420
      
      result[[iList]]$trainDT <- inputDT[[iList]]
    }
  }
  return(result)
}

## timeFit
#' timeFit is a function that returns a multinomial logistic regression model /
#' random forest model fitted for each dataset in \code{inputDT}.
#'
#' @param inputDT List of datasets used in this function.
#' @param nMinMod Minimum number of observations to model with covariates.
#' @param nMinLev Minimum number of observations for each level of the variables \code{inStateTimeTrans} and \code{inProcTimeTrans}.
#' @param nTimesParams Minimum number of observations of the data fed to the fitting procedure should exceed the number of model parameters times this value.
#' @param nMaxNoMod  Maximum number of observations to model without covariates.
#' @param notInModVars The variables that should not be modelled as a main effect - default value is NULL.
#' @param intVars The variables for which there is an interaction term with \code{inStateTime} in the multinomial logistic regression models - default value is NULL.
#' @param nSamp Number of samples to take from the data - default value is 10000.
#' @param nIterMax Maximum number of iterations for the multinomial logistic regression model to iterate - default value is 1000.
#' @param verbose Boolean indicating whether output should be printed or not - default value is TRUE.
#' @param verboseConv Boolean indicating whether output regarding convergence should be printed or not - default value is FALSE.
#' @param RF Boolean indicating whether the random forest model should be used. In case FALSE, the multinomial logistic regression model is used - default value is FALSE.
#'
#' @return Multinomial logistic regression model / random forest model fitted for each dataset in \code{inputDT}.
#
timeFitCont <- function(inputDT, nMinMod, nMinLev, nTimesParams, nMaxNoMod, notInModVars = NULL, intVars = NULL, nSamp = 10000, nIterMax = 1000, verbose = TRUE, verboseConv = FALSE, RF = FALSE){ #changed 150420
  result <- list()
  
  for(iList in 1:length(inputDT)){
    
    result[[iList]] <- list()
    
    if(iList == 1){ 
      notInModVarsOrig = notInModVars
      notInModVars = c(notInModVars, 'inProcTime')
    }else if(iList == 2){ 
      notInModVars = c(notInModVarsOrig, 'cumDelt1Pay')
    }else{
      notInModVars = c(notInModVarsOrig)
    }
    if(verbose) print(paste0('Fitting transition ', iList))
    if(verbose) print('>>> Prep phase')
    # Constructing the default formula, expressed by the variable 'form'.
    if(length(unique(inputDT[[iList]]$inStateTimeTrans)) == 1){
      form <- as.formula('transMN ~ 1')
    } else {
      form <- as.formula('transMN ~ inStateTime')
    }
    
    # The formula needs to be adapted in case that there are more than 'nMinMod' observations and in case that there are 'modVars'.
    if(nrow(inputDT[[iList]]) > nMinMod & length(modVars) > 0){
      
      # This gives 'deltNames': the names of the variables of 'inputDT[[iList]]' with 'delt' in their name.
      deltNames <- names(inputDT[[iList]])[grep('delt', names(inputDT[[iList]]))]
      
      # This gives 'payNames': the names of the variables of 'inputDT[[iList]]' with 'delt' and 'Pay', but without 'Time' in their name.
      payNames <- deltNames[grep('Pay', deltNames)]
      payNames <- payNames[!grepl('Time', payNames)]
      # payNames <- payNames[!grepl('Grouped', payNames)]
      
      # This gives 'deltNoPayNames': the names of the variables of 'inputDT[[iList]]' with 'delt', but without 'Pay' in their name.
      deltNoPayNames <- deltNames[!(deltNames %in% payNames)]
      
      indVars <- !names(inputDT[[iList]]) %in% c('inStateTime', 'transType', 'inProcTime', 'transStat', 'transMN', 'polNumb', 'inStateTimeMax', 'bookDate', 'finYear', 'outComeT', payNames, deltNoPayNames)
      modVars <- names(inputDT[[iList]])[indVars]
      
      # Computing the number of parameters (to gauge the model complexity), expresssed by the variable 'nParamTot'.
      isFact <- isFactorDT(inputDT[[iList]][, .SD, .SDcol = indVars])
      nParam <- rep(1, length(modVars))
      allVarsLevs <- list()
      length(allVarsLevs) <- length(modVars) + 1
      
      # Constructing the formula, expressed by the variable 'form'.
      explVar <- modVars
      if(length(unique(inputDT[[iList]]$inStateTimeTrans)) != 1){
        explVar <- c(modVars, payNames, deltNoPayNames, 'inStateTimeTrans', 'inProcTimeTrans') #interactions with inStateTimeTrans are added to relax the PH assumption
        explVar <- explVar[!explVar %in% c('delt1Pay', 'delt2Pay', 'cumDelt1Pay', 'cumDelt2Pay')]
        if(!is.null(notInModVars)) explVar <- explVar[!(explVar %in% notInModVars)]
        # if(!is.null(intVars)) explVar <- c(explVar, paste(c(intVars[intVars %in% names(inputDT[[iList]])]), 'inStateTimeTrans', sep = '*'))
        if(length(selInt) > 0) explVar <- c(explVar, paste(selInt, 'inStateTimeTrans', sep = '*'))
      }
      
      
      form <- as.formula(paste('transMN ~', paste(explVar, collapse = " + ")))
      
    } else {
      modVars <- NULL
      isFact <- NULL
    }
    
    # Fitting the multinomial (MN) model or Random Forest (RF) model, if there are more than 'nMaxNoMod' observations.
    if(nrow(inputDT[[iList]]) > nMaxNoMod){
      if(verbose) print('>>> Fitting phase')
      if(!RF){ #changed 150420
        result[[iList]]$fitTime <- fitMN(inputDT[[iList]], form, modVars, isFact, nSamp, nIterMax, verboseConv)
      } else { #changed 150420
        result[[iList]]$fitTime <- fitRF(inputDT[[iList]], form, modVars, isFact, nSamp, 128, 300, verboseConv) #changed 150420
      } #changed 150420
      
      result[[iList]]$trainDT <- inputDT[[iList]]
    }
  }
  return(result)
}

#TODO
createGroupedVarPay <- function(dataDT, varName, nGroupsFin, nGroups = 10, nMinLev=50, sampleSizeBin = 50000, nSamp = 10000, nTimes = 5, nIterMax = 1000, verboseConv = FALSE){
  # Check input parameters and certain variables in 'dataDT'.
  checkWholeNumb(list(nSamp, nTimes, nIterMax))
  checkLogicVec(list(verboseConv))
  checkLength(list(nSamp, nTimes, nIterMax, verboseConv), 1)
  checkDT(dataDT, c('linkRatio', 'inStateTimeTrans', varName))
  if(nGroupsFin > nGroups*(length(unique(dataDT$linkRatio)) - 1)){
    stop("The argument 'nGroupsFin' needs to be smaller than or equal to the 'nGroups' argument times the 1 minus the number of unique transitions.")
  }
  
  # The linkRatio model is fitted with 'inStateTimeTrans' and a grouped version of the 'varName' variable,
  # where the factor is based on upon the sequential 2.5 percentiles
  # (as to have a nice trade-off between model complexity and sparseness).
  
  varNameGr <- paste0(varName, 'GroupedPay')
  # Note that we don't have a problem anymore with the binning when the quantiles are not unique.
  splitsVar <- unique(quantile(dataDT[, get(varName)], seq(.025, .975, .025), na.rm = TRUE)) #changed 040520
  splits <- list(splitsVar)
  names(splits)[1] <- varName
  print(varName)
  transform2BinnedVar(dataDT, splits)
  medianVals <- dataDT[, median(get(varName), na.rm = TRUE), by = get(varNameGr)][order(get), ]
  setnames(medianVals, c('get', 'V1'), c(varNameGr, 'median'))
  medianVals <- medianVals[!is.na(median), ]
  
  form <- as.formula(paste0('linkRatio ~ inStateTimeTrans + ', varNameGr))
  modDelta <- fitMN(dataDT, form, varNameGr, rep(TRUE, length(varNameGr)), nSamp, nIterMax, verboseConv) #added
  
  # Getting predictions for all the possible values of the above created factor.
  # test <- data.table(inStateTimeTrans = rep(1, (length(splitsVar) + 1)), V1 = 1:(length(splitsVar) + 1))
  # setnames(test, 'V1', varNameGr)
  # asFactorDT(test, c('inStateTimeTrans', varNameGr))
  # predProbs <- predict(modDelta, newdata = test, type = "probs")
  
  modParams <- coef(modDelta)
  # modParamsDelta <- modParams[, grep('delt', colnames(modParams))]
  modParamsDelta <- modParams[, grep(varNameGr, colnames(modParams))]
  
  # For each of the transitions separately, the best splits are sought for.
  resBinning <- matrix(NA, nrow = nrow(modParamsDelta), ncol = nGroups - 1)
  
  for(transNr in 1:nrow(modParamsDelta)){
    
    dataLoess <- data.table(x = medianVals[-1, median], y = modParamsDelta[transNr, ])
    loessMod <- loess(y ~ x, data = dataLoess, span = 0.5)
    testData <- copy(dataDT)
    addFirstLast(testData, 'polNumb', F, T)
    testData <- testData[last == 1, ]
    setnames(testData, varName, 'x')
    yVals <- predict(loessMod, newdata = testData)
    testData[, y := yVals]
    resBinning[transNr, ] <- binningUnivSpline(testData, nGroups, min(nrow(testData), sampleSizeBin))
    
    # selInds <- sample(1:nrow(testData), 1000)
    # plot(testData[selInds, x], predict(loessMod, newdata = testData[selInds, ]), xlim = c(-3500, 2000))
    # abline(v = resBinning[transNr,], col = 2)
    
  }
  
  # Finding an unique set of splits (lower than the total sum of all generated splits), based upon the splits generated
  # for each separate transition, such that the remaining splits contain a balanced number of observations in each bin.
  # begin Jolien 
  splitsAll <- sort(unique(as.numeric(resBinning)))
  cdfSpl <- rep(NA, length(splitsAll))
  for(iSpl in 1:length(splitsAll)){
    cdfSpl[iSpl] <- sum(dataDT[, get(varName)] < splitsAll[iSpl], na.rm = TRUE)
  }
  
  diffSplCDF <- c(cdfSpl[1], cdfSpl[2:length(cdfSpl)] - cdfSpl[1:(length(cdfSpl) - 1)], sum(dataDT[, get(varName)] > splitsAll[length(splitsAll)], na.rm = TRUE))
  diffSpl <- diff(splitsAll)
  while(length(splitsAll) > (nGroupsFin - 1)){
    indMin <- which.min(diffSpl)
    
    if(diffSplCDF[indMin] < diffSplCDF[indMin + 2]){
      diffSplCDF[indMin] <- diffSplCDF[indMin] + diffSplCDF[indMin+1]
      splitsAll <- splitsAll[-(indMin)]
      diffSpl<- diff(splitsAll)
      diffSplCDF<- diffSplCDF[-(indMin)]
    } else {
      diffSplCDF[indMin+2] <- diffSplCDF[indMin+1] + diffSplCDF[indMin+2]
      splitsAll <- splitsAll[-(indMin+1)]
      diffSpl<- diff(splitsAll)
      diffSplCDF<- diffSplCDF[-(indMin+1)]
    }
    
  }
  
  while(any(diffSplCDF < nMinLev)){
    indMin <- which(diffSplCDF<nMinLev)[which.min(diffSplCDF[which(diffSplCDF<nMinLev)])]
    if(indMin == 1){
      diffSplCDF[2] <- diffSplCDF[1] + diffSplCDF[2]
      diffSpl <- diffSpl[-1]
      diffSplCDF <- diffSplCDF[-1]
      splitsAll <- splitsAll[-1]
    } else if(indMin == length(diffSplCDF)){
      diffSplCDF[length(diffSplCDF) - 1] <- diffSplCDF[length(diffSplCDF) - 1] + diffSplCDF[length(diffSplCDF)]
      diffSplCDF <- diffSplCDF[-length(diffSplCDF)]
      diffSpl <- diffSpl[-length(diffSpl)]
      splitsAll <- splitsAll[-length(splitsAll)]
    } else {
      if(diffSplCDF[indMin-1] < diffSplCDF[indMin + 1]){
        diffSplCDF[indMin-1] <- diffSplCDF[indMin-1] + diffSplCDF[indMin]
        splitsAll <- splitsAll[-(indMin-1)]
        diffSpl<- diff(splitsAll)
        diffSplCDF<- diffSplCDF[-(indMin)]
      } else {
        diffSplCDF[indMin+1] <- diffSplCDF[indMin+1] + diffSplCDF[indMin]
        splitsAll <- splitsAll[-(indMin)]
        diffSpl<- diff(splitsAll)
        diffSplCDF<- diffSplCDF[-(indMin)]
      }
    }
  }
  # einde Jolien
  splitsFin <- list(sort(splitsAll))
  names(splitsFin) <- varName
  transform2BinnedVar(dataDT, splitsFin)
  
  # plot(testData[selInds, x], predict(loessMod, newdata = testData[selInds, ]), xlim = c(-3500, 2000))
  # abline(v = splitsAll, col = 2)
  
  return(splitsFin)
}


createGroupedVar <- function(dataDT, varName, nGroupsFin, nGroups = 10, nMinLev=50, sampleSizeBin = 50000, nSamp = 10000, nTimes = 5, nIterMax = 1000, verboseConv = FALSE){
  # Check input parameters and certain variables in 'dataDT'.
  checkWholeNumb(list(nSamp, nTimes, nIterMax))
  checkLogicVec(list(verboseConv))
  checkLength(list(nSamp, nTimes, nIterMax, verboseConv), 1)
  checkDT(dataDT, c('transMN', 'inStateTimeTrans', varName))
  if(nGroupsFin > nGroups*(length(unique(dataDT$transMN)) - 1)){
    stop("The argument 'nGroupsFin' needs to be smaller than or equal to the 'nGroups' argument times the 1 minus the number of unique transitions.")
  }
  
  # The MN model is fitted with 'inStateTimeTrans' and a grouped version of the 'varName' variable,
  # where the factor is based on upon the sequential 2.5 percentiles
  # (as to have a nice trade-off between model complexity and sparseness).
  
  varNameGr <- paste0(varName, 'Grouped')
  # Note that we don't have a problem anymore with the binning when the quantiles are not unique.
  splitsVar <- unique(quantile(dataDT[, get(varName)], seq(.025, .975, .025), na.rm = TRUE)) #changed 040520
  splits <- list(splitsVar)
  names(splits)[1] <- varName
  transform2BinnedVar(dataDT, splits)
  medianVals <- dataDT[, median(get(varName), na.rm = TRUE), by = get(varNameGr)][order(get), ]
  setnames(medianVals, c('get', 'V1'), c(varNameGr, 'median'))
  medianVals <- medianVals[!is.na(median), ]
  
  form <- as.formula(paste0('transMN ~', varNameGr))
  modDelta <- fitMN(dataDT, form, varNameGr, rep(TRUE, length(varNameGr)), nSamp, nIterMax, verboseConv) #added
  
  # Getting predictions for all the possible values of the above created factor.
  # test <- data.table(inStateTimeTrans = rep(1, (length(splitsVar) + 1)), V1 = 1:(length(splitsVar) + 1))
  # setnames(test, 'V1', varNameGr)
  # asFactorDT(test, c('inStateTimeTrans', varNameGr))
  # predProbs <- predict(modDelta, newdata = test, type = "probs")
  
  modParams <- coef(modDelta)
  # modParamsDelta <- modParams[, grep('delt', colnames(modParams))]
  modParamsDelta <- modParams[, grep(varNameGr, colnames(modParams))]
  
  # For each of the transitions separately, the best splits are sought for.
  resBinning <- matrix(NA, nrow = nrow(modParamsDelta), ncol = nGroups - 1)
  
  for(transNr in 1:nrow(modParamsDelta)){
    
    dataLoess <- data.table(x = medianVals[-1, median], y = modParamsDelta[transNr, ])
    loessMod <- loess(y ~ x, data = dataLoess, span = 0.5)
    testData <- copy(dataDT)
    addFirstLast(testData, 'polNumb', F, T)
    testData <- testData[last == 1, ]
    setnames(testData, varName, 'x')
    yVals <- predict(loessMod, newdata = testData)
    testData[, y := yVals]
    resBinning[transNr, ] <- binningUnivSpline(testData, nGroups, min(nrow(testData), sampleSizeBin))
    
    # selInds <- sample(1:nrow(testData), 1000)
    # plot(testData[selInds, x], predict(loessMod, newdata = testData[selInds, ]), xlim = c(-3500, 2000))
    # abline(v = resBinning[transNr,], col = 2)
    
  }
  
  # Finding an unique set of splits (lower than the total sum of all generated splits), based upon the splits generated
  # for each separate transition, such that the remaining splits contain a balanced number of observations in each bin.
  # begin Jolien 
  splitsAll <- sort(unique(as.numeric(resBinning)))
  cdfSpl <- rep(NA, length(splitsAll))
  for(iSpl in 1:length(splitsAll)){
    cdfSpl[iSpl] <- sum(dataDT[, get(varName)] < splitsAll[iSpl], na.rm = TRUE)
  }
  
  diffSplCDF <- c(cdfSpl[1], cdfSpl[2:length(cdfSpl)] - cdfSpl[1:(length(cdfSpl) - 1)], sum(dataDT[, get(varName)] > splitsAll[length(splitsAll)], na.rm = TRUE))
  diffSpl <- diff(splitsAll)
  while(length(splitsAll) > (nGroupsFin - 1)){
    indMin <- which.min(diffSpl)
    
    if(diffSplCDF[indMin] < diffSplCDF[indMin + 2]){
      diffSplCDF[indMin] <- diffSplCDF[indMin] + diffSplCDF[indMin+1]
      splitsAll <- splitsAll[-(indMin)]
      diffSpl<- diff(splitsAll)
      diffSplCDF<- diffSplCDF[-(indMin)]
    } else {
      diffSplCDF[indMin+2] <- diffSplCDF[indMin+1] + diffSplCDF[indMin+2]
      splitsAll <- splitsAll[-(indMin+1)]
      diffSpl<- diff(splitsAll)
      diffSplCDF<- diffSplCDF[-(indMin+1)]
    }
    
  }
  
  while(any(diffSplCDF < nMinLev)){
    indMin <- which(diffSplCDF<nMinLev)[which.min(diffSplCDF[which(diffSplCDF<nMinLev)])]
    if(indMin == 1){
      diffSplCDF[2] <- diffSplCDF[1] + diffSplCDF[2]
      diffSpl <- diffSpl[-1]
      diffSplCDF <- diffSplCDF[-1]
      splitsAll <- splitsAll[-1]
    } else if(indMin == length(diffSplCDF)){
      diffSplCDF[length(diffSplCDF) - 1] <- diffSplCDF[length(diffSplCDF) - 1] + diffSplCDF[length(diffSplCDF)]
      diffSplCDF <- diffSplCDF[-length(diffSplCDF)]
      diffSpl <- diffSpl[-length(diffSpl)]
      splitsAll <- splitsAll[-length(splitsAll)]
    } else {
      if(diffSplCDF[indMin-1] < diffSplCDF[indMin + 1]){
        diffSplCDF[indMin-1] <- diffSplCDF[indMin-1] + diffSplCDF[indMin]
        splitsAll <- splitsAll[-(indMin-1)]
        diffSpl<- diff(splitsAll)
        diffSplCDF<- diffSplCDF[-(indMin)]
      } else {
        diffSplCDF[indMin+1] <- diffSplCDF[indMin+1] + diffSplCDF[indMin]
        splitsAll <- splitsAll[-(indMin)]
        diffSpl<- diff(splitsAll)
        diffSplCDF<- diffSplCDF[-(indMin)]
      }
    }
  }
  # einde Jolien
  splitsFin <- list(sort(splitsAll))
  names(splitsFin) <- varName
  transform2BinnedVar(dataDT, splitsFin)
  
  # plot(testData[selInds, x], predict(loessMod, newdata = testData[selInds, ]), xlim = c(-3500, 2000))
  # abline(v = splitsAll, col = 2)
  
  return(splitsFin)
}

simPaths <- function(timeMods, payMods, trainData, timeSplits, predData, fixedTimeMax, perLen, nSims, lastTrans, verbose){ #changed 080520
  # Check input parameters and certain variables in 'trainData' and 'predData'.
  if(!is.list(timeMods)) stop("Argument 'timeMods' is a list containing the different time model fits.")
  if(!is.list(payMods)) stop("Argument 'payMods' is a list containing the different payment model fits.")
  # checkEqualLength(list(timeMods, trainData, predData))
  # checkEqualLength(list(payMods, trainData, predData))
  llply(trainData, function(xx) checkDT(xx))
  llply(predData, function(xx) checkDT(xx))
  checkWholeNumb(list(nSims, lastTrans)) #changed 080520
  checkLogicVec(list(verbose))
  checkLength(list(nSims, verbose, lastTrans), 1) #changed 080520
  checkRanges(list(nSims, lastTrans), list(c('>', 0), c('>', 0))) #changed 080520
  
  nNonNs <- laply(predData, function(xx) ifelse(nrow(xx) > 0, nrow(xx[transMN != 'N', ]), 0)) #changed 150420
  if(sum(nNonNs) > 0) stop("The transMN column of at least one element of the 'predData' argument did not all just contain rows with value 'N'. Please correct for this.") #changed 150420
  nNonPs <- laply(predData, function(xx) ifelse(nrow(xx) > 0, nrow(xx[transType != 'P', ]), 0)) #changed 290420
  if(sum(nNonPs) > 0) stop("The transMN column of at least one element of the 'testDT' argument did not all just contain rows with value 'P'. Please correct for this.") #changed 150420
  
  if(verbose) print('Setting the stage')
  
  predSim <- list()
  nSim <- list()
  predTransAll <- list()
  nTrans <- length(timeMods)
  
  # Defining the initial nSim and predSim objects
  # nSim indicates how many sims still need to be taken for the polNumb of interest,
  # while predSim contains the data sets that were used for the timeMod and payMod.
  # both objects are a list, of length the number of transitions and they contain as
  # elements as many polNumbs as still need simulations.
  
  minTrans <- NA
  
  for(iTrans in 1:nTrans){
    
    if(verbose) print(iTrans)
    
    nSim[[iTrans]] <- as.list(rep(nSims, nrow(predData[[iTrans]]))) #a claimNr will just appear for the transition where it is still open (later on, not)
    predSim[[iTrans]] <- nSim[[iTrans]]
    predTransAll[[iTrans]] <- predData[[iTrans]]
    
    if(nrow(predData[[iTrans]]) > 0){
      
      if(is.na(minTrans)) minTrans <- iTrans
      
      for(iRun in 1:length(nSim[[iTrans]])){
        predSim[[iTrans]][[iRun]] <- predTransAll[[iTrans]][rep(iRun, nSim[[iTrans]][iRun]),]
        predSim[[iTrans]][[iRun]]$inStateTimeTrans <- as.numeric(as.character(predSim[[iTrans]][[iRun]]$inStateTimeTrans))
        predSim[[iTrans]][[iRun]]$inProcTimeTrans <- as.numeric(as.character(predSim[[iTrans]][[iRun]]$inProcTimeTrans))
        predSim[[iTrans]][[iRun]][, simPer := 0]
      }
      predTransAll[[iTrans]] <- predData[[iTrans]][rep(1:nrow(predData[[iTrans]]), nSims), ]
      predTransAll[[iTrans]] <- predTransAll[[iTrans]][order(polNumb), ]
      
      names(nSim[[iTrans]]) <- predData[[iTrans]]$polNumb
      names(predSim[[iTrans]]) <- predData[[iTrans]]$polNumb
      
    }
  }
  
  addDT <- data.table()
  
  if(!is.na(minTrans)){
    
    iTrans <- minTrans
    goOn <- TRUE
    
    if(verbose) print('Looping over the different transitions')
    
    while(goOn){
      
      if(verbose) cat('\n')
      if(verbose) print('////||||\\\\')
      if(verbose) print(paste('TRANSITION', iTrans))
      if(verbose) print('\\\\||||////')
      if(verbose) cat('\n')
      
      if(verbose) print(paste('nSimTot: ', sum(unlist(nSim[[iTrans]]))))
      
      #Prepping and augmenting the data for the simulations
      
      asNumericDT(trainData[[min(nTrans, iTrans)]], c('inStateTimeTrans', 'inProcTimeTrans'))
      #inStateTimeTransMax <- max(trainData[[min(nTrans, iTrans)]][, inStateTimeTrans]) #removed 150420
      #inProcTimeTransMax <- max(trainData[[min(nTrans, iTrans)]][, inProcTimeTrans]) #removed 150420
      
      inStateTimeTransMaxPay <- max(as.numeric(names(table(payMods[[min(nTrans, iTrans)]]$trainDT$inStateTimeTrans)))) #changed 150420
      inProcTimeTransMaxPay <- max(as.numeric(names(table(payMods[[min(nTrans, iTrans)]]$trainDT$inProcTimeTrans)))) #changed 150420
      
      inStateTimeTransMinPay <- min(as.numeric(names(table(payMods[[min(nTrans, iTrans)]]$trainDT$inStateTimeTrans)))) #changed 150420
      inProcTimeTransMinPay <- min(as.numeric(names(table(payMods[[min(nTrans, iTrans)]]$trainDT$inProcTimeTrans)))) #changed 150420
      
      inStateTimeTransMaxTime <- max(as.numeric(names(table(timeMods[[min(nTrans, iTrans)]]$trainDT$inStateTimeTrans)))) #changed 150420
      inProcTimeTransMaxTime <- max(as.numeric(names(table(timeMods[[min(nTrans, iTrans)]]$trainDT$inProcTimeTrans)))) #changed 150420
      
      inStateTimeTransMinTime <- min(as.numeric(names(table(timeMods[[min(nTrans, iTrans)]]$trainDT$inStateTimeTrans)))) #changed 150420
      inProcTimeTransMinTime <- min(as.numeric(names(table(timeMods[[min(nTrans, iTrans)]]$trainDT$inProcTimeTrans)))) #changed 150420
      
      inStateTimeTransMin <- max(inStateTimeTransMinTime, inStateTimeTransMinPay) #changed 150420
      inProcTimeTransMin <- max(inProcTimeTransMinTime, inProcTimeTransMinPay) #changed 150420
      
      inStateTimeTransMax <- min(inStateTimeTransMaxTime, inStateTimeTransMaxPay) #changed 150420
      inProcTimeTransMax <- min(inProcTimeTransMaxTime, inProcTimeTransMaxPay) #changed 150420
      
      if(nTrans >= iTrans){
        if(nrow(addDT) != 0){
          addDT[, inStateTimeMax := inStateTimeTransMax]
        }
        predTrans <- rbind(predTransAll[[iTrans]], addDT)
      } else {
        predTrans <- addDT
        #We need to reset the predSim data
        polNumbUniq <- unique(predTrans$polNumb)
        predSim[[iTrans]] <- list()
        
        for(iRun in 1:length(polNumbUniq)){
          predSim[[iTrans]][[iRun]] <- predTrans[polNumb == polNumbUniq[iRun],]
        }
        names(predSim[[iTrans]]) <- polNumbUniq
      }
      
      predTransL <- split(predTrans, by = 'polNumb')
      inStateTimeMax <- min(trainData[[min(nTrans, iTrans)]][1, inStateTimeMax], fixedTimeMax)
      
      if(verbose) print(paste('Number of open claims of this transition:', nrow(predTrans)))
      
      nSimTrans <- nSim[[iTrans]]
      addPer <- 0
      stillOpen <- TRUE
      
      # looping until all claims have their simulated transition
      
      while(stillOpen){
        
        ptm <- proc.time()
        addPer <- pmin(addPer + 1, inStateTimeMax)
        if(verbose) print(paste('Run', addPer))
        
        selPolNumbs <- names(unlist(nSimTrans))[unlist(nSimTrans) != 0]
        
        predTransDT <- rbindlist(predTransL)
        runData <- predTransDT[polNumb %in% selPolNumbs, ][transMN == 'N', ]
        
        if(addPer == 1){ #changed 050520
          asNumericDT(runData, c('inStateTimeTrans', 'inProcTimeTrans')) #changed 050520
          runData[inStateTimeTrans > inStateTimeTransMax, inStateTimeTrans := inStateTimeTransMax] #changed 050520
          runData[inStateTimeTrans < inStateTimeTransMin, inStateTimeTrans := inStateTimeTransMin] #changed 050520
          runData[inProcTimeTrans > inProcTimeTransMax, inProcTimeTrans := inProcTimeTransMax] #changed 050520
          runData[inProcTimeTrans < inProcTimeTransMin, inProcTimeTrans := inProcTimeTransMin] #changed 050520
          asFactorDT(runData, c('inStateTimeTrans', 'inProcTimeTrans')) #changed 050520
        } #changed 050520
        
        predTransL <- split(runData, by = 'polNumb')
        
        asFactorDT(runData, c('inStateTimeTrans', 'inProcTimeTrans'))
        if(sum(names(runData) == 'delt1PayTimeTrans') != 0) asFactorDT(runData, c('delt1PayTimeTrans'))
        if(sum(names(runData) == 'delt2PayTimeTrans') != 0) asFactorDT(runData, c('delt2PayTimeTrans'))
        
        if(nrow(runData) > 1){
          predProb <- predict(timeMods[[min(length(timeMods), min(nTrans, iTrans))]]$fitTime, newdata = runData, type = 'probs')
        } else {
          predProb <- t(as.matrix(predict(timeMods[[min(length(timeMods), min(nTrans, iTrans))]]$fitTime, newdata = runData, type = 'probs')))
        }
        
        if(ncol(predProb) < 4){
          
          fixedColNames <- c('N', 'P', 'TN', 'TP')
          missingCol <- !(fixedColNames %in% colnames(predProb))
          
          fixedColNames[missingCol]
          for(iLoop in 1:sum(missingCol)){
            predProb <- cbind(predProb, rep(0, nrow(predProb)))
            colnames(predProb)[ncol(predProb)] <- fixedColNames[missingCol][iLoop]
          }
          predProb <- predProb[, order(colnames(predProb), fixedColNames)]
        }
        
        if(verbose) print(paste0('The expected number of lines after this run: ', floor(mean(predProb[,1])*nrow(runData))))
        
        if(iTrans > 1){
          pastedPredProb <- aaply(predProb, 1, function(xx) paste(xx, collapse = ''))
        }
        
        # No more non terminal payments when lastTrans is reached #changed 080520
        if(iTrans >= lastTrans){ #changed 080520
          predProb[, 4] <- predProb[, 4] + predProb[, 2] #changed 080520
          predProb[, 2] <- 0 #changed 080520
        } #changed 080520
        # No more staying in the state without a transition when inStateTimeMax is reached #changed 080520
        if(addPer >= inStateTimeMax & iTrans < lastTrans){ #changed 080520
          predProb[, 2] <- predProb[, 2] + predProb[, 1]/3 #changed 080520
          predProb[, 3] <- predProb[, 3] + predProb[, 1]/3 #changed 080520
          predProb[, 4] <- predProb[, 4] + predProb[, 1]/3 #changed 080520
          predProb[, 1] <- 0 #changed 080520
        } else if(addPer >= inStateTimeMax & iTrans >= lastTrans){ #changed 080520
          predProb[, 3] <- predProb[, 3] + predProb[, 1]/2 #changed 080520
          predProb[, 4] <- predProb[, 4] + predProb[, 1]/2 #changed 080520
          predProb[, 1] <- 0 #changed 080520
        }
        if(addPer >= inStateTimeMax){ #removed 080520
          for(iRow in 1:nrow(predProb)){ #removed 080520
            if(sum(predProb[iRow, ][2:4]) < 0.05){ #removed 080520
              predProb[iRow, 2:4] <- 1/3 #removed 080520
              predProb[iRow, 1] <- 0 #removed 080520
            } #removed 080520
          } #removed 080520
        } #removed 080520
        
        runData$inStateTimeTrans <- as.numeric(as.character(runData$inStateTimeTrans))
        runData$inProcTimeTrans <- as.numeric(as.character(runData$inProcTimeTrans))
        
        simRun <- as.list(rep(1, nrow(runData)))
        
        # The actual simulation of the transition
        for(iRun in 1:length(selPolNumbs)){
          if(iRun %% 1000 == 0) print(iRun)
          indNPol <- which(predTransL[[which(names(predTransL) == selPolNumbs[iRun])]][, transMN] == 'N')
          sel <- which(runData$polNumb == selPolNumbs[iRun])
          uPredProb <- predProb[sel, ]
          if(length(sel) > 1){
            uPredProb <- unique(uPredProb)
          }
          
          if((sum(is.null(nrow(uPredProb))) + sum(nrow(uPredProb) == 1)) >= 1){ #hence one row only
            simRun[[iRun]] <- rmultinom(1, as.numeric(nSimTrans[selPolNumbs[iRun]]), prob = uPredProb)# distribuer les nsimsTrans[selPolNumbs[iRun]] tranjectoires restantes pour ce polNumb 
            rownames(simRun[[iRun]]) <- colnames(predProb)
            
            nSimTrans[selPolNumbs[iRun]] <- simRun[[iRun]]['N', ]# ne garder dans nSimTrans que les lignes quii restent i.e. transMN == N 
            
            rowInds <- cumsum(simRun[[iRun]])
            x <- copy(predSim[[iTrans]][selPolNumbs[iRun]][[1]])
            
            indsN <- which(x$transMN == 'N')
            if(simRun[[iRun]][2] != 0) x[indsN, ] <- x[indsN, ][(rowInds[1] + 1):rowInds[2], transMN := 'P']
            if(simRun[[iRun]][3] != 0) x[indsN, ] <- x[indsN, ][(rowInds[2] + 1):rowInds[3], transMN := 'TN']
            if(simRun[[iRun]][4] != 0) x[indsN, ] <- x[indsN, ][(rowInds[3] + 1):rowInds[4], transMN := 'TP']
            
            if(sum(simRun[[iRun]][2:4]) != 0){
              if(sum(names(x) == 'simPer') == 0){
                x[, simPer := NA]
                asNumericDT(x, 'simPer')
              }
              x[indsN, ] <- x[indsN, ][(rowInds[1] + 1):rowInds[4], simPer := addPer]
              x[indsN, ] <- x[indsN, ][(rowInds[1] + 1):rowInds[4], inStateTimeTrans := max(c(min(c(inStateTimeTrans + addPer - 1, inStateTimeTransMax)), inStateTimeTransMin))] #changed 150420
              x[indsN, ] <- x[indsN, ][(rowInds[1] + 1):rowInds[4], inProcTimeTrans := max(c(min(c(inProcTimeTrans + addPer - 1, inProcTimeTransMax)), inProcTimeTransMin))] #changed 150420
              
            }
            
            predSim[[iTrans]][selPolNumbs[iRun]][[1]] <- copy(x)
            predTransL[[which(names(predTransL) == selPolNumbs[iRun])]][indNPol, transMN := x[indsN, transMN]]
            
          } else {
            
            pastedPredProbTemp <- pastedPredProb[which(runData$polNumb == selPolNumbs[iRun])]
            x <- copy(predSim[[iTrans]][selPolNumbs[iRun]][[1]])
            indsN <- which(x$transMN == 'N')
            
            pastedPredProbTempUniq <- unique(pastedPredProbTemp)
            nUniq <- length(pastedPredProbTempUniq)
            predProbTemp <- predProb[which(runData$polNumb == selPolNumbs[iRun]),]
            
            for(iUniq in 1:nUniq){
              uniqRows <- which(pastedPredProbTemp == pastedPredProbTempUniq[iUniq])
              simRunTemp <- rmultinom(1, length(uniqRows), prob = predProbTemp[uniqRows[1], ])
              x <- x[indsN[uniqRows], transMN := rep(rownames(simRunTemp), simRunTemp)]
            }
            
            predSim[[iTrans]][selPolNumbs[iRun]][[1]] <- copy(x)
            nSimTrans[selPolNumbs[iRun]] <- nrow(predSim[[iTrans]][selPolNumbs[iRun]][[1]][transMN == 'N', ])
            
            predSim[[iTrans]][selPolNumbs[iRun]][[1]][indsN, simPer := addPer]
            predSim[[iTrans]][selPolNumbs[iRun]][[1]][indsN, inStateTimeTrans := max(c(min(c(inStateTimeTrans + addPer - 1, inStateTimeTransMax)), inStateTimeTransMin))] #changed 150420
            predSim[[iTrans]][selPolNumbs[iRun]][[1]][indsN, inProcTimeTrans := max(c(min(c(inProcTimeTrans + addPer - 1, inProcTimeTransMax)), inProcTimeTransMin))] #changed 150420
            
            simRun[[iRun]] <- simRunTemp
            predTransL[[which(names(predTransL) == selPolNumbs[iRun])]][, transMN := x[indsN, transMN]]
            
          }
        }
        
        # Updating the values of the time vars that are in the time model
        
        predTransDT <- rbindlist(predTransL)
        
        predTransDT$inStateTimeTrans <- as.numeric(as.character(predTransDT$inStateTimeTrans))
        predTransDT$inProcTimeTrans <- as.numeric(as.character(predTransDT$inProcTimeTrans))
        predTransDT[, inStateTimeTrans := max(c(min(c(inStateTimeTrans + 1, inStateTimeTransMax)), inStateTimeTransMin))] #changed 150420
        predTransDT[, inProcTimeTrans := max(c(min(c(inProcTimeTrans + 1, inProcTimeTransMax)), inProcTimeTransMin))] #changed 150420
        
        asFactorDT(predTransDT, c('inStateTimeTrans', 'inProcTimeTrans'))
        
        predTransL <- split(predTransDT, by = 'polNumb')
        
        if(verbose) print(paste('Number of polNumbs with sims:', sum(unlist(nSimTrans) != 0)))
        if(verbose) print(paste('Number of remaining sims:', sum(unlist(nSimTrans[selPolNumbs]))))
        if(verbose) print(paste('Time in secs of this run:', round(as.numeric((proc.time() - ptm)[3]), digits = 3)))
        if(sum(unlist(nSimTrans) != 0) == 0) stillOpen <- FALSE
      }
      
      if(verbose) print('Simulating payments for those transitions that require a payment')
      
      iPol <- names(predSim[[iTrans]])[1]
      count <- 1
      
      for(iPol in names(predSim[[iTrans]])[count:length(predSim[[iTrans]])]){
        
        if(count %% 1000 == 0) print(count)
        
        payInds <- which(predSim[[iTrans]][iPol][[1]]$transMN %in% c('TP', 'P'))
        noPayInds <- setdiff(1:nrow(predSim[[iTrans]][iPol][[1]]), payInds)
        predSim[[iTrans]][iPol][[1]][noPayInds, outComeT := NA]
        
        #selecting the ones that still need a payment simulation with nSimTrans or so
        
        # x[indsN, ][(rowInds[1] + 1):rowInds[4], inStateTimeTrans := max(c(min(c(inStateTimeTrans + addPer - 1, inStateTimeTransMax)), inStateTimeTransMin))] #removed 210420
        # predSim[[iTrans]][iPol][[1]][, inStateTimeTrans := max(c(min(c(inStateTimeTrans + addPer - 1, inStateTimeTransMax)), inStateTimeTransMin))] #removed 210420
        
        if(length(payInds) != 0){
          sampDT <- predSim[[iTrans]][iPol][[1]][payInds, ]
          asFactorDT(sampDT, names(sampDT)[names(sampDT) %in% c('inStateTimeTrans', 'inProcTimeTrans', 'delt1PayTimeTrans', 'delt2PayTimeTrans')]) #added
          
          samps <- rSplicingNP_fast(sampDT, payMods, min(nTrans, iTrans))
          predSim[[iTrans]][iPol][[1]][payInds, outComeT := samps]
        }
        
        count <- count + 1
        
      }
      
      # Basically, the object 'addDT' is created which will be added to the polnumbs
      # that still need simulations for the next transition
      
      if(verbose) print('Adding non terminal (P) simulations to the next transition')
      
      addDT <- data.table()
      
      if(length(nSim) < (iTrans + 1)){
        nSim[[iTrans + 1]] <- list()
      }
      if(length(predSim) < (iTrans + 1)){
        predSim[[iTrans + 1]] <- list()
      }
      
      inProcTimeTransPlus1Max <- max(as.numeric(trainData[[min(nTrans, iTrans + 1)]][, inProcTimeTrans]))
      
      colNamesFitPlus1 <- colnames(coef(timeMods[[min(length(timeMods), min(nTrans, iTrans + 1))]]$fitTime))
      inProcTimeTransPlus1Min <- min(as.numeric(str_replace_all(colNamesFitPlus1[grep('inProcTimeTrans', colNamesFitPlus1)], "[a-zA-Z\\s]", ""))) - 1
      if(inProcTimeTransPlus1Min == 2) inProcTimeTransPlus1Min <- 1
      if(sum(names(trainData[[min(nTrans, iTrans + 1)]]) == 'delt1PayTimeTrans') == 1){
        delt1PayTimeTransPlus1Max <- max(as.numeric(as.character(trainData[[min(nTrans, iTrans + 1)]][, delt1PayTimeTrans])))
        delt1PayTimeTransPlus1Min <- min(as.numeric(as.character(trainData[[min(nTrans, iTrans + 1)]][, delt1PayTimeTrans])))
      }
      if(sum(names(trainData[[min(nTrans, iTrans + 1)]]) == 'delt2PayTimeTrans') == 1){
        delt2PayTimeTransPlus1Max <- max(as.numeric(as.character(trainData[[min(nTrans, iTrans + 1)]][, delt2PayTimeTrans])))
        delt2PayTimeTransPlus1Min <- min(as.numeric(as.character(trainData[[min(nTrans, iTrans + 1)]][, delt2PayTimeTrans])))
      }
      if(sum(names(trainData[[min(nTrans, iTrans + 1)]]) == 'deltRepTrans') == 1){
        deltRepTransPlus1Max <- max(as.numeric(as.character(trainData[[min(nTrans, iTrans + 1)]][, deltRepTrans])))
        deltRepTransPlus1Min <- min(as.numeric(as.character(trainData[[min(nTrans, iTrans + 1)]][, deltRepTrans])))
      }
      
      count <- 1
      
      iPol <- names(predSim[[iTrans]])[1]
      
      for(iPol in names(predSim[[iTrans]])){
        
        if(count %% 1000 == 0) print(count)
        
        nOpen <- sum(predSim[[iTrans]][iPol][[1]]$transMN == 'P')
        
        if(nOpen > 0){
          
          namesDT <- names(trainData[[min(nTrans, iTrans + 1)]])
          cumDeltNames <- namesDT[grep('cumDelt', namesDT)]
          namesDT <- namesDT[!namesDT %in% cumDeltNames]
          deltRepNames <- namesDT[grep('deltRep', namesDT)]
          namesDT <- namesDT[!namesDT %in% deltRepNames]
          deltPayNames <- namesDT[grep('delt', namesDT)]
          namesDT <- namesDT[!namesDT %in% deltPayNames]
          namesDT <- c(namesDT, deltRepNames)
          
          predSim[[iTrans + 1]][iPol][[1]] <- predSim[[iTrans]][iPol][[1]][transMN == 'P', ]
          predSim[[iTrans + 1]][iPol][[1]][, bookDate := bookDate + simPer*perLen]
          predSim[[iTrans + 1]][iPol][[1]][, finYear := year(bookDate)]
          
          if(sum(cumDeltNames == 'cumDelt2Pay') != 0){
            
            predSim[[iTrans + 1]][iPol][[1]][, cumDelt2Pay := cumDelt1Pay]
            
            selEl <- which(laply(timeSplits[[min(nTrans, iTrans + 1)]], function(xx) grepl('cumDelt2Pay', names(xx)[1])))
            splits <- list(timeSplits[[min(nTrans, iTrans + 1)]][[selEl]])
            names(splits)[1] <- 'cumDelt2Pay'
            transform2BinnedVar(predSim[[iTrans + 1]][iPol][[1]], splits)
          }
          
          if(sum(cumDeltNames == 'cumDelt1Pay') != 0){
            if(sum(names(predSim[[iTrans + 1]][iPol][[1]]) == 'cumDelt1Pay') == 1){
              predSim[[iTrans + 1]][iPol][[1]][, cumDelt1Pay := outComeT + cumDelt1Pay]
            } else {
              predSim[[iTrans + 1]][iPol][[1]][, cumDelt1Pay := outComeT]
            }
            
            selEl <- which(laply(timeSplits[[min(nTrans, iTrans + 1)]], function(xx) grepl('cumDelt1Pay', names(xx)[1])))
            splits <- list(timeSplits[[min(nTrans, iTrans + 1)]][[selEl]])
            names(splits)[1] <- 'cumDelt1Pay'
            transform2BinnedVar(predSim[[iTrans + 1]][iPol][[1]], splits)
          }
          
          if(sum(deltPayNames == 'delt1Pay') != 0){ #changed 040520
            predSim[[iTrans + 1]][iPol][[1]][, delt1Pay := outComeT] #changed 040520
            selEl <- which(laply(timeSplits[[min(nTrans, iTrans + 1)]], function(xx) grepl('delt1Pay', names(xx)[1]))) #changed 040520
            if(length(selEl) != 0){ #changed 180520
              splits <- list(timeSplits[[min(nTrans, iTrans + 1)]][[selEl]]) #changed 040520
              names(splits)[1] <- 'delt1Pay' #changed 040520
              transform2BinnedVar(predSim[[iTrans + 1]][iPol][[1]], splits) #changed 040520
            } #changed 180520
          } #changed 040520
          
          if(sum(deltPayNames == 'delt2Pay') != 0){ #changed 040520
            predSim[[iTrans + 1]][iPol][[1]][, delt2Pay := outComeT] #changed 040520
            selEl <- which(laply(timeSplits[[min(nTrans, iTrans + 1)]], function(xx) grepl('delt2Pay', names(xx)[1]))) #changed 040520
            if(length(selEl) != 0){ #changed 180520
              splits <- list(timeSplits[[min(nTrans, iTrans + 1)]][[selEl]]) #changed 040520
              names(splits)[1] <- 'delt2Pay' #changed 040520
              transform2BinnedVar(predSim[[iTrans + 1]][iPol][[1]], splits) #changed 040520
            } #changed 180520
          } #changed 040520
          
          if(sum(deltPayNames == 'delt1PayTimeTrans') == 1){
            predSim[[iTrans + 1]][iPol][[1]][, delt1PayTimeTrans := inStateTimeTrans]
          }
          if(sum(deltPayNames == 'delt2PayTimeTrans') == 1){
            predSim[[iTrans + 1]][iPol][[1]][, delt2PayTimeTrans := delt1PayTimeTrans]
          }
          
          predSim[[iTrans + 1]][iPol][[1]][, inProcTimeTrans := min(max(c(inProcTimeTrans, simPer)), inProcTimeTransPlus1Max)]
          predSim[[iTrans + 1]][iPol][[1]][, inProcTimeTrans := max(inProcTimeTrans, inProcTimeTransPlus1Min)]
          if(sum(deltPayNames == 'delt1PayTimeTrans') == 1){
            predSim[[iTrans + 1]][iPol][[1]][, delt1PayTimeTrans := min(delt1PayTimeTrans, delt1PayTimeTransPlus1Max)]
            predSim[[iTrans + 1]][iPol][[1]][, delt1PayTimeTrans := max(delt1PayTimeTrans, delt1PayTimeTransPlus1Min)]
          }
          if(sum(deltPayNames == 'delt2PayTimeTrans') == 1){
            predSim[[iTrans + 1]][iPol][[1]][, delt2PayTimeTrans := min(delt2PayTimeTrans, delt2PayTimeTransPlus1Max)]
            predSim[[iTrans + 1]][iPol][[1]][, delt2PayTimeTrans := max(delt2PayTimeTrans, delt2PayTimeTransPlus1Min)]
          }
          if(sum(deltRepNames == 'deltRepTrans') == 1){
            asNumericDT(predSim[[iTrans + 1]][iPol][[1]], 'deltRepTrans')
            predSim[[iTrans + 1]][iPol][[1]][, deltRepTrans := min(deltRepTrans, deltRepTransPlus1Max)]
            predSim[[iTrans + 1]][iPol][[1]][, deltRepTrans := max(deltRepTrans, deltRepTransPlus1Min)]
            asFactorDT(predSim[[iTrans + 1]][iPol][[1]], 'deltRepTrans')
          }
          
          predSim[[iTrans + 1]][iPol][[1]][, simPer := NULL]
          predSim[[iTrans + 1]][iPol][[1]]$inStateTimeTrans <- 1
          predSim[[iTrans + 1]][iPol][[1]]$transMN <- 'N'
          predSim[[iTrans + 1]][iPol][[1]]$outComeT <- NA
          
          if(nrow(addDT) == 0){
            addDT <- predSim[[iTrans + 1]][iPol][[1]]
          } else {
            addDT <- rbind(addDT, predSim[[iTrans + 1]][iPol][[1]])
          }
          nSim[[iTrans + 1]][iPol] <- nOpen
        }
        
        count <- count + 1
      }
      
      if(nrow(addDT) == 0){
        goOn <- FALSE
      } else {
        iTrans <- iTrans + 1
      }
    }
    return(list(predSim = predSim, nSim = nSim))
  }
}

concProbCR <- function(testDT, mods, selTime, nSample = NULL, overallSurv = TRUE, verbose = TRUE){
  # Check input parameters and certain variables in 'testDT'.
  checkDT(testDT, c('polNumb', 'inStateTimeTrans', 'transMN'))
  if(!is.null(nSample)){
    checkWholeNumb(list(nSample))
  } else{
    nSample <- nrow(testDT)
  }
  
  checkDT(testDT, c('inStateTimeTrans', 'transMN'))
  checkLength(list(nSample, selTime, overallSurv, verbose), rep(1, 4))
  checkWholeNumb(list(selTime))
  checkLogicVec(list(verbose, overallSurv))
  
  if(verbose) print('Prepping the data')
  
  #Getting the predictions out of the testDT (only the line where the actual transition takes place matters)
  addFirstLast(testDT, 'polNumb', F, T)
  testDT <- testDT[last == 1, ][, last := NULL]
  if(sum(testDT$transMN == 'N') > 0) print("No censorings are allowed in the argument 'testDT'.")
  testDTSamp <- testDT[sample(1:nrow(testDT), nSample), ]
  nTimePoints <- length(unique(testDTSamp$inStateTimeTrans))
  predProbs <- predict(mods, newdata = testDTSamp, type = "probs")
  
  if(verbose) print('Extracting the predicted CIFs')
  
  #Computing the cumulative incidence functions for the different transition types
  selTrans <- 'P'
  cif_P <- matrix(NA, nrow = nSample, ncol = nTimePoints)
  cif_P[, 1] <- predProbs[, colnames(predProbs) == selTrans]
  for(iStep in 2:nTimePoints){
    cif_P[, iStep] <- aaply(predProbs[, 1], 1, function(xx) prod(rep(xx, iStep)))*predProbs[, colnames(predProbs) == selTrans]
  }
  selTrans <- 'TP'
  cif_TP <- matrix(NA, nrow = nSample, ncol = nTimePoints)
  cif_TP[, 1] <- predProbs[, colnames(predProbs) == selTrans]
  for(iStep in 2:nTimePoints){
    cif_TP[, iStep] <- aaply(predProbs[, 1], 1, function(xx) prod(rep(xx, iStep)))*predProbs[, colnames(predProbs) == selTrans]
  }
  selTrans <- 'TN'
  cif_TN <- matrix(NA, nrow = nSample, ncol = nTimePoints)
  cif_TN[, 1] <- predProbs[, colnames(predProbs) == selTrans]
  for(iStep in 2:nTimePoints){
    cif_TN[, iStep] <- aaply(predProbs[, 1], 1, function(xx) prod(rep(xx, iStep)))*predProbs[, colnames(predProbs) == selTrans]
  }
  
  cif <- matrix(NA, nrow = nSample, ncol = nTimePoints)
  predDT <- copy(testDTSamp[, .(inStateTimeTrans, transMN)])
  asNumericDT(predDT, c('inStateTimeTrans'))
  results <- list()
  
  #Computing the within and between concProb
  for(cTransI in c('P', 'TP', 'TN')){
    for(cTransJ in c('P', 'TP', 'TN')){
      if(verbose) print(paste(paste(paste0('Going for concProb ', cTransI), '-'), cTransJ))
      if(overallSurv){
        cif <- 1 - aaply(cif_P + cif_TP + cif_TN, 1, function(xx) cumsum(xx))
      } else {
        if(cTransI == 'P'){
          cif <- cif_P
        } else if(cTransI == 'TP'){
          cif <- cif_TP
        } else if(cTransI == 'TN'){
          cif <- cif_TN
        }
      }
      predDT[, pred := cif[, selTime]]
      results[[cTransI]][[cTransJ]] <- concProbCalcCR(predDT, selTime, cTransI, cTransJ, FALSE, verbose)
    }
  }
  
  #Computing the overall concProb
  if(overallSurv){
    cif <- 1 - aaply(cif_P + cif_TP + cif_TN, 1, function(xx) cumsum(xx))
    predDT[, pred := cif[, selTime]]
    for(cTransI in c('P', 'TP', 'TN')){
      if(verbose) print(paste('Going for concProb O -', cTransI))
      results[['O']][[cTransI]] <- concProbCalcCR(predDT, selTime, cTransI, NULL, TRUE, verbose)
    }
  } else {
    results$O <- concProbCalcCR(predDT, selTime, NULL, NULL, TRUE, verbose)
    for(cTransI in c('P', 'TP', 'TN')){
      if(verbose) print(paste('Going for concProb O -', cTransI))
      if(cTransI == 'P'){
        cif <- cif_P
      } else if(cTransI == 'TP'){
        cif <- cif_TP
      } else if(cTransI == 'TN'){
        cif <- cif_TN
      }
      predDT[, pred := cif[, selTime]]
      results[['O']][[cTransI]] <- concProbCalcCR(predDT, selTime, cTransI, NULL, TRUE, verbose)
    }
  }
  
  if(verbose) print('Going for concProb O - O')
  diffTrans <- c('P', 'TP', 'TN')
  nCompTot <- 0
  concProbOverall <- 0
  prevalenceMat <- matrix(NA, nrow = 3, ncol = 3, dimnames = list(rownames = diffTrans, colnames = diffTrans))
  for(iRun in 1:3){
    for(jRun in 1:3){
      tempNComp <- results[[diffTrans[iRun]]][[diffTrans[jRun]]]$nComp
      tempConcProb <- results[[diffTrans[iRun]]][[diffTrans[jRun]]]$concProb
      if(!is.null(tempNComp) & !is.null(tempConcProb)){
        concProbOverall <- concProbOverall + tempConcProb*tempNComp
        nCompTot <- nCompTot + tempNComp
        prevalenceMat[iRun, jRun] <- tempNComp
      }
    }
  }
  results[['O']][['O']]$concProb <- concProbOverall/nCompTot
  prevalenceMat <- round(prevalenceMat/nCompTot, digits = 3)
  
  #Alternative to the above nested for-loop (exactly the same result)
  # results[['O']][['O']] <- concProbCalcCR(predDT, selTime, NULL, NULL, TRUE, verbose)
  
  diffTrans <- c('P', 'TP', 'TN', 'O')
  # resultCP <- matrix(NA, nrow = 3, ncol = 4, dimnames = list(rownames = diffTrans[-4], colnames = diffTrans))
  resultCP <- matrix(NA, nrow = 4, ncol = 4, dimnames = list(rownames = diffTrans, colnames = diffTrans))
  
  for(iRun in 1:3){
    for(jRun in 1:3){
      tempRes <- results[[diffTrans[iRun]]][[diffTrans[jRun]]]$concProb
      if(!is.null(tempRes)) resultCP[iRun, jRun] <- tempRes
    }
  }
  for(iRun in 1:4){
    tempRes <- resultCP[iRun, 4] <- results[['O']][[diffTrans[iRun]]]$concProb
    if(!is.null(tempRes)) resultCP[iRun, 4] <- tempRes
  }
  return(list(resAll = results, CP = resultCP, prevalence = prevalenceMat))
}

concProbCalcCR <- function(predDT, selTime, cTransI, cTransJ, overall = TRUE, verbose = TRUE){
  # Check input parameters and certain variables in 'predDT'.
  checkDT(predDT, c('inStateTimeTrans', 'transMN', 'pred'))
  checkLength(list(selTime, overall, verbose), rep(1, 3))
  if(!is.null(cTransI)){
    checkValues(list(cTransI), list(c('P', 'TP', 'TN')))
    checkLength(list(cTransI), 1)
    checkCharVec(list(c(cTransI)))
  }
  if(!is.null(cTransJ)){
    checkValues(list(cTransJ), list(c('P', 'TP', 'TN')))
    checkLength(list(cTransJ), 1)
    checkCharVec(list(c(cTransJ)))
  }
  if((is.null(cTransI) | is.null(cTransJ)) & !overall){
    print("The arguments 'cTransI' and 'cTransI' can't be equal to NULL when argument 'overall' is FALSE." )
  }
  if(is.null(cTransI) & !is.null(cTransJ) & overall){
    print("The argument 'cTransI' can't be NULL when argument 'cTransJ' is not NULL and when argument 'overall' is TRUE." )
  }
  checkWholeNumb(list(selTime))
  checkLogicVec(list(verbose, overall))
  
  nComp <- 0
  nConc <- 0
  
  if(overall){
    if(is.null(cTransI) & is.null(cTransJ)){
      for(i in 1:(nrow(predDT))){
        if(verbose & i %% 1000 == 0) print(paste0('>>> Processing row ', i))
        if(predDT[i, inStateTimeTrans] < selTime){
          nComp <- nComp + nrow(predDT[-i, ][predDT[i, inStateTimeTrans] < inStateTimeTrans])
          nConc <- nConc + nrow(predDT[-i, ][predDT[i, inStateTimeTrans] < inStateTimeTrans][predDT[i, pred] < pred, ])
        }
      }
    } else if(!is.null(cTransI) & is.null(cTransJ)){
      for(i in 1:(nrow(predDT))){
        if(verbose & i %% 1000 == 0) print(paste0('>>> Processing row ', i))
        if((predDT[i, transMN] == cTransI) & predDT[i, inStateTimeTrans] < selTime){
          nComp <- nComp + nrow(predDT[-i, ][predDT[i, inStateTimeTrans] < inStateTimeTrans])
          nConc <- nConc + nrow(predDT[-i, ][predDT[i, inStateTimeTrans] < inStateTimeTrans][predDT[i, pred] < pred, ])
        }
      }
    }
  } else {
    for(i in 1:(nrow(predDT))){
      if(verbose & i %% 1000 == 0) print(paste0('>>> Processing row ', i))
      if((predDT[i, transMN] == cTransI) & predDT[i, inStateTimeTrans] < selTime){
        nComp <- nComp + nrow(predDT[-i, ][predDT[i, inStateTimeTrans] < inStateTimeTrans & transMN == cTransJ])
        nConc <- nConc + nrow(predDT[-i, ][predDT[i, inStateTimeTrans] < inStateTimeTrans & transMN == cTransJ][predDT[i, pred] < pred, ])
      }
    }
  }
  
  return(list(concProb = nConc/nComp, nConc = nConc, nComp = nComp))
  
}

payFit <- function(inputDT, quants, nMinMod, nMinLev, nTimesParams, nMaxNoMod, intVars = NULL, nSamp = 50000, nIterMax = 1000, verbose = TRUE, verboseConv = FALSE, RF = FALSE){ #changed 150420
  # Check input parameters and certain variables in 'inputDT'.
  if(!is.list(inputDT)) stop("The 'inputDT' argument needs to be a list of data.tables objects.")
  llply(inputDT, function(xx) checkDT(xx, c('transMN', 'outComeT')))
  checkLength(list(nMinMod, nMinLev, nTimesParams, nMaxNoMod), c(1, 1, 1, 1))
  checkLength(list(verbose, nSamp, nIterMax, verboseConv), c(1, 1, 1, 1))
  if(!is.null(intVars)) checkCharVec(list(intVars))
  
  checkWholeNumb(list(nSamp, nIterMax))
  checkWholeNumb(list(nMinMod, nMinLev, nTimesParams, nMaxNoMod))
  checkNumVec(list(quants))
  checkLogicVec(list(verbose, verboseConv))
  checkRanges(list(nSamp, nIterMax), list(c('>', 0), c('>', 0)))
  checkRanges(list(nMinMod, nMinLev, nTimesParams, nMaxNoMod), list(c('>', 0), c('>', 0), c('>', 0), c('>', 0)))
  alply(quants, 1, function(xx) checkRanges(list(xx), list( c('>=', 0, '<=', 1))))
  
  iPay <- 1
  
  meanVals <- list()
  result <- list()
  
  for(iPay in 1:length(inputDT)){
    
    if(verbose) print(paste0('>>> Fitting payment distr ', iPay))
    if(verbose) print('>>> Prep + fitting GPD phase')
    
    result[[iPay]] <- list()
    
    payData <- copy(inputDT[[iPay]][transMN %in% c('P', 'TP'), ])
    if(iPay == 1){
      payData <- copy(payData[outComeT > 0, ])
    }
    # table(payData$transMN)
    removeEmptyLevelsDT(payData)
    asNumericDT(payData, 'outComeT')
    
    splitOn <- TRUE
    nextJump <- 5
    lastTry <- FALSE
    
    currentInd  <- 0
    threshold <- quantile(payData[, outComeT], quants[length(quants)])
    dataGPD <- payData[outComeT > threshold, outComeT]
    if(length(dataGPD) <= nMaxNoMod){
      stop("Please select a lower value for the highest value of the 'quant' argument. There are too little observations (as dictated by the 'nMaxNoMod' argument) to fit a GPD distribution.")
    }
    valsGPD <- sort(dataGPD)
    
    while(splitOn){
      tempFit <- try(suppressWarnings(ismev::gpd.fit(xdat = dataGPD, threshold = threshold, show = FALSE)), silent = TRUE)
      if(class(tempFit) != "try-error"){
        splitOn <- FALSE
      } else if(lastTry){
        splitOn <- FALSE
        stop('No GPD distribution could be fitted. Please check if there is no error in your data.')
      } else {
        dataGPD <- payData[outComeT > threshold, ]
        currentInd <- currentInd + nextJump
        threshold <- valsGPD[currentInd]
        dataGPD <- payData[outComeT > threshold, outComeT]
        if(length(dataGPD) <= nMaxNoMod){
          lastTry <- TRUE
        }
      }
    }
    
    result[[iPay]]$fitGPD <- tempFit
    
    splittingPoints <- list()
    splittingPoints[[1]] <- unique(quantile(payData$outComeT, quants))
    splittingPoints[[1]][length(splittingPoints[[1]])] <- threshold
    
    result[[iPay]]$splits <- splittingPoints
    nSplits <- length(splittingPoints[[1]])
    names(splittingPoints) <- 'outComeT'
    transform2BinnedVar(payData, splittingPoints)
    setnames(payData, 'outComeTGrouped', 'outcomeTrans')
    asFactorDT(payData, 'outcomeTrans')
    
    #constructing the default formula, expressed by the var form
    if(length(unique(payData$inStateTimeTrans)) == 1){
      form <- as.formula('outcomeTrans ~ 1')
    } else {
      form <- as.formula('outcomeTrans ~ inStateTimeTrans')
    }
    
    indVars <- !names(payData) %in% c('polNumb', 'transStat', 'transMN', 'outComeT', 'claimSize', 'predicted', 'observed', 'exposure', 'predMN', 'inProcTime', 'outcomeTrans', 'delt0PayTimeTrans', 'inStateTimeMax', 'bookDate', 'finYear', 'delt1Pay', 'delt2Pay', 'cumDelt1Pay', 'cumDelt2Pay')
    modVars <- names(payData)[indVars]
    isFact <- isFactorDT(payData[, .SD, .SDcol = indVars])
    levs <- extractLevelDT(payData[, .SD, .SDcol = indVars])
    nLevs <- laply(levs, length)
    uselessFact <- names(levs)[nLevs < 2]
    if(length(uselessFact) > 0){
      indVars <- !names(payData) %in% c('polNumb', 'transStat', 'transMN', 'outComeT', 'claimSize', 'predicted', 'observed', 'exposure', 'predMN', 'inProcTime', 'outcomeTrans', 'delt0PayTimeTrans', 'inStateTimeMax', 'bookDate', 'finYear', 'delt1Pay', 'delt2Pay', 'cumDelt1Pay', 'cumDelt2Pay', uselessFact)
      modVars <- names(payData)[indVars]
      isFact <- isFactorDT(payData[, .SD, .SDcol = indVars])
    }
    
    if(isFactorDT(payData, 'inStateTimeTrans')) asNumericDT(payData, 'inStateTimeTrans') #changed 050520
    if(isFactorDT(payData, 'inProcTimeTrans')) asNumericDT(payData, 'inProcTimeTrans') #changed 050520
    
    # catMerge is necessary to assure that for later transitions, no almost empty inStateTimeTrans and inProcTimeTrans variables are in the model.
    # (we need to do an additional selection step for the payFit functions after the dataPrep, hence we need to rerun the catMerge function)
    catMerge(payData, 'inStateTimeTrans', nMinLev, nMinMod) #changed 050520
    catMerge(payData, 'inProcTimeTrans', nMinLev, nMinMod) #changed 050520
    if(sum(names(payData) == 'inStateTimeTransTrans')){ #changed 050520
      payData[, inStateTimeTrans := NULL] #changed 050520
      setnames(payData, 'inStateTimeTransTrans', 'inStateTimeTrans') #changed 050520
    } #changed 050520
    if(sum(names(payData) == 'inProcTimeTransTrans')){ #changed 050520
      payData[, inProcTimeTrans := NULL] #changed 050520
      setnames(payData, 'inProcTimeTransTrans', 'inProcTimeTrans') #changed 050520
    } #changed 050520
    if(!isFactorDT(payData, 'inStateTimeTrans')) asFactorDT(payData, 'inStateTimeTrans') #changed 050520
    if(!isFactorDT(payData, 'inProcTimeTrans')) asFactorDT(payData, 'inProcTimeTrans') #changed 050520
    
    #the formula needs to be adapted in case that there are more than nMinMod observations and in case that there are modVars
    if(nrow(payData) > nMinMod & length(modVars) > 0){
      
      nParam <- rep(1, length(modVars))
      allVarsLevs <- list()
      length(allVarsLevs) <- length(modVars) + 1
      names(allVarsLevs) <- c(modVars, 'inStateTimeTrans')
      nIntParams <- 0
      selInt <- c()
      for(iVar in 1:length(modVars)){
        if(isFact[iVar]){
          levs <- extractLevelDT(payData, modVars[iVar])[[1]]
          refLev <- extractRefLevelDT(payData, modVars[iVar])[[1]]
          levs <- levs[levs != refLev]
          nParam[iVar] <- length(levs)
          allVarsLevs[[iVar]] <- levs
          if(sum(intVars %in% modVars[iVar])){
            nIntParams <- nIntParams + length(levs)
            selInt <- c(selInt, modVars[iVar])
          }
        } else {
          if(sum(intVars %in% modVars[iVar])){
            nIntParams <- nIntParams + 1
            selInt <- c(selInt, modVars[iVar])
          }
        }
      }
      inStateLevs <- extractLevelDT(payData, 'inStateTimeTrans')[[1]]
      refInStateLev <- extractRefLevelDT(payData, 'inStateTimeTrans')[[1]]
      inStateLevs <- inStateLevs[inStateLevs != refInStateLev]
      allVarsLevs$inStateTimeTrans <- inStateLevs
      intLev <- length(inStateLevs)
      
      nParamTotMax <- (sum(nParam) + sum(nIntParams*intLev) + 1)*nSplits
      nSamp <- pmax(nSamp, nParamTotMax*nTimesParams)
      
      #constructing the formula, expressed by the var form
      
      if(nrow(payData) > nParamTotMax*nTimesParams){
        explVar <- modVars
        if(length(unique(payData$inStateTimeTrans)) != 1){
          if(length(selInt) > 0) explVar <- c(explVar, paste(selInt, 'inStateTimeTrans', sep = '*'))
        }
        form <- as.formula(paste('outcomeTrans ~', paste(explVar, collapse = " + ")))
      } else {
        form <- as.formula('outcomeTrans ~ 1')
        if(length(unique(payData$inStateTimeTrans)) != 1){
          form <- as.formula('outcomeTrans ~ inStateTimeTrans') #just the baseline hazard
        }
      }
    } else {
      modVars <- NULL
      isFact <- NULL
    }
    
    #fitting the multinomial (MN) model, if there are more than nMaxNoMod observations
    if(nrow(payData) > nMaxNoMod){
      if(verbose) print('>>> Fitting phase')
      if(verbose) print(paste0('Covariates: ', form[3]))
      if(!RF){ #changed 150420
        result[[iPay]]$fitPay <- fitMN(payData, form, modVars, isFact, min(nrow(payData), nSamp), nIterMax, verboseConv)
      } else { #changed 150420
        result[[iPay]]$fitPay <- fitRF(payData, form, modVars, isFact, min(nrow(payData), nSamp), 128, 300, verboseConv) #changed 150420
      } #changed 150420
      
      result[[iPay]]$trainDT <- payData
      result[[iPay]]$meanVals <- payData[, mean(outComeT), by = outcomeTrans][order(outcomeTrans), ][, estCost := V1][, V1 := NULL]
      
      result[[iPay]]$meanVals[nSplits + 1, estCost := result[[iPay]]$fitGPD$mle[1]/(1 - result[[iPay]]$fitGPD$mle[2]) + threshold]
    }
  }
  
  return(result)
  
}

payFitNP <- function(inputDT, quantsN, quantsP, nMinMod, nMinLev, nTimesParams, nMaxNoMod, intVars = NULL, nSamp = 50000, nIterMax = 1000, verbose = TRUE, verboseConv = FALSE){
  # Check input parameters.
  if(!is.list(inputDT)) stop("The 'inputDT' argument needs to be a list of data.tables objects.")
  checkLength(list(nMinMod, nMinLev, nTimesParams, nMaxNoMod), c(1, 1, 1, 1))
  checkLength(list(verbose, nSamp, nIterMax, verboseConv), c(1, 1, 1, 1))
  if(!is.null(intVars)) checkCharVec(list(intVars))
  
  checkWholeNumb(list(nSamp, nIterMax))
  checkWholeNumb(list(nMinMod, nMinLev, nTimesParams, nMaxNoMod))
  checkNumVec(list(quantsN, quantsP))
  checkLogicVec(list(verbose, verboseConv))
  checkRanges(list(nSamp, nIterMax), list(c('>', 0), c('>', 0)))
  checkRanges(list(nMinMod, nMinLev, nTimesParams, nMaxNoMod), list(c('>', 0), c('>', 0), c('>', 0), c('>', 0)))
  alply(quantsN, 1, function(xx) checkRanges(list(xx), list( c('>=', 0, '<=', 1))))
  alply(quantsP, 1, function(xx) checkRanges(list(xx), list( c('>=', 0, '<=', 1))))
  
  meanVals <- list()
  result <- list()
  
  for(iPay in 1:length(inputDT)){
    
    if(verbose) print(paste0('>>> Fitting payment distr ', iPay))
    if(verbose) print('>>> Prep + fitting GPD phase')
    
    result[[iPay]] <- list()
    
    payData <- copy(inputDT[[iPay]]) #changed 050520
    payData <- addFirstLast(payData, 'polNumb', F, T)
    payData <- payData[last == 1 & (transMN %in% c('P', 'TP')), ][, last := NULL]
    
    # table(payData$transMN)
    removeEmptyLevelsDT(payData)
    payData <- copy(payData[outComeT != 'Inf', ])
    asNumericDT(payData, 'outComeT')
    
    threshold_N <- -quantile(-payData[outComeT < 0, outComeT], quantsN[length(quantsN)])
    dataGPD_N <- payData[outComeT < threshold_N, outComeT]
    valsGPD_N <- sort(dataGPD_N, decreasing = T)
    noGPD_N <- FALSE
    
    if(length(dataGPD_N) > nMaxNoMod){
      
      splitOn <- TRUE
      nextJump <- 5
      lastTry <- FALSE
      currentInd  <- 0
      
      while(splitOn){
        tempFit_N <- try(suppressWarnings(ismev::gpd.fit(xdat = -dataGPD_N, threshold = -threshold_N, show = FALSE)), silent = TRUE)
        if(class(tempFit_N) != "try-error"){
          splitOn <- FALSE
        } else if(lastTry){
          splitOn <- FALSE
          noGPD_N <- TRUE
          warning('No GPD distribution could be fitted. Please check if there is no error in your data.')
          
        } else {
          currentInd <- currentInd + nextJump
          threshold_N <- valsGPD_N[currentInd]
          dataGPD_N <- payData[outComeT < threshold_N, outComeT]
          if(length(dataGPD_N) <= nMaxNoMod){
            lastTry <- TRUE
          }
        }
      }
      
      result[[iPay]]$fitGPD_N <- tempFit_N
      
    } else {
      noGPD_N <- TRUE
    }
    
    threshold_P <- quantile(payData[outComeT > 0, outComeT], quantsP[length(quantsP)])
    dataGPD_P <- payData[outComeT > threshold_P, outComeT]
    valsGPD_P <- sort(dataGPD_P)
    noGPD_P <- FALSE
    
    if(length(dataGPD_P) > nMaxNoMod){
      
      splitOn <- TRUE
      nextJump <- 5
      lastTry <- FALSE
      currentInd  <- 0
      
      while(splitOn){
        tempFit_P <- try(suppressWarnings(ismev::gpd.fit(xdat = dataGPD_P, threshold = threshold_P, show = FALSE)), silent = TRUE)
        if(class(tempFit_P) != "try-error"){
          splitOn <- FALSE
        } else if(lastTry){
          splitOn <- FALSE
          noGPD_P <- TRUE
          warning('No GPD distribution could be fitted. Please check if there is no error in your data.')
        } else {
          currentInd <- currentInd + nextJump
          threshold_P <- valsGPD_P[currentInd]
          dataGPD_P <- payData[outComeT > threshold_P, outComeT]
          if(length(dataGPD_P) <= nMaxNoMod){
            lastTry <- TRUE
          }
        }
      }
      
      result[[iPay]]$fitGPD_P <- tempFit_P
      
    } else {
      noGPD_P <- TRUE
    }
    
    splits_N <- sort(unique(-quantile(-payData[outComeT < 0, outComeT], quantsN)))
    splits_N <- smartQuantSel(payData[outComeT < 0, .(outComeT)], 'outComeT', splits_N, nMinLev)
    splits_N[1] <- threshold_N
    splits_P <- sort(unique(quantile(payData[outComeT > 0, outComeT], quantsP)))
    splits_P <- smartQuantSel(payData[outComeT > 0, .(outComeT)], 'outComeT', splits_P, nMinLev)
    splits_P[length(splits_P)] <- threshold_P
    
    splittingPoints <- list()
    splittingPoints[[1]] <- unique(c(splits_N, splits_P))
    
    result[[iPay]]$splits <- splittingPoints
    nSplits <- length(splittingPoints[[1]])
    names(splittingPoints) <- 'outComeT'
    transform2BinnedVar(payData, splittingPoints)
    setnames(payData, 'outComeTGrouped', 'outcomeTrans')
    asFactorDT(payData, 'outcomeTrans')
    
    #constructing the default formula, expressed by the var form
    if(length(unique(payData$inStateTimeTrans)) == 1){
      form <- as.formula('outcomeTrans ~ 1')
    } else {
      form <- as.formula('outcomeTrans ~ inStateTimeTrans')
    }
    
    indVars <- !names(payData) %in% c('polNumb', 'transStat', 'transMN', 'outComeT', 'claimSize', 'predicted', 'observed', 'exposure', 'predMN', 'inProcTime', 'outcomeTrans', 'delt0PayTimeTrans', 'inStateTimeMax', 'bookDate', 'finYear', 'delt1Pay', 'delt2Pay', 'cumDelt1Pay', 'cumDelt2Pay')
    modVars <- names(payData)[indVars]
    isFact <- isFactorDT(payData[, .SD, .SDcol = indVars])
    levs <- extractLevelDT(payData[, .SD, .SDcol = indVars])
    nLevs <- laply(levs, length)
    uselessFact <- names(levs)[nLevs < 2]
    if(length(uselessFact) > 0){
      indVars <- !names(payData) %in% c('polNumb', 'transStat', 'transMN', 'outComeT', 'claimSize', 'predicted', 'observed', 'exposure', 'predMN', 'inProcTime', 'outcomeTrans', 'delt0PayTimeTrans', 'inStateTimeMax', 'bookDate', 'finYear', 'delt1Pay', 'delt2Pay', 'cumDelt1Pay', 'cumDelt2Pay', uselessFact)
      modVars <- names(payData)[indVars]
      isFact <- isFactorDT(payData[, .SD, .SDcol = indVars])
    }
    
    if(isFactorDT(payData, 'inStateTimeTrans')) asNumericDT(payData, 'inStateTimeTrans') #changed 050520
    if(isFactorDT(payData, 'inProcTimeTrans')) asNumericDT(payData, 'inProcTimeTrans') #changed 050520
    # catMerge is necessary to assure that for later transitions, no almost empty inStateTimeTrans and inProcTimeTrans variables are in the model.
    # (we need to do an additional selection step for the payFit functions after the dataPrep, hence we need to rerun the catMerge function)
    catMerge(payData, 'inStateTimeTrans', nMinLev, nMinMod) #changed 050520
    catMerge(payData, 'inProcTimeTrans', nMinLev, nMinMod) #changed 050520
    if(sum(names(payData) == 'inStateTimeTransTrans')){ #changed 050520
      payData[, inStateTimeTrans := NULL] #changed 050520
      setnames(payData, 'inStateTimeTransTrans', 'inStateTimeTrans') #changed 050520
    } #changed 050520
    if(sum(names(payData) == 'inProcTimeTransTrans')){ #changed 050520
      payData[, inProcTimeTrans := NULL] #changed 050520
      setnames(payData, 'inProcTimeTransTrans', 'inProcTimeTrans') #changed 050520
    } #changed 050520
    if(!isFactorDT(payData, 'inStateTimeTrans')) asFactorDT(payData, 'inStateTimeTrans') #changed 050520
    if(!isFactorDT(payData, 'inProcTimeTrans')) asFactorDT(payData, 'inProcTimeTrans') #changed 050520
    
    #the formula needs to be adapted in case that there are more than nMinMod observations and in case that there are modVars
    if(nrow(payData) > nMinMod & length(modVars) > 0){
      
      nParam <- rep(1, length(modVars))
      allVarsLevs <- list()
      length(allVarsLevs) <- length(modVars) + 1
      names(allVarsLevs) <- c(modVars, 'inStateTimeTrans')
      nIntParams <- 0
      selInt <- c()
      for(iVar in 1:length(modVars)){
        if(isFact[iVar]){
          levs <- extractLevelDT(payData, modVars[iVar])[[1]]
          refLev <- extractRefLevelDT(payData, modVars[iVar])[[1]]
          levs <- levs[levs != refLev]
          nParam[iVar] <- length(levs)
          allVarsLevs[[iVar]] <- levs
          if(sum(intVars %in% modVars[iVar])){
            nIntParams <- nIntParams + length(levs)
            selInt <- c(selInt, modVars[iVar])
          }
        } else {
          if(sum(intVars %in% modVars[iVar])){
            nIntParams <- nIntParams + 1
            selInt <- c(selInt, modVars[iVar])
          }
        }
      }
      inStateLevs <- extractLevelDT(payData, 'inStateTimeTrans')[[1]]
      refInStateLev <- extractRefLevelDT(payData, 'inStateTimeTrans')[[1]]
      inStateLevs <- inStateLevs[inStateLevs != refInStateLev]
      allVarsLevs$inStateTimeTrans <- inStateLevs
      intLev <- length(inStateLevs)
      
      nParamTotMax <- (sum(nParam) + sum(nIntParams*intLev) + 1)*nSplits
      nSamp <- pmax(nSamp, nParamTotMax*nTimesParams)
      
      #constructing the formula, expressed by the var form
      
      if(nrow(payData) > nParamTotMax*nTimesParams){
        explVar <- modVars
        if(length(unique(payData$inStateTimeTrans)) != 1){
          if(length(selInt) > 0) explVar <- c(explVar, paste(selInt, 'inStateTimeTrans', sep = '*'))
        }
        if(iPay == 1){ #changed 290420
          explVar <- explVar[explVar != 'inProcTimeTrans'] #changed 290420
        } #changed 290420
        form <- as.formula(paste('outcomeTrans ~', paste(explVar, collapse = " + ")))
      } else {
        form <- as.formula('outcomeTrans ~ 1')
        if(length(unique(payData$inStateTimeTrans)) != 1){
          form <- as.formula('outcomeTrans ~ inStateTimeTrans') #just the baseline hazard
        }
      }
    } else {
      modVars <- NULL
      isFact <- NULL
    }
    
    #fitting the multinomial (MN) model, if there are more than nMaxNoMod observations
    if(nrow(payData) > nMaxNoMod){
      if(verbose) print('>>> Fitting phase')
      if(verbose) print(paste0('Covariates: ', form[3]))
      result[[iPay]]$fitPay <- fitMN(payData, form, modVars, isFact, min(nrow(payData), nSamp), nIterMax, verboseConv)
      
      result[[iPay]]$trainDT <- payData
      result[[iPay]]$meanVals <- payData[, mean(outComeT), by = outcomeTrans][order(outcomeTrans), ][, estCost := V1][, V1 := NULL]
      
      #if the shape is higher or equal than 1, then the expected value is undefined
      if(!noGPD_N){
        if(result[[iPay]]$fitGPD_N$mle[2] < 1){
          result[[iPay]]$meanVals[1, estCost := -result[[iPay]]$fitGPD_N$mle[1]/(1 - result[[iPay]]$fitGPD_N$mle[2]) + threshold_N]
        }
      }
      if(!noGPD_P){
        if(result[[iPay]]$fitGPD_P$mle[2] < 1){
          result[[iPay]]$meanVals[nSplits + 1, estCost := result[[iPay]]$fitGPD_P$mle[1]/(1 - result[[iPay]]$fitGPD_P$mle[2]) + threshold_P]
        }
      }
    }
  }
  
  return(result)
  
}

rSplicingNP_fast <- function(sampDT, finPayMods, listNr){
  
  trainDT <- finPayMods[[listNr]]$trainDT
  
  if(nrow(sampDT) == 1){
    predProbs <- t(as.matrix(predict(finPayMods[[listNr]]$fitPay, newdata = sampDT, type = "probs")))
  } else {
    predProbs <- predict(finPayMods[[listNr]]$fitPay, newdata = sampDT, type = "probs")
  }
  
  nSamps <- nrow(sampDT)
  sampsObs <- rep(NA, nSamps)
  
  splits <- c(0, finPayMods[[listNr]]$splits[[1]])
  smallerDT <- trainDT[outComeT < splits[1], outComeT]
  largerDT <- trainDT[outComeT > splits[length(splits)], outComeT]
  
  for(iObs in 1:nSamps){
    sampMN <- rmultinom(1, 1, predProbs[iObs, ])
    
    sampleRow <- which(sampMN[, 1] == 1)
    if(sampleRow == 1){
      sampsObs[iObs] <- sample(smallerDT, 1, replace = TRUE)
    } else if(sampleRow == length(splits)){
      sampsObs[iObs] <- sample(largerDT, 1, replace = TRUE)
    } else {
      sampsObs[iObs] <- runif(1, splits[sampleRow], splits[sampleRow + 1])
    }
  }
  
  return(sampsObs)
  
}

## predPay
#'
#' @param sampDT
#' @param finPayMods
#' @param listNr
#' @param verbose boolean indicating whether output should be printed or not
#'
#' @return the expected values from the multinomial payment model

#changed 290420 (this function is added to the mix)
predPay <- function(sampDT, finPayMods, listNr, verbose = TRUE){
  
  trainDT <- finPayMods[[listNr]]$trainDT
  if(nrow(sampDT) == 1){
    predProbs <- t(as.matrix(predict(finPayMods[[listNr]]$fitPay, newdata = sampDT, type = "probs")))
  } else {
    predProbs <- predict(finPayMods[[listNr]]$fitPay, newdata = sampDT, type = "probs")
  }
  preds <- predProbs %*% as.matrix(finPayMods[[listNr]]$meanVals[, estCost])
  return(preds)
}


## rSplicingNP
#' In rSplicingNP, we take one sample from each line of sampDT.
#'
#' @param sampDT
#' @param finPayMods
#' @param listNr
#' @param uniform - default value is TRUE
#' @param sampGPD - default value is FALSE
#' @param verbose boolean indicating whether output should be printed or not - default value is TRUE
#'
#' @return the samples from the multinomial payment model
rSplicingNP <- function(sampDT, finPayMods, listNr, uniform = TRUE, sampGPD = FALSE, verbose = TRUE){
  
  trainDT <- finPayMods[[listNr]]$trainDT
  
  if(nrow(sampDT) == 1){
    predProbs <- t(as.matrix(predict(finPayMods[[listNr]]$fitPay, newdata = sampDT, type = "probs")))
  } else {
    predProbs <- predict(finPayMods[[listNr]]$fitPay, newdata = sampDT, type = "probs")
  }
  
  # preds <- predProbs %*% as.matrix(meanVals[, V1])
  
  nSamps <- nrow(sampDT)
  sampsObs <- rep(NA, nSamps)
  
  for(iObs in 1:nSamps){
    if(verbose) if(iObs %% 1000 == 0) print(paste0('Sample number: ', iObs))
    sampMN <- rmultinom(1, 1, predProbs[iObs, ])
    splits <- c(0, finPayMods[[listNr]]$splits[[1]])
    temp <- c()
    if(length(splits) >= 3){
      for(iSpl in 2:(length(splits) - 1)){
        if(uniform){
          temp <- c(temp, runif(sampMN[iSpl, 1], splits[iSpl], splits[iSpl + 1]))
        } else {
          temp <- c(temp, sample(trainDT[outComeT > splits[iSpl] & outComeT <= splits[iSpl + 1], outComeT], sampMN[iSpl, 1], replace = TRUE))
        }
      }
    }
    
    if(sampGPD){
      
      epsilon <- 0.05
      
      xi_N <- finPayMods[[listNr]]$fitGPD_N$mle[1]
      beta_N <- finPayMods[[listNr]]$fitGPD_N$mle[2]
      splits <- finPayMods[[listNr]]$splits[[1]]
      threshold_N <- splits[1]
      lambda_N <- sampMN[length(splits), 1]
      
      PML_N <- threshold_N - (beta_N/xi_N)*((-lambda_N/log(1-epsilon))^xi_N - 1)
      sampsGPD_N <- threshold_N - rgp(sampMN[1, 1], shape = xi_N, scale = beta_N)
      sampsGPD_N[sampsGPD_N < PML_N] <- PML_N
      
      xi_P <- finPayMods[[listNr]]$fitGPD_P$mle[1]
      beta_P <- finPayMods[[listNr]]$fitGPD_P$mle[2]
      splits <- finPayMods[[listNr]]$splits[[1]]
      threshold_P <- splits[length(splits)]
      lambda_P <- sampMN[length(splits), 1]
      
      PML_P <- threshold_P + (beta_P/xi_P)*((-lambda_P/log(1-epsilon))^xi_P - 1)
      sampsGPD_P <- threshold_P + rgp(sampMN[length(splits), 1], shape = xi_P, scale = beta_P)
      sampsGPD_P[sampsGPD_P > PML_P] <- PML_P
      
      sampsObs[iObs] <- c(sampsGPD_N, temp, sampsGPD_P)
      
    } else {
      sampsObs[iObs] <- c(sample(trainDT[outComeT < splits[1], outComeT], sampMN[1, 1], replace = TRUE), temp, sample(trainDT[outComeT > splits[length(splits)], outComeT], sampMN[length(splits), 1], replace = TRUE))
    }
  }
  
  return(sampsObs)
  
}

#rSplicing is made with another phylosophy in mind, as compared to rSplicingNP, here we take
# nSamps from each line of sampDT.

rSplicing <- function(nSamps, listNr, sampDT, finPayMods, uniform = TRUE, sampGPD = FALSE, verbose = TRUE){
  
  trainDT <- finPayMods[[listNr]]$trainDT
  
  if(nrow(sampDT) == 1){
    predProbs <- t(as.matrix(predict(finPayMods[[listNr]]$fitPay, newdata = sampDT, type = "probs")))
  } else {
    predProbs <- predict(finPayMods[[listNr]]$fitPay, newdata = sampDT, type = "probs")
  }
  
  # preds <- predProbs %*% as.matrix(meanVals[, V1])
  
  sampsObs <- list()
  
  for(iObs in 1:nSamps){
    if(verbose) if(iObs %% 1000 == 0) print(paste0('Sample number: ', iObs))
    sampMN <- rmultinom(1, nSamps, predProbs[iObs, ])
    splits <- c(0, finPayMods[[listNr]]$splits[[1]])
    temp <- c()
    for(iSpl in 1:(length(splits) - 1)){
      if(uniform){
        temp <- c(temp, runif(sampMN[iSpl, 1], splits[iSpl], splits[iSpl + 1]))
      } else {
        temp <- c(temp, sample(trainDT[outComeT > splits[iSpl] & outComeT <= splits[iSpl + 1], outComeT], sampMN[iSpl, 1], replace = TRUE))
      }
    }
    
    sampsObs[[iObs]] <- temp
    
    if(sampGPD){
      xi <- finPayMods[[listNr]]$fitGPD$mle[1]
      beta <- finPayMods[[listNr]]$fitGPD$mle[2]
      splits <- finPayMods[[listNr]]$splits[[1]]
      threshold <- splits[length(splits)]
      lambda <- sampMN[length(splits), 1]
      epsilon <- 0.05
      
      PML <- threshold + (beta/xi)*((-lambda/log(1-epsilon))^xi - 1)
      sampsGPD <- threshold + rgp(sampMN[length(splits), 1], shape = xi, scale = beta)
      sampsGPD[sampsGPD > PML] <- PML
      
      sampsObs[[iObs]] <- c(sampsObs[[iObs]], sampsGPD)
      
    } else {
      sampsObs[[iObs]] <- c(sampsObs[[iObs]], sample(trainDT[outComeT > splits[length(splits)], outComeT], sampMN[length(splits) + 1, 1], replace = TRUE))
    }
  }
  return(sampsObs)
}

cvMod <- function(fitData, nGroups, quants, nMinMod, nMinLev, nTimesParams, nMaxNoMod, intVars, nSamp, nIterMax, verbose, verboseConv){
  
  splitList <- createFolds(1:nrow(fitData), k = nGroups, list = TRUE, returnTrain = FALSE)
  cvCP <- rep(NA, length(splitList))
  cvMSE <- rep(NA, length(splitList))
  
  fitData[, observed := outComeT]
  fitData[, exposure := 1]
  fitData[, claimSize := outComeT]
  fitData[, transMN := transType]
  
  for(iSpl in 1:length(splitList)){
    
    testInds <- splitList[[iSpl]]
    trainInds <- setdiff(1:nrow(fitData), testInds)
    
    redData <- list()
    redData[[1]] <- fitData[trainInds, ]
    
    finPayMods <- payFit(redData, quants, nMinMod, nMinLev, nTimesParams, nMaxNoMod, intVars, nSamp, nIterMax, verbose, verboseConv)
    predProbs <- predict(finPayMods[[1]]$fitPay, newdata = fitData[testInds, ], type = "probs")
    
    preds <- predProbs %*% as.matrix(finPayMods[[1]]$meanVals[, estCost])
    fitData[testInds, predicted := preds]
    
    inds <- sample(1:length(testInds), pmin(20000, length(testInds)))
    cvCP[iSpl] <- concProbCont(fitData[testInds[inds], ], 0, +Inf, 5000, calcCI = TRUE)$concProbTau
    cvMSE[iSpl] <- mse(fitData$observed[testInds[inds]], fitData$predicted[testInds[inds]], na.rm = TRUE)
  }
  
  return(list(concProb = cvCP, MSE = cvMSE))
  
}

smartQuantSel <- function(dataDT, varName, splitsAll, nMinLev){
  
  cdfSpl <- rep(NA, length(splitsAll))
  for(iSpl in 1:length(splitsAll)){
    cdfSpl[iSpl] <- sum(dataDT[, get(varName)] < splitsAll[iSpl], na.rm = TRUE)
  }
  
  diffSpl <- c(cdfSpl[1], cdfSpl[2:length(cdfSpl)] - cdfSpl[1:(length(cdfSpl) - 1)], cdfSpl[length(cdfSpl)])
  while(length(diffSpl[!is.na(diffSpl)]) != 0 & min(diffSpl, na.rm = T) < nMinLev){
    indMin <- which.min(diffSpl)
    if(indMin == 1){
      diffSpl[2] <- diffSpl[1] + diffSpl[2]
      diffSpl <- diffSpl[-1]
      splitsAll <- splitsAll[-1]
    } else if(indMin == length(diffSpl)){
      diffSpl[length(diffSpl) - 1] <- diffSpl[length(diffSpl) - 1] + diffSpl[length(diffSpl)]
      diffSpl <- diffSpl[-length(diffSpl)]
      splitsAll <- splitsAll[length(splitsAll)]
    } else {
      if(diffSpl[indMin - 1] < diffSpl[indMin + 1]){
        diffSpl[indMin - 1] <- diffSpl[indMin - 1] + diffSpl[indMin]
        splitsAll <- splitsAll[-(indMin - 1)]
      } else {
        diffSpl[indMin + 1] <- diffSpl[indMin + 1] + diffSpl[indMin]
        splitsAll <- splitsAll[-indMin]
      }
      diffSpl <- diffSpl[-indMin]
    }
  }
  
  splitsFin <- sort(unique(splitsAll))
  return(splitsFin)
  
}

inflCorrBE <- function(predSim, inflRates, currentYear, maxSim = NULL){
  
  if(!is.list(predSim)) stop("The argument 'predSim' is a list of lists, containing data.table object. An output object of the microModel::simPaths function.")
  checkNumOrIntVec(list(inflRates, currentYear))
  if(!is.null(maxSim)) checkNumOrIntVec(list(maxSim))
  
  BE <- list()
  nTrans <- length(inflRates)
  nRunSim <- length(predSim)
  
  if(!is.null(maxSim)){
    nRunSim <- maxSim
  }
  
  for(iList in 1:nRunSim){
    
    selInflRate <- inflRates[min(nTrans, iList)]
    polNumbs <- names(predSim[[iList]])
    
    for(iPol in polNumbs){
      ind <- which(names(BE) == iPol)
      if(length(ind) == 0){
        BE[[iPol]] <- predSim[[iList]][[iPol]]$outComeT * (selInflRate/100 + 1)^(as.numeric(as.character(predSim[[iList]][[iPol]]$finYear)) - currentYear)
        BE[[iPol]][is.na(BE[[iPol]])] <- 0
      } else {
        indBE <- which(predSim[[iList - 1]][[iPol]]$transMN == 'P')
        indCurr <- which(!is.na(predSim[[iList]][[iPol]]$outComeT))
        BE[[ind]][indBE][indCurr] <- BE[[ind]][indBE][indCurr] + predSim[[iList]][[iPol]]$outComeT[indCurr] * (selInflRate/100 + 1)^(as.numeric(as.character(predSim[[iList]][[iPol]]$finYear[indCurr])) - currentYear)
      }
    }
  }
  return(BE)
}

#to extract the observed reserve for the test data set
extractObsRes <- function(predSim, fullDT, inflRates, raw2 = FALSE, totalCost = FALSE){
  # Check input parameters.
  if(!is.list(predSim)) stop("The argument 'predSim' is a list of lists, containing data.table object. An output object of the microModel::simPaths function.")
  if(!is.list(fullDT)) stop("The argument 'predSim' is a list")
  
  checkNumOrIntVec(list(inflRates))
  checkLogicVec(list(raw2))
  checkEqualLength(list(inflRates, fullDT))
  checkLength(list(raw2), 1)
  
  polNumbsCurr <- c()
  obsCost <- list()
  obsCostRaw <- list()
  
  for(iList in 1:(length(fullDT))){
    
    selInflRate <- inflRates[min(length(inflRates), iList)] #changed 180520
    
    if(iList == 1){
      polNumbs <- llply(predSim, names)
      polNumbs <- unique(unlist(polNumbs))
      
      length(obsCost) <- length(polNumbs)
      names(obsCost) <- polNumbs
      obsCost <- llply(obsCost, function(xx) xx <- 0)
      
      if(raw2){
        length(obsCostRaw) <- length(polNumbs)
        names(obsCostRaw) <- polNumbs
        obsCostRaw <- llply(obsCostRaw, function(xx) xx <- 0)
      }
      
    }
    
    if(totalCost){
      polNumbsCurr <- polNumbs
    } else {
      polNumbsCurr <- unique(c(polNumbsCurr, names(predSim[[iList]])))
    }
    
    for(iPol in polNumbsCurr){
      if(nrow(fullDT[[iList]][polNumb == iPol, ])){
        ind <- which(names(obsCost) == iPol)
        #selFinYear <- fullDT[[iList]][polNumb == iPol, finYear][1]
        #obsCost[[ind]] <- obsCost[[ind]] + fullDT[[iList]][polNumb == iPol, outComeT][1] * (selInflRate/100 + 1)^(selFinYear - currentYear)
        #if(raw2) obsCostRaw[[ind]] <- obsCostRaw[[ind]] + fullDT[[iList]][polNumb == iPol, outComeT][1]
        
        temp_DT <- unique(fullDT[[iList]][polNumb == iPol, .(finYear, outComeT)])
        temp_DT <- temp_DT[outComeT != 0, ]
        if(nrow(temp_DT) > 0){
          for(iTemp in 1:nrow(temp_DT)){
            obsCost[[ind]] <- obsCost[[ind]] + temp_DT[iTemp, 2]* (selInflRate/100 + 1)^(currentYear - temp_DT[iTemp, 1])
            if(raw2) obsCostRaw[[ind]] <- obsCostRaw[[ind]] + temp_DT[iTemp, 2]
          }
        }
      }
    }
  }
  if(raw2){
    return(list(obsResRaw = obsCostRaw, obsRes = obsCost))
  } else {
    return(list(obsRes = obsCost))
  }
}

#the Spearman correlation and the concProb are calculated for a predictor extracted from the BE distribution and the observed reserve
corrObsPred <- function(obsResVec, predVec, nus = 0, obsMin = NULL, obsMax = NULL, predMin = NULL, predMax = NULL){
  # Check input parameters.
  checkNumOrIntVec(list(obsResVec, predVec, nus))
  checkEqualLength(list(obsResVec, predVec))
  
  redInputs <- sum(c(!is.null(obsMin), !is.null(obsMax), !is.null(predMin), !is.null(predMax)))
  if(!(redInputs != 0 | redInputs != 4)){
    stop("The arguments obsMin, obsMax, predMin and predMax are either all NULL, or all have a distinct numeric value.")
  }
  
  corrDT <- data.table(observed = obsResVec, predicted = predVec)
  spear <- suppressWarnings(cor.test(obsResVec, predVec, method = 'spearman')$estimate)
  cp <- aaply(nus, 1, function(xx) concProbCont(corrDT, nu = xx)$concProbGlobal)
  
  if(sum(redInputs) == 4){
    corrRedDT <- corrDT[observed >= obsMin & observed <= obsMax & predicted <= predMax & predicted >= predMin, ]
    spearRed <- suppressWarnings(cor.test(corrRedDT$observed, corrRedDT$predicted, method = 'spearman')$estimate)
    cpRed <- aaply(nus, 1, function(xx) concProbCont(corrRedDT, nu = xx)$concProbGlobal)
    return(list(spearCorr = spear, spearCorrRed = spearRed, concProb = cp, concProbRed = cpRed))
  }
  return(list(spearCorr = spear, concProb = cp))
}

# the concProb is computed for a timeMod)
concProbPayPred <- function(predSim, finPayMods, allDataDT, transNr, nus, verbose = TRUE){
  # Check input parameters.
  if(!is.list(predSim)) stop("The argument 'predSim' is a list of lists, containing data.table object. An output object of the microModel::simPaths function.")
  checkEqualLength(list(finPayMods, allDataDT))
  checkNumOrIntVec(list(transNr, nus))
  checkLogicVec(list(verbose))
  
  if(length(predSim) < transNr) stop("The argument 'predSim' should be a list with at least 'transNr' objects in it." )
  if(length(finPayMods) < transNr) stop("The argument 'finPayMods' should be a list with at least 'transNr' objects in it." )
  if(length(allDataDT) < transNr) stop("The argument 'allDataDT' should be a list with at least 'transNr' objects in it." )
  
  nPol <- length(names(predSim[[transNr]]))
  observed <- rep(NA, nPol)
  predicted <- rep(NA, nPol)
  count <- 1
  
  for(iPol in names(predSim[[transNr]])){
    if(verbose) if(count %% 100 == 0) print(paste0('Sample ', count))
    sampDT <- dataFin[[transNr]][polNumb == iPol, ][.N, ]
    if(nrow(sampDT) != 0){
      if(!is.na(sampDT$outComeT) & sampDT$transType %in% c('TP', 'P')){
        observed[count] <- sampDT$outComeT
        asFactorDT(sampDT, names(sampDT)[names(sampDT) %in% c('inStateTimeTrans', 'inProcTimeTrans', 'delt1PayTimeTrans', 'delt2PayTimeTrans')]) #added
        predProbs <- predict(finPayMods[[transNr]]$fitPay, newdata = sampDT, type = "probs")
        predBins <- predProbs * as.matrix(finPayMods[[transNr]]$meanVals[, estCost])
        predicted[count] <- sum(predBins)
        # samps <- rep(NA, 500)
        # for(iRep in 1:500){
        #   samps[iRep] <- rSplicingNP_fast(sampDT, payMods, min(nTrans, transNr))
        # }
        # predicted[count] <- mean(samps, na.rm = T)
      }
    }
    count <- count + 1
  }
  
  corrDT <- data.table(observed = observed, predicted = predicted)
  corrDT <- corrDT[!is.na(observed) & !is.na(predicted), ]
  concProb <- rep(NA, length(nus))
  for(iNu in 1:length(nus)){
    concProb[iNu] <- concProbCont(corrDT, nu = nus[iNu])$concProbGlobal
  }
  
  return(list(concProb = concProb))
}

#To be used when the predictions of the time models need to be evaluated on a test set (expressed by predSim) that was not present during the model fit
#helper function to compute the cumulative incidence
cifCalc <- function(predProbs, selTime, selTrans){
  
  cif <- rep(NA, selTime)
  cif[1] <- predProbs[1, colnames(predProbs) == selTrans]
  OS <- predProbs[1, colnames(predProbs) == 'N']
  if(selTime > 1){
    for(iStep in 2:selTime){
      cif[iStep] <- OS*(predProbs[iStep, colnames(predProbs) == selTrans])
      OS <- OS*predProbs[iStep, colnames(predProbs) == 'N']
    }
  }
  cumsum(cif)
}

# the concProb is computed for a payMod
concProbTimePred <- function(predSim, finTimeMods, allDataDT, transNr, selTime, nSample = NULL, meanPred = TRUE, overallSurv = TRUE, verbose = TRUE){
  # Check input parameters.
  if(!is.list(predSim)) stop("The argument 'predSim' is a list of lists, containing data.table object. An output object of the microModel::simPaths function.")
  checkWholeNumb(list(transNr, selTime))
  checkEqualLength(list(finPayMods, allDataDT))
  
  if(!is.null(nSample)){
    checkWholeNumb(list(nSample))
  } else{
    nSample <- nrow(predSim[[transNr]])
  }
  
  if(length(predSim) < transNr) stop("The argument 'predSim' should be a list with at least 'transNr' objects in it." )
  if(length(finPayMods) < transNr) stop("The argument 'finPayMods' should be a list with at least 'transNr' objects in it." )
  if(length(allDataDT) < transNr) stop("The argument 'allDataDT' should be a list with at least 'transNr' objects in it." )
  
  checkLength(list(nSample, selTime, overallSurv, verbose, meanPred), rep(1, 5))
  checkLogicVec(list(verbose, overallSurv, meanPred))
  
  if(verbose) print('Prepping the data')
  
  allDT <- allDataDT[[transNr]]
  
  lastDT <- copy(allDT)
  addFirstLast(lastDT, 'polNumb', F, T)
  lastDT <- copy(lastDT[last == 1, ])
  removeEmptyLevelsDT(lastDT)
  
  asNumericDT(allDT, c('inStateTimeTrans', 'inProcTimeTrans'))
  
  firstDT <- copy(allDT)
  addFirstLast(firstDT, 'polNumb', T, F)
  firstDT <- copy(firstDT[first == 1, ])
  removeEmptyLevelsDT(firstDT)
  
  # predSimAll <- rbindlist(predSim[[transNr]], use.names = TRUE)
  # checkDT(predSimAll, c('inStateTimeTrans', 'transMN'))
  
  if(nSample > length(predSim[[transNr]])){
    polNumbs <- names(predSim[[transNr]])
  } else {
    polNumbs <- sample(names(predSim[[transNr]]), nSample)
  }
  selPredDT <- firstDT[polNumb %in% polNumbs, ]
  predDT <- lastDT[polNumb %in% polNumbs, ]
  
  inStateTimeTransMaxTime <- max(as.numeric(names(table(finTimeMods[[transNr]]$trainDT$inStateTimeTrans)))) #changed 190520
  inProcTimeTransMaxTime <- max(as.numeric(names(table(finTimeMods[[transNr]]$trainDT$inProcTimeTrans)))) #changed 190520
  inStateTimeTransMinTime <- min(as.numeric(names(table(finTimeMods[[transNr]]$trainDT$inStateTimeTrans)))) #changed 190520
  inProcTimeTransMinTime <- min(as.numeric(names(table(finTimeMods[[transNr]]$trainDT$inProcTimeTrans)))) #changed 190520
  
  if(verbose) print('Extracting the predicted CIFs')
  
  cifP_all <- rep(NA, nrow(selPredDT))
  cifTP_all <- rep(NA, nrow(selPredDT))
  cifTN_all <- rep(NA, nrow(selPredDT))
  cif_all <- rep(NA, nrow(selPredDT))
  results <- list() #changed 190520
  
  for(iRow in 1:nrow(selPredDT)){
    tempDT <- copy(selPredDT[rep(iRow, selTime), ])
    tempDT$inStateTimeTrans <- pmax(pmin(seq(tempDT$inStateTimeTrans[1], nrow(tempDT)), inStateTimeTransMaxTime), inStateTimeTransMinTime)
    tempDT$inProcTimeTrans <- pmax(pmin(tempDT$inProcTimeTrans[1]:(tempDT$inProcTimeTrans[1] + nrow(tempDT) - 1), inProcTimeTransMaxTime), inProcTimeTransMinTime)
    asFactorDT(tempDT, c('inStateTimeTrans', 'inProcTimeTrans'))
    predProbs <- predict(finTimeMods[[transNr]]$fitTime, newdata = tempDT, type = "probs")
    cifP <- cifCalc(predProbs, selTime, 'P')
    cifTP <- cifCalc(predProbs, selTime, 'TP')
    cifTN <- cifCalc(predProbs, selTime, 'TN')
    cif <- 1 - (cifP + cifTP + cifTN)
    if(meanPred){
      cifP_all[iRow] <- mean(cifP)
      cifTP_all[iRow] <- mean(cifTP)
      cifTN_all[iRow] <- mean(cifTN)
      cif_all[iRow] <- mean(cif)
    } else {
      cifP_all[iRow] <- cifP[selTime]
      cifTP_all[iRow] <- cifTP[selTime]
      cifTN_all[iRow] <- cifTN[selTime]
      cif_all[iRow] <- cif[selTime]
    }
  }
  
  #Computing the within and between concProb
  for(cTransI in c('P', 'TP', 'TN')){
    for(cTransJ in c('P', 'TP', 'TN')){
      if(verbose) print(paste(paste(paste0('Going for concProb ', cTransI), '-'), cTransJ))
      if(overallSurv){
        predDT[, pred := cif_all]
      } else {
        if(cTransI == 'P'){
          predDT[, pred := cifP_all]
        } else if(cTransI == 'TP'){
          predDT[, pred := cifTP_all]
        } else if(cTransI == 'TN'){
          predDT[, pred := cifTN_all]
        }
      }
      results[[cTransI]][[cTransJ]] <- concProbCalcCR(predDT, selTime, cTransI, cTransJ, FALSE, verbose)
    }
  }
  
  #Computing the overall concProb
  if(overallSurv){
    predDT[, pred := cif_all]
    for(cTransI in c('P', 'TP', 'TN')){
      if(verbose) print(paste('Going for concProb O -', cTransI))
      results[['O']][[cTransI]] <- concProbCalcCR(predDT, selTime, cTransI, NULL, TRUE, verbose)
    }
  } else {
    # results$O <- concProbCalcCR(predDT, selTime, NULL, NULL, TRUE, verbose) #I don't understand the use of this rule, but this might be due to my temporary misunderstanding
    for(cTransI in c('P', 'TP', 'TN')){
      if(verbose) print(paste('Going for concProb O -', cTransI))
      if(cTransI == 'P'){
        predDT[, pred := cifP_all]
      } else if(cTransI == 'TP'){
        predDT[, pred := cifTP_all]
      } else if(cTransI == 'TN'){
        predDT[, pred := cifTN_all]
      }
      results[['O']][[cTransI]] <- concProbCalcCR(predDT, selTime, cTransI, NULL, TRUE, verbose)
    }
  }
  
  if(verbose) print('Going for concProb O - O')
  diffTrans <- c('P', 'TP', 'TN')
  nCompTot <- 0
  concProbOverall <- 0
  prevalenceMat <- matrix(NA, nrow = 3, ncol = 3, dimnames = list(rownames = diffTrans, colnames = diffTrans))
  for(iRun in 1:3){
    for(jRun in 1:3){
      tempNComp <- results[[diffTrans[iRun]]][[diffTrans[jRun]]]$nComp
      tempConcProb <- results[[diffTrans[iRun]]][[diffTrans[jRun]]]$concProb
      if(!is.null(tempNComp) & !is.null(tempConcProb)){
        concProbOverall <- concProbOverall + tempConcProb*tempNComp
        nCompTot <- nCompTot + tempNComp
        prevalenceMat[iRun, jRun] <- tempNComp
      }
    }
  }
  results[['O']][['O']]$concProb <- concProbOverall/nCompTot
  prevalenceMat <- round(prevalenceMat/nCompTot, digits = 3)
  
  #Alternative to the above nested for-loop (exactly the same result)
  # results[['O']][['O']] <- concProbCalcCR(predDT, selTime, NULL, NULL, TRUE, verbose)
  
  diffTrans <- c('P', 'TP', 'TN', 'O')
  # resultCP <- matrix(NA, nrow = 3, ncol = 4, dimnames = list(rownames = diffTrans[-4], colnames = diffTrans))
  resultCP <- matrix(NA, nrow = 4, ncol = 4, dimnames = list(rownames = diffTrans, colnames = diffTrans))
  
  for(iRun in 1:3){
    for(jRun in 1:3){
      tempRes <- results[[diffTrans[iRun]]][[diffTrans[jRun]]]$concProb
      if(!is.null(tempRes)) resultCP[iRun, jRun] <- tempRes
    }
  }
  for(iRun in 1:4){
    tempRes <- resultCP[iRun, 4] <- results[['O']][[diffTrans[iRun]]]$concProb
    if(!is.null(tempRes)) resultCP[iRun, 4] <- tempRes
  }
  return(list(resAll = results, CP = resultCP, prevalence = prevalenceMat))
}

#to be used when you want to check how the simulated time points correlate with the predictions on which they are based (to gauge the internal consistency of the predictions)
#slightly different function of concProbTimePred, used for when the input is different
concProbTimePredSim <- function(predSim, finTimeMods, transNr, selTime, nSample = NULL, meanPred = TRUE, overallSurv = TRUE, verbose = TRUE){
  # Check input parameters and certain variables in 'predSimAll'.
  if(!is.list(predSim)) stop("The argument 'predSim' is a list of lists, containing data.table object. An output object of the microModel::simPaths function.")
  checkWholeNumb(list(transNr, selTime))
  
  if(!is.null(nSample)){
    checkWholeNumb(list(nSample))
  } else{
    nSample <- nrow(predSim[[transNr]])
  }
  
  checkLength(list(nSample, selTime, overallSurv, verbose, meanPred), rep(1, 5))
  checkLogicVec(list(verbose, overallSurv, meanPred))
  
  predSimAll <- rbindlist(predSim[[transNr]], use.names = TRUE)
  checkDT(predSimAll, c('inStateTimeTrans', 'transMN'))
  
  if(verbose) print('Prepping the data')
  
  #Getting the predictions out of the predSimAll (only the line where the actual transition takes place matters)
  predSimAllSamp <- predSimAll[sample(1:nrow(predSimAll), nSample), ]
  nTimePoints <- length(unique(predSimAllSamp$inStateTimeTrans))
  asFactorDT(predSimAllSamp, c('inStateTimeTrans', 'inProcTimeTrans'))
  predProbs <- predict(finTimeMods[[transNr]]$fitTime, newdata = predSimAllSamp, type = "probs")
  
  if(verbose) print('Extracting the predicted CIFs')
  
  #Computing the cumulative incidence functions for the different transition types
  selTrans <- 'P'
  cif_P <- matrix(NA, nrow = nSample, ncol = nTimePoints)
  cif_P[, 1] <- predProbs[, colnames(predProbs) == selTrans]
  for(iStep in 2:nTimePoints){
    cif_P[, iStep] <- aaply(predProbs[, 1], 1, function(xx) prod(rep(xx, iStep)))*predProbs[, colnames(predProbs) == selTrans]
  }
  selTrans <- 'TP'
  cif_TP <- matrix(NA, nrow = nSample, ncol = nTimePoints)
  cif_TP[, 1] <- predProbs[, colnames(predProbs) == selTrans]
  for(iStep in 2:nTimePoints){
    cif_TP[, iStep] <- aaply(predProbs[, 1], 1, function(xx) prod(rep(xx, iStep)))*predProbs[, colnames(predProbs) == selTrans]
  }
  selTrans <- 'TN'
  cif_TN <- matrix(NA, nrow = nSample, ncol = nTimePoints)
  cif_TN[, 1] <- predProbs[, colnames(predProbs) == selTrans]
  for(iStep in 2:nTimePoints){
    cif_TN[, iStep] <- aaply(predProbs[, 1], 1, function(xx) prod(rep(xx, iStep)))*predProbs[, colnames(predProbs) == selTrans]
  }
  
  cif <- matrix(NA, nrow = nSample, ncol = nTimePoints)
  predDT <- copy(predSimAllSamp[, .(inStateTimeTrans, transMN)])
  asNumericDT(predDT, c('inStateTimeTrans'))
  results <- list()
  
  #Computing the within and between concProb
  for(cTransI in c('P', 'TP', 'TN')){
    for(cTransJ in c('P', 'TP', 'TN')){
      if(verbose) print(paste(paste(paste0('Going for concProb ', cTransI), '-'), cTransJ))
      if(overallSurv){
        cif <- 1 - aaply(cif_P + cif_TP + cif_TN, 1, function(xx) cumsum(xx))
      } else {
        if(cTransI == 'P'){
          cif <- cif_P
        } else if(cTransI == 'TP'){
          cif <- cif_TP
        } else if(cTransI == 'TN'){
          cif <- cif_TN
        }
      }
      if(meanPred){
        selPred <- aaply(cif, 1, function(xx) mean(xx[1:selTime]))
        predDT[, pred := selPred]
      } else {
        predDT[, pred := cif[, selTime]]
      }
      results[[cTransI]][[cTransJ]] <- concProbCalcCR(predDT, selTime, cTransI, cTransJ, FALSE, verbose)
    }
  }
  
  #Computing the overall concProb
  if(overallSurv){
    cif <- 1 - aaply(cif_P + cif_TP + cif_TN, 1, function(xx) cumsum(xx))
    predDT[, pred := cif[, selTime]]
    for(cTransI in c('P', 'TP', 'TN')){
      if(verbose) print(paste('Going for concProb O -', cTransI))
      results[['O']][[cTransI]] <- concProbCalcCR(predDT, selTime, cTransI, NULL, TRUE, verbose)
    }
  } else {
    results$O <- concProbCalcCR(predDT, selTime, NULL, NULL, TRUE, verbose)
    for(cTransI in c('P', 'TP', 'TN')){
      if(verbose) print(paste('Going for concProb O -', cTransI))
      if(cTransI == 'P'){
        cif <- cif_P
      } else if(cTransI == 'TP'){
        cif <- cif_TP
      } else if(cTransI == 'TN'){
        cif <- cif_TN
      }
      if(meanPred){
        selPred <- aaply(cif, 1, function(xx) mean(xx[1:selTime]))
        predDT[, pred := selPred]
      } else {
        predDT[, pred := cif[, selTime]]
      }
      results[['O']][[cTransI]] <- concProbCalcCR(predDT, selTime, cTransI, NULL, TRUE, verbose)
    }
  }
  
  if(verbose) print('Going for concProb O - O')
  diffTrans <- c('P', 'TP', 'TN')
  nCompTot <- 0
  concProbOverall <- 0
  prevalenceMat <- matrix(NA, nrow = 3, ncol = 3, dimnames = list(rownames = diffTrans, colnames = diffTrans))
  for(iRun in 1:3){
    for(jRun in 1:3){
      tempNComp <- results[[diffTrans[iRun]]][[diffTrans[jRun]]]$nComp
      tempConcProb <- results[[diffTrans[iRun]]][[diffTrans[jRun]]]$concProb
      if(!is.null(tempNComp) & !is.null(tempConcProb)){
        concProbOverall <- concProbOverall + tempConcProb*tempNComp
        nCompTot <- nCompTot + tempNComp
        prevalenceMat[iRun, jRun] <- tempNComp
      }
    }
  }
  results[['O']][['O']]$concProb <- concProbOverall/nCompTot
  prevalenceMat <- round(prevalenceMat/nCompTot, digits = 3)
  
  #Alternative to the above nested for-loop (exactly the same result)
  # results[['O']][['O']] <- concProbCalcCR(predDT, selTime, NULL, NULL, TRUE, verbose)
  
  diffTrans <- c('P', 'TP', 'TN', 'O')
  # resultCP <- matrix(NA, nrow = 3, ncol = 4, dimnames = list(rownames = diffTrans[-4], colnames = diffTrans))
  resultCP <- matrix(NA, nrow = 4, ncol = 4, dimnames = list(rownames = diffTrans, colnames = diffTrans))
  
  for(iRun in 1:3){
    for(jRun in 1:3){
      tempRes <- results[[diffTrans[iRun]]][[diffTrans[jRun]]]$concProb
      if(!is.null(tempRes)) resultCP[iRun, jRun] <- tempRes
    }
  }
  for(iRun in 1:4){
    tempRes <- resultCP[iRun, 4] <- results[['O']][[diffTrans[iRun]]]$concProb
    if(!is.null(tempRes)) resultCP[iRun, 4] <- tempRes
  }
  return(list(resAll = results, CP = resultCP, prevalence = prevalenceMat))
}

QuantifQuantileMean <- function (X, Y, alpha = c(0.05, 0.25, 0.5, 0.75, 0.95), x = seq(min(X), max(X), length = 100), testN = c(35, 40, 45, 50, 55), p = 2, B = 50, tildeB = 20, same_N = TRUE, ncores = 1){
  # Check input parameters.
  if (!is.numeric(X))
    stop("X must be numeric")
  if (!is.numeric(Y))
    stop("Y must be numeric")
  if (!is.numeric(x))
    stop("x must be numeric")
  if (!is.vector(X))
    stop("X must be a vector")
  if (!is.vector(Y))
    stop("Y must be a vector")
  if (!is.vector(x))
    stop("x must be a vector")
  if (!all(floor(testN) == testN & testN > 0))
    stop("testN must have entire positive entries")
  if (!all(alpha > 0 & alpha < 1))
    stop("alpha must be strictly between 0 and 1")
  if ((!(floor(B) == B)) | (B <= 0))
    stop("B must be a positive entire")
  if ((!(floor(tildeB) == tildeB)) | (tildeB <= 0))
    stop("tildeB must be a positive entire")
  if (p < 1)
    stop("p must be at least 1")
  if (!is.logical(same_N))
    stop("same_N must be logical")
  n <- length(X)
  hatISE_N <- array(0, dim = c(length(alpha) + 1, length(testN)))
  hatq_N <- array(0, dim = c(length(alpha) + 1, length(x), length(testN)))
  primeX <- matrix(sample(X, n * (B + tildeB), replace = TRUE),
                   nrow = (B + tildeB))
  calc_hatq_N <- function(N) {
    hatX <- choice.grid(X, N, ng = (B + tildeB))$opti_grid
    projXboot <- array(0, dim = c(n, B + tildeB))
    iminx <- array(0, dim = c(n, B + tildeB))
    for (i in 1:n) {
      RepX <- matrix(rep(X[i], N * (B + tildeB)), ncol = (B +
                                                            tildeB), byrow = TRUE)
      Ax <- sqrt((RepX - hatX)^2)
      iminx[i, ] <- apply(Ax, 2, which.min)
      mx <- matrix(c(iminx[i, ], c(1:(B + tildeB))), nrow = (B +
                                                               tildeB))
      projXboot[i, ] <- hatX[mx]
    }
    Hatq <- array(0, dim = c(length(x), length(alpha) + 1, (B +
                                                              tildeB)))
    Hatq_cell <- array(0, dim = c(N, length(alpha) + 1, (B +
                                                           tildeB)))
    proj_gridx_boot <- function(z) {
      Repz <- matrix(rep(z, N * (B + tildeB)), nrow = N,
                     byrow = TRUE)
      A <- sqrt((Repz - hatX)^2)
      i <- apply(A, 2, which.min)
      m <- matrix(c(i, c(1:(B + tildeB))), nrow = (B +
                                                     tildeB))
      z <- hatX[m]
      z
    }
    vector_x <- array(x, dim = c(length(x), 1))
    projection_x <- apply(vector_x, 1, proj_gridx_boot)
    repY <- matrix(rep(Y, B + tildeB), nrow = n)
    for (i in 1:N) {
      for (j in 1:(B + tildeB)) {
        a <- which(projXboot[, j] == hatX[i, j])
        if (length(a) > 0) {
          Hatq_cell[i, , j] <- c(mean(repY[a, j]), quantile(repY[a, j], probs = alpha))
        }
      }
    }
    identification <- function(z) {
      identification <- array(0, dim = c(B + tildeB, 1))
      i <- which(z == x)[1]
      for (j in 1:(B + tildeB)) {
        identification[j, ] <- which(projection_x[j,
                                                  i] == hatX[, j])
      }
      identification
    }
    identification_projection_x <- apply(as.matrix(vector_x), 1, identification)
    for (i in 1:(length(alpha) + 1)) {
      for (j in 1:length(x)) {
        r = matrix(c(identification_projection_x[, j],
                     rep(i, B + tildeB), c(1:(B + tildeB))), ncol = 3)
        Hatq[j, i, ] = Hatq_cell[r]
      }
    }
    hatq <- array(0, dim = c(length(x), length(alpha) + 1))
    hatq <- apply(Hatq[, , c(1:B), drop = FALSE], c(1, 2),
                  mean)
    HATq <- array(rep(hatq, tildeB), dim = c(length(x), length(alpha) + 1,
                                             tildeB))
    hatISE <- (HATq - Hatq[, , c((1 + B):(B + tildeB)), drop = FALSE])^2
    hatISE <- apply(hatISE, 2, sum)/(length(x) * tildeB)
    print(N)
    list(hatq = hatq, hatISE = hatISE)
  }
  parallel_hatq_hatISE <- mclapply(testN, calc_hatq_N, mc.cores = ncores,
                                   mc.set.seed = F)
  for (i in 1:length(testN)) {
    hatq_N[, , i] <- t(parallel_hatq_hatISE[[i]]$hatq)
    hatISE_N[, i] <- parallel_hatq_hatISE[[i]]$hatISE
  }
  if (same_N) {
    hatISEmean_N <- apply(hatISE_N, 2, mean)
    i_opt <- which.min(hatISEmean_N)
    N_opt <- testN[i_opt]
    hatq_opt <- hatq_N[, , i_opt, drop = F]
    hatq_opt <- matrix(hatq_opt, ncol = length(x))
  }
  else {
    i_opt <- apply(hatISE_N, 1, which.min)
    N_opt <- testN[i_opt]
    hatq_opt <- array(0, dim = c(length(alpha) + 1, length(x)))
    for (i in 1:(length(alpha) + 1)) {
      hatq_opt[i, ] <- hatq_N[i, , i_opt[i]]
    }
  }
  if (length(N_opt) == 1) {
    if (N_opt == min(testN)) {
      warning("N_opt is on the left boundary of testN")
    }
    if (N_opt == max(testN)) {
      warning("N_opt is on the right boundary of testN")
    }
  }
  else {
    if (any(N_opt == min(testN))) {
      warning("N_opt is on the left boundary of testN for at least one value of alpha")
    }
    if (any(N_opt == max(testN))) {
      warning("N_opt is on the right boundary of testN for at least one value of alpha")
    }
  }
  output <- list(fitted.values = hatq_opt, hatq_opt = hatq_opt, N_opt = N_opt, hatISE_N = hatISE_N, hatq_N = hatq_N, X = X, Y = Y, x = x, alpha = alpha, testN = testN)
  class(output) <- "QuantifQuantile"
  return(output)
}



fitNorMix <- function(inputDT, nComps,form,whichTrans = 'P', verboseConv = T ){
  
  
  meanVals <- list()
  result <- list()
  
  for(iPay in 1:length(inputDT)){
    
    if(verboseConv) print(paste0('>>> Fitting payment distr ', iPay))
    
    result[[iPay]] <- list()
    payData <- copy(inputDT[[iPay]]) 
    
    #select only lines where there was a payment
    payData <- payData[transMN %in% c('P', 'TP','PI'), ]
    
    result[[iPay]]$fitPay <- flexmix(form, data = payData, k=nComps, control = list(verb = 5, iter = 500))
    
    result[[iPay]]$trainDT <- payData
    result[[iPay]]$form <- form
    
  }
  
  return(result)
  
}

#to extract the observed reserve for the test data set
extractObsResRobin <- function(predSim, fullDT, inflRates, raw2 = FALSE, totalCost = FALSE){
  # Check input parameters.
  if(!is.list(predSim)) stop("The argument 'predSim' is a list of lists, containing data.table object. An output object of the microModel::simPaths function.")
  if(!is.list(fullDT)) stop("The argument 'predSim' is a list")
  
  checkNumOrIntVec(list(inflRates))
  checkLogicVec(list(raw2))
  checkEqualLength(list(inflRates, fullDT))
  checkLength(list(raw2), 1)
  
  polNumbsCurr <- c()
  obsCost <- list()
  obsCostRaw <- list()
  
  for(iList in 1:(length(fullDT))){
    
    selInflRate <- inflRates[min(length(inflRates), iList)] #changed 180520
    
    if(iList == 1){
      polNumbs <- llply(predSim, names)
      polNumbs <- unique(unlist(polNumbs))
      
      length(obsCost) <- length(polNumbs)
      names(obsCost) <- polNumbs
      obsCost <- llply(obsCost, function(xx) xx <- 0)
      
      if(raw2){
        length(obsCostRaw) <- length(polNumbs)
        names(obsCostRaw) <- polNumbs
        obsCostRaw <- llply(obsCostRaw, function(xx) xx <- 0)
      }
      
    }
    
    if(totalCost){
      polNumbsCurr <- polNumbs
    } else {
      polNumbsCurr <- unique(c(polNumbsCurr, names(predSim[[iList]])))
    }
    
    for(iPol in polNumbsCurr){
      if(nrow(fullDT[[iList]][polNumb == iPol, ])){
        ind <- which(names(obsCost) == iPol)
        #selFinYear <- fullDT[[iList]][polNumb == iPol, finYear][1]
        #obsCost[[ind]] <- obsCost[[ind]] + fullDT[[iList]][polNumb == iPol, outComeT][1] * (selInflRate/100 + 1)^(selFinYear - currentYear)
        #if(raw2) obsCostRaw[[ind]] <- obsCostRaw[[ind]] + fullDT[[iList]][polNumb == iPol, outComeT][1]
        
        temp_DT <- unique(fullDT[[iList]][polNumb == iPol, .(finYear, outComeT)])
        temp_DT <- temp_DT[outComeT != 0, ]
        if(nrow(temp_DT) > 0){
          for(iTemp in 1:nrow(temp_DT)){
            obsCost[[ind]] <- obsCost[[ind]] + temp_DT[iTemp, 2]* (selInflRate/100 + 1)^(currentYear - temp_DT[iTemp, 1])
            if(raw2) obsCostRaw[[ind]] <- obsCostRaw[[ind]] + temp_DT[iTemp, 2]
          }
        }
      }
    }
  }
  if(raw2){
    return(list(obsResRaw = obsCostRaw, obsRes = obsCost))
  } else {
    return(list(obsRes = obsCost))
  }
}



extractFinCalibTestDTNoMix<- function(BECalib,obsResCalib, BETest,obsResTest,savePath,nSim=1){
  iSim <- nSim
  set.seed(9*2020*nSim)
  optQuantList <- list()
  relDiff <- rep(NA, nSim)
  absDiff <- rep(NA, nSim)
  
  relDiffDistr <- rep(NA, nSim)
  absDiffDistr <- rep(NA, nSim)
  
  obsTestTotalVec <- rep(NA, nSim)
  sumTestMeanPredVec <-rep(NA, nSim)
  distrBEList <- list()
  obsResList <- list()
  NoCalibResList <- list()
  testMeanPredList <- list()
  
  calibPolNumbsList <- list()
  testPolNumbsList <- list()
  
  quantDTCalibList <- list()
  quantDTTestList <- list()
  # the lengths will represent the amount of polnumbs to sample from each 
  lenCalibDT <- length(BECalib)
  lenTestDT <- length(BETest)
  bindBE <- c(BECalib,BETest)
  bindObsRes <- c(obsResCalib,obsResTest)
  
  
  
  calibInds <- sample(1:length(bindBE), lenCalibDT, replace = F)
  polNumbsCalib <- names(bindBE)[calibInds]
  polNumbsTest <- names(bindBE)[-calibInds]
  
  subsetCalibBE <-  BECalib
  subsetTestBE <- BETest
  subsetCalibObsRes <- obsResCalib
  subsetTestObsRes <- obsResTest
  
  ###perform opt quants and abs/relative differences
  resOpt<- optQuants(subsetCalibBE,subsetCalibObsRes, method="concProb",nus=c(0,2000),tr=0.02,obsMin = -100, obsMax = 10000, predMin = -100, predMax = 10000)
  optQuantList[[iSim]] <- resOpt
  testDTBest <- laply(subsetTestBE, quantile,probs=resOpt$bestName)
  quantDTCalib <- data.table(pred = resOpt$bestSummary, obs = subsetCalibObsRes)
  quantDTCalibList[[iSim]] <- quantDTCalib
  quantDTTest <-  data.table(pred = testDTBest, obs = subsetTestObsRes)
  quantDTTestList[[iSim]] <- quantDTTest
  
  calibPolNumbsList[[iSim]] <- polNumbsCalib
  testPolNumbsList[[iSim]] <- polNumbsTest
  
  resTest <- QuantifQuantileMean(quantDTCalib$pred, quantDTCalib$obs, x =quantDTTest$pred, alpha = seq(.01, .99,.01), testN = 200)
  NoCalibResList[[iSim]] <- quantDTCalib$pred
  obsResList[[iSim]] <- quantDTTest$obs
  sampsQR <- resTest$fitted.values
  #print(dim(sampsQR))
  #print(length(sampsQR))
  testMeanPred <- sampsQR[1, ]
  testMeanPredList[[iSim]] <- testMeanPred
  testQuantPred <- sampsQR[2:100, ]
  
  sampsBE <- aaply(testQuantPred, 2, function(xx) sample(xx, 1000, replace = T))
  distrBE <- aaply(sampsBE, 2, sum)
  distrBEList[[iSim]] <- distrBE
  
  obsTestTotal <- sum(quantDTTest$obs)
  
  
  obsTestTotalVec[iSim] <-  obsTestTotal
  sumTestMeanPredVec[iSim] <- sum(testMeanPred)
  relDiff[iSim] <- obsTestTotal/sum(testMeanPred)
  absDiff[iSim] <- obsTestTotal - sum(testMeanPred)
  relDiffDistr[iSim] <- obsTestTotal/mean(distrBE)
  absDiffDistr[iSim] <- obsTestTotal - mean(distrBE)
  
  
  hist(distrBE,breaks=50,main= paste("Best estimate distribution",iSim))
  abline(v = obsTestTotal, col = 2, lwd = 6)
  abline(v = sum(testMeanPred), col = 4, lwd = 3)
  
  saveList <- list(relDiff=relDiff, absDiff=absDiff, relDiffDistr=relDiffDistr, absDiffDistr=absDiffDistr,
                   obsTestTotalVec=obsTestTotalVec, sumTestMeanPredVec=sumTestMeanPredVec,
                   distrBEList=distrBEList, obsResList=obsResList,NoCalibResList=NoCalibResList,
                   testMeanPredList=testMeanPredList,calibPolNumbsList=calibPolNumbsList,
                   testPolNumbsList=testPolNumbsList,optQuantList=optQuantList, quantDTCalibList=quantDTCalibList,
                   quantDTTestList=quantDTTestList)
  save(saveList, file= savePath)
  
  
  
  return(list(relDiff=relDiff, absDiff=absDiff, relDiffDistr=relDiffDistr, absDiffDistr=absDiffDistr,
              obsTestTotalVec=obsTestTotalVec, sumTestMeanPredVec=sumTestMeanPredVec,
              distrBEList=distrBEList, obsResList=obsResList,NoCalibResList=NoCalibResList,
              testMeanPredList=testMeanPredList,calibPolNumbsList=calibPolNumbsList,
              testPolNumbsList=testPolNumbsList,optQuantList=optQuantList, quantDTCalibList=quantDTCalibList,
              quantDTTestList=quantDTTestList))
}

inflCorrBERobin <- function(predSim, inflRates, currentYear, maxSim = NULL){
  
  if(!is.list(predSim)) stop("The argument 'predSim' is a list of lists, containing data.table object. An output object of the microModel::simPaths function.")
  checkNumOrIntVec(list(inflRates, currentYear))
  if(!is.null(maxSim)) checkNumOrIntVec(list(maxSim))
  
  BE <- list()
  nTrans <- length(inflRates)
  nRunSim <- length(predSim)
  
  if(!is.null(maxSim)){
    nRunSim <- maxSim
  }
  
  for(iList in 1:nRunSim){
    
    selInflRate <- inflRates[min(nTrans, iList)]
    polNumbs <- names(predSim[[iList]])
    
    for(iPol in polNumbs){
      ind <- which(names(BE) == iPol)
      if(length(ind) == 0){
        BE[[iPol]] <- predSim[[iList]][[iPol]]$outComeT * (selInflRate/100 + 1)^(predSim[[iList]][[iPol]]$finYear - currentYear)
        BE[[iPol]][is.na(BE[[iPol]])] <- 0
      } else {
        indBE <- which(predSim[[iList - 1]][[iPol]]$transMN == 'P')
        indCurr <- which(!is.na(predSim[[iList]][[iPol]]$outComeT))
        BE[[ind]][indBE][indCurr] <- BE[[ind]][indBE][indCurr] + predSim[[iList]][[iPol]]$outComeT[indCurr] * (selInflRate/100 + 1)^(predSim[[iList]][[iPol]]$finYear[indCurr] - currentYear)
      }
    }
  }
  return(BE)
}

timeFitEasy <- function(inputDT, form1, form2, form3, mod = "MN" ){
  
  if(!is.list(inputDT)) stop("The 'inputDT' argument needs to be a list of data.tables objects.")
  llply(inputDT, function(xx) checkDT(xx, c('inStateTimeTrans', 'transType', 'inProcTimeTrans', 'transStat')))
  
  result <- list()
  for(iList in 1:length(inputDT)){
    
    result[[iList]] <- list()
    if(iList == 1){
      form <- form1
    }
    else if(iList == 1){
      form <- form2
    }
    else{
      form <- form3
    }
    
    print(paste0('Fitting transition ', iList))
    
    if(mod =="MN"){ #changed 150420
      result[[iList]]$fitTime <- fitMN(inputDT[[iList]], form, modVars = NULL, isFact= NULL, nSamp = 10000, nIterMax = 1000, verboseConv = T)
    }
    result[[iList]]$trainDT <- inputDT[[iList]]
    result[[iList]]$explVar <- labels(terms(form))
    result[[iList]]$propTable <- prop.table(table(inputDT[[iList]][,'transMN']))
    
  }
  return(result)
}


#timeMods <- finTimeMods; payMods <- finPayMods; trainData <- trainDT; timeSplits <- NULL; predData <- calibDT2; fixedTimeMax <- 24;  perLen <- 30; nSims <- 10; lastTrans <- 10; verbose <- TRUE; payModType = "splicedGPD"
simPaths2 <- function(timeMods, payMods, trainData, timeSplits, predData, fixedTimeMax, perLen, nSims, lastTrans,payModType = "MN", verbose){ #changed 080520
  # Check input parameters and certain variables in 'trainData' and 'predData'.
  if(!is.list(timeMods)) stop("Argument 'timeMods' is a list containing the different time model fits.")
  if(!is.list(payMods)) stop("Argument 'payMods' is a list containing the different payment model fits.")
  # checkEqualLength(list(timeMods, trainData, predData))
  # checkEqualLength(list(payMods, trainData, predData))
  llply(trainData, function(xx) checkDT(xx))
  llply(predData, function(xx) checkDT(xx))
  checkWholeNumb(list(nSims, lastTrans)) #changed 080520
  checkLogicVec(list(verbose))
  checkLength(list(nSims, verbose, lastTrans), 1) #changed 080520
  checkRanges(list(nSims, lastTrans), list(c('>', 0), c('>', 0))) #changed 080520
  
  nNonNs <- laply(predData, function(xx) ifelse(nrow(xx) > 0, nrow(xx[transMN != 'N', ]), 0)) #changed 150420
  if(sum(nNonNs) > 0) stop("The transMN column of at least one element of the 'predData' argument did not all just contain rows with value 'N'. Please correct for this.") #changed 150420
  nNonPs <- laply(predData, function(xx) ifelse(nrow(xx) > 0, nrow(xx[transType != 'P', ]), 0)) #changed 290420
  if(sum(nNonPs) > 0) stop("The transMN column of at least one element of the 'testDT' argument did not all just contain rows with value 'P'. Please correct for this.") #changed 150420
  
  if(verbose) print('Setting the stage')
  
  predSim <- list()
  nSim <- list()
  predTransAll <- list()
  nTrans <- min(length(timeMods), length(predData))
  
  # Defining the initial nSim and predSim objects
  # nSim indicates how many sims still need to be taken for the polNumb of interest,
  # while predSim contains the data sets that were used for the timeMod and payMod.
  # both objects are a list, of length the number of transitions and they contain as
  # elements as many polNumbs as still need simulations.
  
  minTrans <- NA
  
  for(iTrans in 1:nTrans){
    
    if(verbose) print(iTrans)
    if(nrow(predData[[iTrans]]) > 0){
      nSim[[iTrans]] <- as.list(rep(nSims, nrow(predData[[iTrans]]))) #a claimNr will just appear for the transition where it is still open (later on, not)
      predSim[[iTrans]] <- nSim[[iTrans]]
      predTransAll[[iTrans]] <- predData[[iTrans]]
      
      
      
      if(is.na(minTrans)) minTrans <- iTrans
      
      for(iRun in 1:length(nSim[[iTrans]])){
        predSim[[iTrans]][[iRun]] <- predTransAll[[iTrans]][rep(iRun, nSim[[iTrans]][iRun]),]
        predSim[[iTrans]][[iRun]]$inStateTimeTrans <- as.numeric(as.character(predSim[[iTrans]][[iRun]]$inStateTimeTrans))
        predSim[[iTrans]][[iRun]]$inProcTimeTrans <- as.numeric(as.character(predSim[[iTrans]][[iRun]]$inProcTimeTrans))
        predSim[[iTrans]][[iRun]][, simPer := 0]
      }
      predTransAll[[iTrans]] <- predData[[iTrans]][rep(1:nrow(predData[[iTrans]]), nSims), ]
      predTransAll[[iTrans]] <- predTransAll[[iTrans]][order(polNumb), ]
      
      names(nSim[[iTrans]]) <- predData[[iTrans]]$polNumb
      names(predSim[[iTrans]]) <- predData[[iTrans]]$polNumb
      
    }
  }
  
  addDT <- data.table()
  
  if(!is.na(minTrans)){# if there is at least one transition 
    
    iTrans <- minTrans
    goOn <- TRUE
    
    if(verbose) print('Looping over the different transitions')
    
    while(goOn){
      
      if(verbose) cat('\n')
      if(verbose) print('////||||\\\\')
      if(verbose) print(paste('TRANSITION', iTrans))
      if(verbose) print('\\\\||||////')
      if(verbose) cat('\n')
      
      if(verbose) print(paste('nSimTot: ', sum(unlist(nSim[[iTrans]]))))
      
      #Prepping and augmenting the data for the simulations
      
      asNumericDT(trainData[[min(nTrans, iTrans)]], c('inStateTimeTrans', 'inProcTimeTrans'))
      #inStateTimeTransMax <- max(trainData[[min(nTrans, iTrans)]][, inStateTimeTrans]) #removed 150420
      #inProcTimeTransMax <- max(trainData[[min(nTrans, iTrans)]][, inProcTimeTrans]) #removed 150420
      
      inStateTimeTransMaxPay <- max(as.numeric(names(table(payMods[[min(nTrans, iTrans)]]$trainDT$inStateTimeTrans)))) #changed 150420
      inProcTimeTransMaxPay <- max(as.numeric(names(table(payMods[[min(nTrans, iTrans)]]$trainDT$inProcTimeTrans)))) #changed 150420
      
      inStateTimeTransMinPay <- min(as.numeric(names(table(payMods[[min(nTrans, iTrans)]]$trainDT$inStateTimeTrans)))) #changed 150420
      inProcTimeTransMinPay <- min(as.numeric(names(table(payMods[[min(nTrans, iTrans)]]$trainDT$inProcTimeTrans)))) #changed 150420
      
      inStateTimeTransMaxTime <- max(as.numeric(names(table(timeMods[[min(nTrans, iTrans)]]$trainDT$inStateTimeTrans)))) #changed 150420
      inProcTimeTransMaxTime <- max(as.numeric(names(table(timeMods[[min(nTrans, iTrans)]]$trainDT$inProcTimeTrans)))) #changed 150420
      
      inStateTimeTransMinTime <- min(as.numeric(names(table(timeMods[[min(nTrans, iTrans)]]$trainDT$inStateTimeTrans)))) #changed 150420
      inProcTimeTransMinTime <- min(as.numeric(names(table(timeMods[[min(nTrans, iTrans)]]$trainDT$inProcTimeTrans)))) #changed 150420
      
      inStateTimeTransMin <- max(inStateTimeTransMinTime, inStateTimeTransMinPay) #changed 150420
      inProcTimeTransMin <- max(inProcTimeTransMinTime, inProcTimeTransMinPay) #changed 150420
      
      inStateTimeTransMax <- min(inStateTimeTransMaxTime, inStateTimeTransMaxPay) #changed 150420
      inProcTimeTransMax <- min(inProcTimeTransMaxTime, inProcTimeTransMaxPay) #changed 150420
      
      if(nTrans >= iTrans){
        if(nrow(addDT) != 0){
          addDT[, inStateTimeMax := inStateTimeTransMax]
        }
        predTrans <- rbind(predTransAll[[iTrans]], addDT,fill=T)
      } else {
        predTrans <- addDT
        #We need to reset the predSim data
        polNumbUniq <- unique(predTrans$polNumb)
        predSim[[iTrans]] <- list()
        
        for(iRun in 1:length(polNumbUniq)){
          predSim[[iTrans]][[iRun]] <- predTrans[polNumb == polNumbUniq[iRun],]
        }
        names(predSim[[iTrans]]) <- polNumbUniq
      }
      
      predTransL <- split(predTrans, by = 'polNumb')
      inStateTimeMax <- min(trainData[[min(nTrans, iTrans)]][1, inStateTimeMax], fixedTimeMax)
      
      if(verbose) print(paste('Number of open claims of this transition:', nrow(predTrans)))
      
      nSimTrans <- nSim[[iTrans]]
      addPer <- 0
      stillOpen <- TRUE
      
      # looping until all claims have their simulated transition
      
      while(stillOpen){
        
        ptm <- proc.time()
        addPer <- pmin(addPer + 1, inStateTimeMax)
        if(verbose) print(paste('Run', addPer))
        
        selPolNumbs <- names(unlist(nSimTrans))[unlist(nSimTrans) != 0]
        
        predTransDT <- rbindlist(predTransL)
        runData <- predTransDT[polNumb %in% selPolNumbs, ][transMN == 'N', ]
        
        if(addPer == 1){ #changed 050520
          asNumericDT(runData, c('inStateTimeTrans', 'inProcTimeTrans')) #changed 050520
          runData[inStateTimeTrans > inStateTimeTransMax, inStateTimeTrans := inStateTimeTransMax] #changed 050520
          runData[inStateTimeTrans < inStateTimeTransMin, inStateTimeTrans := inStateTimeTransMin] #changed 050520
          runData[inProcTimeTrans > inProcTimeTransMax, inProcTimeTrans := inProcTimeTransMax] #changed 050520
          runData[inProcTimeTrans < inProcTimeTransMin, inProcTimeTrans := inProcTimeTransMin] #changed 050520
          asFactorDT(runData, c('inStateTimeTrans', 'inProcTimeTrans')) #changed 050520
        } #changed 050520
        
        predTransL <- split(runData, by = 'polNumb')
        
        asFactorDT(runData, c('inStateTimeTrans', 'inProcTimeTrans'))
        if(sum(names(runData) == 'delt1PayTimeTrans') != 0) asFactorDT(runData, c('delt1PayTimeTrans'))
        if(sum(names(runData) == 'delt2PayTimeTrans') != 0) asFactorDT(runData, c('delt2PayTimeTrans'))
        
        if(nrow(runData) > 1){
          predProb <- predict(timeMods[[min(length(timeMods), min(nTrans, iTrans))]]$fitTime, newdata = runData, type = 'probs')
        } else {
          predProb <- t(as.matrix(predict(timeMods[[min(length(timeMods), min(nTrans, iTrans))]]$fitTime, newdata = runData, type = 'probs')))
        }
        
        if(ncol(predProb) < 4){
          
          print('missing predProb')
          
          
          fixedColNames <- c('N', 'P', 'TN', 'TP')
          missingCol <- !(fixedColNames %in% colnames(predProb))
          #print(missingCol)
          fixedColNames[missingCol]
          for(iLoop in 1:sum(missingCol)){
            predProb <- cbind(predProb, rep(0, nrow(predProb)))
            colnames(predProb)[ncol(predProb)] <- fixedColNames[missingCol][iLoop]
          }
          predProb <- predProb[, order(colnames(predProb), fixedColNames)]
        }
        
        if(verbose) print(paste0('The expected number of lines after this run: ', floor(mean(predProb[,1])*nrow(runData))))
        
        if(iTrans >= 1){
          pastedPredProb <- aaply(predProb, 1, function(xx) paste(xx, collapse = ''))
        }
        
        # No more non terminal payments when lastTrans is reached #changed 080520
        if(iTrans >= lastTrans){ #changed 080520
          predProb[, 4] <- predProb[, 4] + predProb[, 2] #changed 080520
          predProb[, 2] <- 0 #changed 080520
        } #changed 080520
        # No more staying in the state without a transition when inStateTimeMax is reached #changed 080520
        if(addPer >= inStateTimeMax & iTrans < lastTrans){ #changed 080520
          predProb[, 2] <- predProb[, 2] + predProb[, 1]/3 #changed 080520
          predProb[, 3] <- predProb[, 3] + predProb[, 1]/3 #changed 080520
          predProb[, 4] <- predProb[, 4] + predProb[, 1]/3 #changed 080520
          predProb[, 1] <- 0 #changed 080520
        } else if(addPer >= inStateTimeMax & iTrans >= lastTrans){ #changed 080520
          predProb[, 3] <- predProb[, 3] + predProb[, 1]/2 #changed 080520
          predProb[, 4] <- predProb[, 4] + predProb[, 1]/2 #changed 080520
          predProb[, 1] <- 0 #changed 080520
        }
        if(addPer >= inStateTimeMax){ #removed 080520
          for(iRow in 1:nrow(predProb)){ #removed 080520
            if(sum(predProb[iRow, ][2:4]) < 0.05){ #removed 080520
              predProb[iRow, 2:4] <- 1/3 #removed 080520
              predProb[iRow, 1] <- 0 #removed 080520
            } #removed 080520
          } #removed 080520
        } #removed 080520
        
        runData$inStateTimeTrans <- as.numeric(as.character(runData$inStateTimeTrans))
        runData$inProcTimeTrans <- as.numeric(as.character(runData$inProcTimeTrans))
        
        simRun <- as.list(rep(1, nrow(runData)))
        
        # The actual simulation of the transition
        for(iRun in 1:length(selPolNumbs)){
          if(iRun %% 1000 == 0) print(iRun)
          indNPol <- which(predTransL[[which(names(predTransL) == selPolNumbs[iRun])]][, transMN] == 'N')
          #if(length(indNPol)>10) print(selPolNumbs[iRun])
          sel <- which(runData$polNumb == selPolNumbs[iRun])
          uPredProb <- predProb[sel, ]
          if(length(sel) > 1){
            uPredProb <- unique(uPredProb)
          }
          
          if((sum(is.null(nrow(uPredProb))) + sum(nrow(uPredProb) == 1)) >= 1){ #hence one row only
            simRun[[iRun]] <- rmultinom(1, as.numeric(nSimTrans[selPolNumbs[iRun]]), prob = uPredProb)
            rownames(simRun[[iRun]]) <- colnames(predProb)
            
            nSimTrans[selPolNumbs[iRun]] <- simRun[[iRun]]['N', ]
            
            rowInds <- cumsum(simRun[[iRun]])
            x <- copy(predSim[[iTrans]][selPolNumbs[iRun]][[1]])
            
            indsN <- which(x$transMN == 'N')
            if(simRun[[iRun]][2] != 0) x[indsN, ] <- x[indsN, ][(rowInds[1] + 1):rowInds[2], transMN := 'P']
            if(simRun[[iRun]][3] != 0) x[indsN, ] <- x[indsN, ][(rowInds[2] + 1):rowInds[3], transMN := 'TN']
            if(simRun[[iRun]][4] != 0) x[indsN, ] <- x[indsN, ][(rowInds[3] + 1):rowInds[4], transMN := 'TP']
            
            if(sum(simRun[[iRun]][2:4]) != 0){
              if(sum(names(x) == 'simPer') == 0){
                x[, simPer := NA]
                asNumericDT(x, 'simPer')
              }
              x[indsN, ] <- x[indsN, ][(rowInds[1] + 1):rowInds[4], simPer := addPer]
              x[indsN, ] <- x[indsN, ][(rowInds[1] + 1):rowInds[4], inStateTimeTrans := max(c(min(c(inStateTimeTrans + addPer - 1, inStateTimeTransMax)), inStateTimeTransMin))] #changed 150420
              x[indsN, ] <- x[indsN, ][(rowInds[1] + 1):rowInds[4], inProcTimeTrans := max(c(min(c(inProcTimeTrans + addPer - 1, inProcTimeTransMax)), inProcTimeTransMin))] #changed 150420
              
            }
            
            predSim[[iTrans]][selPolNumbs[iRun]][[1]] <- copy(x)
            predTransL[[which(names(predTransL) == selPolNumbs[iRun])]][indNPol, transMN := x[indsN, transMN]]
            
          } else {
            
            pastedPredProbTemp <- pastedPredProb[which(runData$polNumb == selPolNumbs[iRun])]
            x <- copy(predSim[[iTrans]][selPolNumbs[iRun]][[1]])
            indsN <- which(x$transMN == 'N')
            
            pastedPredProbTempUniq <- unique(pastedPredProbTemp)
            nUniq <- length(pastedPredProbTempUniq)
            predProbTemp <- predProb[which(runData$polNumb == selPolNumbs[iRun]),]
            
            for(iUniq in 1:nUniq){
              uniqRows <- which(pastedPredProbTemp == pastedPredProbTempUniq[iUniq])
              simRunTemp <- rmultinom(1, length(uniqRows), prob = predProbTemp[uniqRows[1], ])
              x <- x[indsN[uniqRows], transMN := rep(rownames(simRunTemp), simRunTemp)]
            }
            
            predSim[[iTrans]][selPolNumbs[iRun]][[1]] <- copy(x)
            nSimTrans[selPolNumbs[iRun]] <- nrow(predSim[[iTrans]][selPolNumbs[iRun]][[1]][transMN == 'N', ])
            
            predSim[[iTrans]][selPolNumbs[iRun]][[1]][indsN, simPer := addPer]
            predSim[[iTrans]][selPolNumbs[iRun]][[1]][indsN, inStateTimeTrans := max(c(min(c(inStateTimeTrans + addPer - 1, inStateTimeTransMax)), inStateTimeTransMin))] #changed 150420
            predSim[[iTrans]][selPolNumbs[iRun]][[1]][indsN, inProcTimeTrans := max(c(min(c(inProcTimeTrans + addPer - 1, inProcTimeTransMax)), inProcTimeTransMin))] #changed 150420
            
            simRun[[iRun]] <- simRunTemp
            predTransL[[which(names(predTransL) == selPolNumbs[iRun])]][, transMN := x[indsN, transMN]]
            
          }
        }
        
        # Updating the values of the time vars that are in the time model
        
        predTransDT <- rbindlist(predTransL)
        
        predTransDT$inStateTimeTrans <- as.numeric(as.character(predTransDT$inStateTimeTrans))
        predTransDT$inProcTimeTrans <- as.numeric(as.character(predTransDT$inProcTimeTrans))
        predTransDT[, inStateTimeTrans := max(c(min(c(inStateTimeTrans + 1, inStateTimeTransMax)), inStateTimeTransMin))] #changed 150420
        predTransDT[, inProcTimeTrans := max(c(min(c(inProcTimeTrans + 1, inProcTimeTransMax)), inProcTimeTransMin))] #changed 150420
        
        asFactorDT(predTransDT, c('inStateTimeTrans', 'inProcTimeTrans'))
        
        predTransL <- split(predTransDT, by = 'polNumb')
        
        if(verbose) print(paste('Number of polNumbs with sims:', sum(unlist(nSimTrans) != 0)))
        if(verbose) print(paste('Number of remaining sims:', sum(unlist(nSimTrans[selPolNumbs]))))
        if(verbose) print(paste('Time in secs of this run:', round(as.numeric((proc.time() - ptm)[3]), digits = 3)))
        if(sum(unlist(nSimTrans) != 0) == 0) stillOpen <- FALSE
      }
      
      if(verbose) print('Simulating payments for those transitions that require a payment')
      
      iPol <- names(predSim[[iTrans]])[1]
      count <- 1
      
      for(iPol in names(predSim[[iTrans]])[count:length(predSim[[iTrans]])]){
        
        if(count %% 1000 == 0) print(count)
        
        payInds <- which(predSim[[iTrans]][iPol][[1]]$transMN %in% c('TP', 'P'))
        noPayInds <- setdiff(1:nrow(predSim[[iTrans]][iPol][[1]]), payInds)
        if(length(noPayInds)!=0){
          predSim[[iTrans]][iPol][[1]][noPayInds, outComeT := 0.0]
        }
        
        #selecting the ones that still need a payment simulation with nSimTrans or so
        
        # x[indsN, ][(rowInds[1] + 1):rowInds[4], inStateTimeTrans := max(c(min(c(inStateTimeTrans + addPer - 1, inStateTimeTransMax)), inStateTimeTransMin))] #removed 210420
        # predSim[[iTrans]][iPol][[1]][, inStateTimeTrans := max(c(min(c(inStateTimeTrans + addPer - 1, inStateTimeTransMax)), inStateTimeTransMin))] #removed 210420
        
        if(length(payInds) != 0){
          sampDT <- predSim[[iTrans]][iPol][[1]][payInds, ]
          asFactorDT(sampDT, names(sampDT)[names(sampDT) %in% c('inStateTimeTrans', 'inProcTimeTrans', 'delt1PayTimeTrans', 'delt2PayTimeTrans','deltRepTrans')]) #added
          sampDT[is.na(sampDT)] <- 0
          if(payModType == "MN"){
            samps <- rSplicingNP_fast(sampDT, payMods, min(nTrans, iTrans))
          }
          else if(payModType == "GLM"){
            samps <- predict(payMods[[min(nTrans, iTrans)]]$fitPay, newdata =sampDT ,type= "response")
          }
          else if(payModType == "normix"){
            samps <- unlist(predict(payMods[[min(nTrans, iTrans)]]$fitPay, newdata = sampDT, aggregate = TRUE))
          }
          else if(payModType == "splicedGPD"){
            samps <- predictSplicedPareto(sampDT, payMods, min(nTrans, iTrans))
          }
          else{
            
          }
          if(is.logical(samps)){
            print('breaking')
            break
          }
          predSim[[iTrans]][iPol][[1]][payInds, outComeT := samps]
        }
        
        count <- count + 1
        
      }
      
      # Basically, the object 'addDT' is created which will be added to the polnumbs
      # that still need simulations for the next transition
      
      if(verbose) print('Adding non terminal (P) simulations to the next transition')
      
      addDT <- data.table()
      
      if(length(nSim) < (iTrans + 1)){
        nSim[[iTrans + 1]] <- list()
      }
      if(length(predSim) < (iTrans + 1)){
        predSim[[iTrans + 1]] <- list()
      }
      
      inProcTimeTransPlus1Max <- max(as.numeric(trainData[[min(nTrans, iTrans + 1)]][, inProcTimeTrans]))
      
      colNamesFitPlus1 <- colnames(coef(timeMods[[min(length(timeMods), min(nTrans, iTrans + 1))]]$fitTime))
      inProcTimeTransPlus1MinTemp <- min(as.numeric(str_replace_all(colNamesFitPlus1[grep('inProcTimeTrans', colNamesFitPlus1)], "[a-zA-Z\\s]", ""))) - 1
      
      inProcTimeTransPlus1Min <- max(inProcTimeTransPlus1MinTemp[is.finite(inProcTimeTransPlus1MinTemp)],min(nTrans, iTrans + 1))
      if(inProcTimeTransPlus1Min == 2) inProcTimeTransPlus1Min <- 1
      
      if(sum(names(trainData[[min(nTrans, iTrans + 1)]]) == 'delt1PayTimeTrans') == 1){
        delt1PayTimeTransPlus1Max <- max(as.numeric(as.character(trainData[[min(nTrans, iTrans + 1)]][, delt1PayTimeTrans])))
        delt1PayTimeTransPlus1Min <- min(as.numeric(as.character(trainData[[min(nTrans, iTrans + 1)]][, delt1PayTimeTrans])))
      }
      if(sum(names(trainData[[min(nTrans, iTrans + 1)]]) == 'delt2PayTimeTrans') == 1){
        delt2PayTimeTransPlus1Max <- max(as.numeric(as.character(trainData[[min(nTrans, iTrans + 1)]][, delt2PayTimeTrans])))
        delt2PayTimeTransPlus1Min <- min(as.numeric(as.character(trainData[[min(nTrans, iTrans + 1)]][, delt2PayTimeTrans])))
      }
      if(sum(names(trainData[[min(nTrans, iTrans + 1)]]) == 'deltRepTrans') == 1){
        deltRepTransPlus1Max <- max(as.numeric(as.character(trainData[[min(nTrans, iTrans + 1)]][, deltRepTrans])))
        deltRepTransPlus1Min <- min(as.numeric(as.character(trainData[[min(nTrans, iTrans + 1)]][, deltRepTrans])))
      }
      
      count <- 1
      
      iPol <- names(predSim[[iTrans]])[1]
      
      for(iPol in names(predSim[[iTrans]])){
        
        if(count %% 1000 == 0) print(count)
        
        nOpen <- sum(predSim[[iTrans]][iPol][[1]]$transMN == 'P')
        
        if(nOpen > 0){
          
          namesDT <- names(trainData[[min(nTrans, iTrans + 1)]])
          cumDeltNames <- namesDT[grep('cumDelt', namesDT)]
          namesDT <- namesDT[!namesDT %in% cumDeltNames]
          deltRepNames <- namesDT[grep('deltRep', namesDT)]
          namesDT <- namesDT[!namesDT %in% deltRepNames]
          deltPayNames <- namesDT[grep('delt', namesDT)]
          namesDT <- namesDT[!namesDT %in% deltPayNames]
          namesDT <- c(namesDT, deltRepNames)
          
          predSim[[iTrans + 1]][iPol][[1]] <- predSim[[iTrans]][iPol][[1]][transMN == 'P', ]
          predSim[[iTrans + 1]][iPol][[1]][, bookDate := bookDate + simPer*perLen]
          predSim[[iTrans + 1]][iPol][[1]][, finYear := year(bookDate)]
          
          if(sum(cumDeltNames == 'cumDelt2Pay') != 0){
            
            predSim[[iTrans + 1]][iPol][[1]][, cumDelt2Pay := cumDelt1Pay]
            if(!is.null(timeSplits[[min(nTrans, iTrans + 1)]])){
              selEl <- which(laply(timeSplits[[min(nTrans, iTrans + 1)]], function(xx) grepl('cumDelt2Pay', names(xx)[1])))
              splits <- list(timeSplits[[min(nTrans, iTrans + 1)]][[selEl]])
              names(splits)[1] <- 'cumDelt2Pay'
              
              transform2BinnedVar(predSim[[iTrans + 1]][iPol][[1]], splits)
            }
          }
          
          if(sum(cumDeltNames == 'cumDelt1Pay') != 0){
            if(sum(names(predSim[[iTrans + 1]][iPol][[1]]) == 'cumDelt1Pay') == 1){
              predSim[[iTrans + 1]][iPol][[1]][, cumDelt1Pay := outComeT + cumDelt1Pay]
            } else {
              predSim[[iTrans + 1]][iPol][[1]][, cumDelt1Pay := outComeT]
            }
            
            if(!(is.null(timeSplits[[min(nTrans, iTrans + 1)]]))){
              selEl <- which(laply(timeSplits[[min(nTrans, iTrans + 1)]], function(xx) grepl('cumDelt1Pay', names(xx)[1])))
              splits <- list(timeSplits[[min(nTrans, iTrans + 1)]][[selEl]])
              names(splits)[1] <- 'cumDelt1Pay'
              
              transform2BinnedVar(predSim[[iTrans + 1]][iPol][[1]], splits)
            }
          }
          
          if(sum(deltPayNames == 'delt1Pay') != 0){ #changed 040520
            predSim[[iTrans + 1]][iPol][[1]][, delt1Pay := outComeT] #changed 040520
            if(!is.null(timeSplits[[min(nTrans, iTrans + 1)]])){
              selEl <- which(laply(timeSplits[[min(nTrans, iTrans + 1)]], function(xx) grepl('delt1Pay', names(xx)[1]))) #changed 040520
              if(length(selEl) != 0){ #changed 180520
                splits <- list(timeSplits[[min(nTrans, iTrans + 1)]][[selEl]]) #changed 040520
                names(splits)[1] <- 'delt1Pay' #changed 040520
                transform2BinnedVar(predSim[[iTrans + 1]][iPol][[1]], splits) #changed 040520
              } #changed 180520
            }
          } #changed 040520
          
          if(sum(deltPayNames == 'delt2Pay') != 0){ #changed 040520
            predSim[[iTrans + 1]][iPol][[1]][, delt2Pay := outComeT] #changed 040520
            selEl <- which(laply(timeSplits[[min(nTrans, iTrans + 1)]], function(xx) grepl('delt2Pay', names(xx)[1]))) #changed 040520
            if(length(selEl) != 0){ #changed 180520
              splits <- list(timeSplits[[min(nTrans, iTrans + 1)]][[selEl]]) #changed 040520
              names(splits)[1] <- 'delt2Pay' #changed 040520
              transform2BinnedVar(predSim[[iTrans + 1]][iPol][[1]], splits) #changed 040520
            } #changed 180520
          } #changed 040520
          
          if(sum(deltPayNames == 'delt1PayTimeTrans') == 1){
            predSim[[iTrans + 1]][iPol][[1]][, delt1PayTimeTrans := inStateTimeTrans]
          }
          if(sum(deltPayNames == 'delt2PayTimeTrans') == 1){
            predSim[[iTrans + 1]][iPol][[1]][, delt2PayTimeTrans := delt1PayTimeTrans]
          }
          
          predSim[[iTrans + 1]][iPol][[1]][, inProcTimeTrans := min(max(c(inProcTimeTrans, simPer)), inProcTimeTransPlus1Max)]
          predSim[[iTrans + 1]][iPol][[1]][, inProcTimeTrans := max(inProcTimeTrans, inProcTimeTransPlus1Min)]
          if(sum(deltPayNames == 'delt1PayTimeTrans') == 1){
            predSim[[iTrans + 1]][iPol][[1]][, delt1PayTimeTrans := min(delt1PayTimeTrans, delt1PayTimeTransPlus1Max)]
            predSim[[iTrans + 1]][iPol][[1]][, delt1PayTimeTrans := max(delt1PayTimeTrans, delt1PayTimeTransPlus1Min)]
          }
          if(sum(deltPayNames == 'delt2PayTimeTrans') == 1){
            predSim[[iTrans + 1]][iPol][[1]][, delt2PayTimeTrans := min(delt2PayTimeTrans, delt2PayTimeTransPlus1Max)]
            predSim[[iTrans + 1]][iPol][[1]][, delt2PayTimeTrans := max(delt2PayTimeTrans, delt2PayTimeTransPlus1Min)]
          }
          if(sum(deltRepNames == 'deltRepTrans') == 1){
            asNumericDT(predSim[[iTrans + 1]][iPol][[1]], 'deltRepTrans')
            predSim[[iTrans + 1]][iPol][[1]][, deltRepTrans := min(deltRepTrans, deltRepTransPlus1Max)]
            predSim[[iTrans + 1]][iPol][[1]][, deltRepTrans := max(deltRepTrans, deltRepTransPlus1Min)]
            asFactorDT(predSim[[iTrans + 1]][iPol][[1]], 'deltRepTrans')
          }
          
          predSim[[iTrans + 1]][iPol][[1]][, simPer := NULL]
          predSim[[iTrans + 1]][iPol][[1]]$inStateTimeTrans <- 1
          predSim[[iTrans + 1]][iPol][[1]]$transMN <- 'N'
          predSim[[iTrans + 1]][iPol][[1]]$outComeT <- 0.0
          
          if(nrow(addDT) == 0){
            addDT <- predSim[[iTrans + 1]][iPol][[1]]
          } else {
            addDT <- rbind(addDT, predSim[[iTrans + 1]][iPol][[1]],fill=T)
          }
          nSim[[iTrans + 1]][iPol] <- nOpen
        }
        
        count <- count + 1
      }
      
      if(nrow(addDT) == 0){
        goOn <- FALSE
      } else {
        iTrans <- iTrans + 1
      }
    }
    return(list(predSim = predSim, nSim = nSim))
  }
}

timeFit2 <- function(inputDT, nMinMod, nMinLev, nTimesParams, nMaxNoMod, notInModVars = NULL, intVars = NULL, nSamp = 10000, nIterMax = 1000, verbose = TRUE, verboseConv = FALSE, RF = FALSE){ #changed 150420
  # Check input parameters and certain variables in 'inputDT'.
  if(!is.list(inputDT)) stop("The 'inputDT' argument needs to be a list of data.tables objects.")
  if(!is.null(notInModVars)) checkCharVec(list(notInModVars))
  if(!is.null(intVars)) checkCharVec(list(intVars))
  checkLogicVec(list(verbose, verboseConv))
  checkLength(list(verbose, nSamp, nIterMax, verboseConv), c(1, 1, 1, 1))
  checkWholeNumb(list(nMinMod, nMinLev, nTimesParams, nMaxNoMod))
  checkLength(list(nMinMod, nMinLev, nTimesParams, nMaxNoMod), 1)
  checkRanges(list(nSamp, nIterMax), list(c('>', 0), c('>', 0)))
  checkRanges(list(nMinMod, nMinLev, nTimesParams, nMaxNoMod), list(c('>', 0), c('>', 0), c('>', 0), c('>', 0)))
  llply(inputDT, function(xx) checkDT(xx, c('inStateTimeTrans', 'transType', 'inProcTimeTrans', 'transStat')))
  result <- list()
  
  for(iList in 1:length(inputDT)){
    
    result[[iList]] <- list()
    
    if(verbose) print(paste0('Fitting transition ', iList))
    if(verbose) print('>>> Prep phase')
    # Constructing the default formula, expressed by the variable 'form'.
    if(length(unique(inputDT[[iList]]$inStateTimeTrans)) == 1){
      form <- as.formula('transMN ~ 1')
    } else {
      form <- as.formula('transMN ~ inStateTimeTrans')
    }
    
    # The formula needs to be adapted in case that there are more than 'nMinMod' observations and in case that there are 'modVars'.
    if(nrow(inputDT[[iList]]) > nMinMod & length(modVars) > 0){
      
      # This gives 'deltNames': the names of the variables of 'inputDT[[iList]]' with 'delt' in their name.
      deltNames <- names(inputDT[[iList]])[grep('delt', names(inputDT[[iList]]))]
      
      # This gives 'payNames': the names of the variables of 'inputDT[[iList]]' with 'delt' and 'Pay', but without 'Time' in their name.
      payNames <- deltNames[grep('Pay', deltNames)]
      payNames <- payNames[!grepl('Time', payNames)]
      # payNames <- payNames[!grepl('Grouped', payNames)]
      
      # This gives 'deltNoPayNames': the names of the variables of 'inputDT[[iList]]' with 'delt', but without 'Pay' in their name.
      deltNoPayNames <- deltNames[!(deltNames %in% payNames)]
      
      indVars <- !names(inputDT[[iList]]) %in% c('inStateTimeTrans', 'transType', 'inProcTimeTrans', 'transStat', 'transMN', 'polNumb', 'inStateTimeMax', 'bookDate', 'finYear', 'outComeT', 'delt1Pay', 'delt2Pay', 'cumDelt1Pay', 'cumDelt2Pay', payNames, deltNoPayNames)
      modVars <- names(inputDT[[iList]])[indVars]
      
      if(isFactorDT(inputDT[[iList]], 'inStateTimeTrans')) asNumericDT(inputDT[[iList]], 'inStateTimeTrans') #changed 050520
      if(isFactorDT(inputDT[[iList]], 'inProcTimeTrans')) asNumericDT(inputDT[[iList]], 'inProcTimeTrans') #changed 050520
      catMerge(inputDT[[iList]], 'inStateTimeTrans', nMinLev, nMinMod) #changed 050520
      catMerge(inputDT[[iList]], 'inProcTimeTrans', nMinLev, nMinMod) #changed 050520
      if(sum(names(inputDT[[iList]]) == 'inStateTimeTransTrans')){ #changed 050520
        inputDT[[iList]][, inStateTimeTrans := NULL] #changed 050520
        setnames(inputDT[[iList]], 'inStateTimeTransTrans', 'inStateTimeTrans') #changed 050520
      } #changed 050520
      if(sum(names(inputDT[[iList]]) == 'inProcTimeTransTrans')){ #changed 050520
        inputDT[[iList]][, inProcTimeTrans := NULL] #changed 050520
        setnames(inputDT[[iList]], 'inProcTimeTransTrans', 'inProcTimeTrans') #changed 050520
      } #changed 050520
      if(!isFactorDT(inputDT[[iList]], 'inStateTimeTrans')) asFactorDT(inputDT[[iList]], 'inStateTimeTrans') #changed 050520
      if(!isFactorDT(inputDT[[iList]], 'inProcTimeTrans')) asFactorDT(inputDT[[iList]], 'inProcTimeTrans') #changed 050520
      
      # Computing the number of parameters (to gauge the model complexity), expresssed by the variable 'nParamTot'.
      print(indVars)
      if(sum(indVars)>0){
      isFact <- isFactorDT(inputDT[[iList]][, .SD, .SDcol = indVars])
      }
      else{
        isFact <- as.vector(indVars)
      }
      nParam <- rep(1, length(modVars))
      allVarsLevs <- list()
      length(allVarsLevs) <- length(modVars) + 1
      names(allVarsLevs) <- c(modVars, 'inStateTimeTrans')
      nIntParams <- 0
      selInt <- c()
      print(modVars)
      if(length(modVars)>0){
        for(iVar in 1:length(modVars)){
          if(isFact[iVar]){
          levs <- extractLevelDT(inputDT[[iList]], modVars[iVar])[[1]]
          refLev <- extractRefLevelDT(inputDT[[iList]], modVars[iVar])[[1]]
          levs <- levs[levs != refLev]
          nParam[iVar] <- length(levs)
          allVarsLevs[[iVar]] <- levs
            if(sum(intVars %in% modVars[iVar])){
            nIntParams <- nIntParams + length(levs)
            selInt <- c(selInt, modVars[iVar])
          }
        } else {
          if(sum(intVars %in% modVars[iVar])){
            nIntParams <- nIntParams + 1
            selInt <- c(selInt, modVars[iVar])
          }
        }
      }
      }
      else{
        modVars <- NULL
        isFact <- NULL
      }
      inStateLevs <- extractLevelDT(inputDT[[iList]], 'inStateTimeTrans')[[1]]
      refInStateLev <- extractRefLevelDT(inputDT[[iList]], 'inStateTimeTrans')[[1]]
      inStateLevs <- inStateLevs[inStateLevs != refInStateLev]
      allVarsLevs$inStateTimeTrans <- inStateLevs
      intLev <- length(inStateLevs)
      
      nParamTotMax <- intLev + sum(nParam) + sum(nIntParams*intLev) + 1
      nSamp <- pmax(nSamp, nParamTotMax*nTimesParams)
      
      # Constructing the formula, expressed by the variable 'form'.
      if(nrow(inputDT[[iList]]) > nParamTotMax*nTimesParams){
        explVar <- modVars
        if(length(unique(inputDT[[iList]]$inStateTimeTrans)) != 1){
          #explVar <- c(modVars, payNames, deltNoPayNames, 'inStateTimeTrans', 'inProcTimeTrans') #interactions with inStateTimeTrans are added to relax the PH assumption
          #print(explVar)
          #explVar <- explVar[!explVar %in% c('delt1Pay', 'delt2Pay', 'cumDelt1Pay', 'cumDelt2Pay')]
          if(iList == 1){
            explVar <- c( 'inProcTimeTrans', 'fastRep','deltRepTrans')
            
          }
          else if(iList == 2){
            explVar <- c('inProcTimeTrans','cumDelt1Pay', 'fastRep','deltRepTrans')  
            
          }
          else{
            explVar <- c('inProcTimeTrans','cumDelt1Pay','delt1Pay')
            
          }
          if(!is.null(notInModVars)) explVar <- explVar[!(explVar %in% notInModVars)]
          # if(!is.null(intVars)) explVar <- c(explVar, paste(c(intVars[intVars %in% names(inputDT[[iList]])]), 'inStateTimeTrans', sep = '*'))
          if(length(selInt) > 0) explVar <- c(explVar, paste(selInt, 'inStateTimeTrans', sep = '*'))
        }
        
        if(iList == 1){
          explVar <- c( 'inProcTimeTrans', 'fastRep','deltRepTrans')
          
        }
        else if(iList == 2){
          explVar <- c('inProcTimeTrans','cumDelt1Pay', 'fastRep','deltRepTrans')  
          
        }
        else{
          explVar <- c('inProcTimeTrans','cumDelt1Pay','delt1Pay')
          
        }
        #explVar <- c( 'inProcTimeTrans')
        form <- as.formula(paste('transMN ~', paste(explVar, collapse = " + ")))
      } else {
        form <- as.formula('transMN ~ 1')
        if(length(unique(inputDT[[iList]]$inStateTimeTrans)) != 1){
          form <- as.formula('transMN ~ inStateTimeTrans') #just the baseline hazard
        }
      }
    } else {
      modVars <- NULL
      isFact <- NULL
    }
    
    # Fitting the multinomial (MN) model or Random Forest (RF) model, if there are more than 'nMaxNoMod' observations.
    if(nrow(inputDT[[iList]]) > nMaxNoMod){
      if(verbose) print('>>> Fitting phase')
      if(!RF){ #changed 150420
        print(form)
        print(modVars)
        result[[iList]]$fitTime <- fitMN(inputDT[[iList]], form, modVars, isFact, nSamp, nIterMax, verboseConv)
      } else { #changed 150420
        result[[iList]]$fitTime <- fitRF(inputDT[[iList]], form, modVars, isFact, nSamp, 128, 300, verboseConv) #changed 150420
      } #changed 150420
      
      result[[iList]]$trainDT <- inputDT[[iList]]
    }
  }
  return(result)
}


timeFit3 <- function(inputDT, nMinMod, nMinLev, nTimesParams, nMaxNoMod, notInModVars = NULL, intVars = NULL, nSamp = 10000, nIterMax = 1000, verbose = TRUE, verboseConv = FALSE, RF = FALSE){ #changed 150420
  # Check input parameters and certain variables in 'inputDT'.
  if(!is.list(inputDT)) stop("The 'inputDT' argument needs to be a list of data.tables objects.")
  if(!is.null(notInModVars)) checkCharVec(list(notInModVars))
  if(!is.null(intVars)) checkCharVec(list(intVars))
  checkLogicVec(list(verbose, verboseConv))
  checkLength(list(verbose, nSamp, nIterMax, verboseConv), c(1, 1, 1, 1))
  checkWholeNumb(list(nMinMod, nMinLev, nTimesParams, nMaxNoMod))
  checkLength(list(nMinMod, nMinLev, nTimesParams, nMaxNoMod), 1)
  checkRanges(list(nSamp, nIterMax), list(c('>', 0), c('>', 0)))
  checkRanges(list(nMinMod, nMinLev, nTimesParams, nMaxNoMod), list(c('>', 0), c('>', 0), c('>', 0), c('>', 0)))
  llply(inputDT, function(xx) checkDT(xx, c('inStateTimeTrans', 'transType', 'inProcTimeTrans', 'transStat')))
  result <- list()
  
  for(iList in 1:length(inputDT)){
    
    result[[iList]] <- list()
    
    if(verbose) print(paste0('Fitting transition ', iList))
    if(verbose) print('>>> Prep phase')
    # Constructing the default formula, expressed by the variable 'form'.
    if(length(unique(inputDT[[iList]]$inStateTimeTrans)) == 1){
      form <- as.formula('transMN ~ 1')
    } else {
      form <- as.formula('transMN ~ inStateTimeTrans')
    }
    
    # The formula needs to be adapted in case that there are more than 'nMinMod' observations and in case that there are 'modVars'.
    if(nrow(inputDT[[iList]]) > nMinMod & length(modVars) > 0){
      
      # This gives 'deltNames': the names of the variables of 'inputDT[[iList]]' with 'delt' in their name.
      deltNames <- names(inputDT[[iList]])[grep('delt', names(inputDT[[iList]]))]
      
      # This gives 'payNames': the names of the variables of 'inputDT[[iList]]' with 'delt' and 'Pay', but without 'Time' in their name.
      payNames <- deltNames[grep('Pay', deltNames)]
      payNames <- payNames[!grepl('Time', payNames)]
      # payNames <- payNames[!grepl('Grouped', payNames)]
      
      # This gives 'deltNoPayNames': the names of the variables of 'inputDT[[iList]]' with 'delt', but without 'Pay' in their name.
      deltNoPayNames <- deltNames[!(deltNames %in% payNames)]
      
      indVars <- !names(inputDT[[iList]]) %in% c('inStateTimeTrans', 'transType', 'inProcTimeTrans', 'transStat', 'transMN', 'polNumb', 'inStateTimeMax', 'bookDate', 'finYear', 'outComeT', 'delt1Pay', 'delt2Pay', 'cumDelt1Pay', 'cumDelt2Pay', payNames, deltNoPayNames)
      modVars <- names(inputDT[[iList]])[indVars]
      
      if(isFactorDT(inputDT[[iList]], 'inStateTimeTrans')) asNumericDT(inputDT[[iList]], 'inStateTimeTrans') #changed 050520
      if(isFactorDT(inputDT[[iList]], 'inProcTimeTrans')) asNumericDT(inputDT[[iList]], 'inProcTimeTrans') #changed 050520
      catMerge(inputDT[[iList]], 'inStateTimeTrans', nMinLev, nMinMod) #changed 050520
      catMerge(inputDT[[iList]], 'inProcTimeTrans', nMinLev, nMinMod) #changed 050520
      if(sum(names(inputDT[[iList]]) == 'inStateTimeTransTrans')){ #changed 050520
        inputDT[[iList]][, inStateTimeTrans := NULL] #changed 050520
        setnames(inputDT[[iList]], 'inStateTimeTransTrans', 'inStateTimeTrans') #changed 050520
      } #changed 050520
      if(sum(names(inputDT[[iList]]) == 'inProcTimeTransTrans')){ #changed 050520
        inputDT[[iList]][, inProcTimeTrans := NULL] #changed 050520
        setnames(inputDT[[iList]], 'inProcTimeTransTrans', 'inProcTimeTrans') #changed 050520
      } #changed 050520
      if(!isFactorDT(inputDT[[iList]], 'inStateTimeTrans')) asFactorDT(inputDT[[iList]], 'inStateTimeTrans') #changed 050520
      if(!isFactorDT(inputDT[[iList]], 'inProcTimeTrans')) asFactorDT(inputDT[[iList]], 'inProcTimeTrans') #changed 050520
      
      # Computing the number of parameters (to gauge the model complexity), expresssed by the variable 'nParamTot'.
      isFact <- isFactorDT(inputDT[[iList]][, .SD, .SDcol = indVars])
      nParam <- rep(1, length(modVars))
      allVarsLevs <- list()
      length(allVarsLevs) <- length(modVars) + 1
      names(allVarsLevs) <- c(modVars, 'inStateTimeTrans')
      nIntParams <- 0
      selInt <- c()
      for(iVar in 1:length(modVars)){
        if(isFact[iVar]){
          levs <- extractLevelDT(inputDT[[iList]], modVars[iVar])[[1]]
          refLev <- extractRefLevelDT(inputDT[[iList]], modVars[iVar])[[1]]
          levs <- levs[levs != refLev]
          nParam[iVar] <- length(levs)
          allVarsLevs[[iVar]] <- levs
          if(sum(intVars %in% modVars[iVar])){
            nIntParams <- nIntParams + length(levs)
            selInt <- c(selInt, modVars[iVar])
          }
        } else {
          if(sum(intVars %in% modVars[iVar])){
            nIntParams <- nIntParams + 1
            selInt <- c(selInt, modVars[iVar])
          }
        }
      }
      inStateLevs <- extractLevelDT(inputDT[[iList]], 'inStateTimeTrans')[[1]]
      refInStateLev <- extractRefLevelDT(inputDT[[iList]], 'inStateTimeTrans')[[1]]
      inStateLevs <- inStateLevs[inStateLevs != refInStateLev]
      allVarsLevs$inStateTimeTrans <- inStateLevs
      intLev <- length(inStateLevs)
      
      nParamTotMax <- intLev + sum(nParam) + sum(nIntParams*intLev) + 1
      nSamp <- pmax(nSamp, nParamTotMax*nTimesParams)
      
      # Constructing the formula, expressed by the variable 'form'.
      if(nrow(inputDT[[iList]]) > nParamTotMax*nTimesParams){
        explVar <- modVars
        if(length(unique(inputDT[[iList]]$inStateTimeTrans)) != 1){
          #explVar <- c(modVars, payNames, deltNoPayNames, 'inStateTimeTrans', 'inProcTimeTrans') #interactions with inStateTimeTrans are added to relax the PH assumption
          #print(explVar)
          #explVar <- explVar[!explVar %in% c('delt1Pay', 'delt2Pay', 'cumDelt1Pay', 'cumDelt2Pay')]
          if(iList == 1){
            explVar <- c( 'inProcTimeTrans', "deltRepTrans", "fastRep")
          }
          else if(iList == 2){
            explVar <- c('inStateTimeTrans', 'inProcTimeTrans',"deltRepTrans", "fastRep", "cumDelt1PayGrouped")  
          }
          else{
            explVar <- c('inStateTimeTrans', 'inProcTimeTrans',"deltRepTrans", "fastRep", "cumDelt1PayGrouped", "delt1PayGrouped")  
          }
          if(!is.null(notInModVars)) explVar <- explVar[!(explVar %in% notInModVars)]
          # if(!is.null(intVars)) explVar <- c(explVar, paste(c(intVars[intVars %in% names(inputDT[[iList]])]), 'inStateTimeTrans', sep = '*'))
          if(length(selInt) > 0) explVar <- c(explVar, paste(selInt, 'inStateTimeTrans', sep = '*'))
        }
        form <- as.formula(paste('transMN ~', paste(explVar, collapse = " + ")))
      } else {
        form <- as.formula('transMN ~ 1')
        if(length(unique(inputDT[[iList]]$inStateTimeTrans)) != 1){
          form <- as.formula('transMN ~ inStateTimeTrans') #just the baseline hazard
        }
      }
    } else {
      modVars <- NULL
      isFact <- NULL
    }
    
    # Fitting the multinomial (MN) model or Random Forest (RF) model, if there are more than 'nMaxNoMod' observations.
    if(nrow(inputDT[[iList]]) > nMaxNoMod){
      if(verbose) print('>>> Fitting phase')
      if(!RF){ #changed 150420
        print(form)
        print(modVars)
        result[[iList]]$fitTime <- fitMN(inputDT[[iList]], form, modVars, isFact, nSamp, nIterMax, verboseConv)
      } else { #changed 150420
        result[[iList]]$fitTime <- fitRF(inputDT[[iList]], form, modVars, isFact, nSamp, 128, 300, verboseConv) #changed 150420
      } #changed 150420
      
      result[[iList]]$trainDT <- inputDT[[iList]]
    }
  }
  return(result)
}



calibJordy <- function(BECalib,obsResCalib, BETest,nBoots = 50){
  
  
  totResBoot <-list()
  indivResBoot <- list()
  meanPredsTest <- unlist(lapply(BETest,mean))
  meanPredsCalib <- unlist(lapply(BECalib,mean))
  
  #obsPay <- which(obsResCalib != 0)
  
  resTest <- QuantifQuantileMean(meanPredsCalib, obsResCalib, x = meanPredsTest, alpha = seq(.01, .99,.01), testN = 200)
  
  sampsQR <- resTest$fitted.values
  testMeanPred <- sampsQR[1, ]
  totRes <- sum(testMeanPred)
  print(paste0('total Reserve: ',totRes))
  
  
  #perform Bootstrap to get a predictive distribution of the total reserve
  for(bootRep in 1:nBoots){
    print(paste0("boot repetition: ", bootRep))
    bootInds <- sample(length(obsResCalib),replace=T)
    meanPredsCalibBoot <- unlist(lapply(BECalib,mean))[bootInds]
    obsResCalibBoot <- obsResCalib[bootInds]
    resTestBoot <- QuantifQuantileMean(meanPredsCalibBoot, obsResCalibBoot, x =meanPredsTest, alpha = seq(.01, .99,.01), testN = 200)
    
    sampsQRBoot <- resTestBoot$fitted.values
    testMeanPredBoot <- sampsQRBoot[1, ]
    indivResBoot[[bootRep]] <- testMeanPredBoot
    totResBoot[[bootRep]] <- sum(testMeanPredBoot)
    
  }
  
  return(list(indivResNoBoot =testMeanPred, totResNoBoot = totRes, indivResBoot = indivResBoot, totResBoot = totResBoot ))
  
  
}

fitSplicedPareto2 <- function(inputDT,form1,form2,form3,splitPoints, verboseConv = T){
  #TODO: document input arguments; assert that the length of splitPoints is 3;  change the ifelses by something better 
  meanVals <- list()
  result <- list()
  
  
  for(iPay in 1:length(inputDT)){
    
    if(iPay  == 1){
      form <- form1
    }
    else if(iPay == 2){
      form <- form2
    }
    else{
      form <- form3  
    }
    if(verboseConv) print(paste0('>>> Fitting payment distr ', iPay))
    
    result[[iPay]] <- list()
    payData <- copy(inputDT[[iPay]]) 
    
    #select only lines where there was a payment
    payData <- payData[(transMN %in% c('P', 'TP','PI')), ]
    
    # bin necessary variables 
    
    # if(isFactorDT(payData, 'inStateTimeTrans')) asNumericDT(payData, 'inStateTimeTrans') #changed 050520
    # if(isFactorDT(payData, 'inProcTimeTrans')) asNumericDT(payData, 'inProcTimeTrans') #changed 050520
    # catMerge(payData, 'inStateTimeTrans', nMinLev, nMinMod) #changed 050520
    # catMerge(payData, 'inProcTimeTrans', nMinLev, nMinMod) #changed 050520
    # if(sum(names(payData) == 'inStateTimeTransTrans')){ #changed 050520
    #   payData[, inStateTimeTrans := NULL] #changed 050520
    #   setnames(payData, 'inStateTimeTransTrans', 'inStateTimeTrans') #changed 050520
    # } #changed 050520
    # if(sum(names(payData) == 'inProcTimeTransTrans')){ #changed 050520
    #   payData[, inProcTimeTrans := NULL] #changed 050520
    #   setnames(payData, 'inProcTimeTransTrans', 'inProcTimeTrans') #changed 050520
    # } #changed 050520
    # if(!isFactorDT(payData, 'inStateTimeTrans')) asFactorDT(payData, 'inStateTimeTrans') #changed 050520
    # if(!isFactorDT(payData, 'inProcTimeTrans')) asFactorDT(payData, 'inProcTimeTrans') #changed 050520
    # 
    # Define split points 
    
    
    print(nrow(payData[outComeT<0,]))
    print(nrow(payData[outComeT>0,]))
    normalDo <- function(payData){
      error_ind <- 0
      threshold_P <- tea::ggplot(sample(payData[outComeT>0, outComeT],min(nrow(payData[outComeT>0,]),nrow(payData[outComeT>0,]))))[["threshold"]][1]
      return(list(error_ind=error_ind,threshold_P=threshold_P))
      
    }
      exceptionalDo <- function(err){
        error_ind <- 1
        #threshold_P <- tea::ggplot(sample(payData[outComeT>0, outComeT],min(5000,nrow(payData[outComeT>0,]))))[["threshold"]][1]
        threshold_P <- 5000
        return(list(error_ind=error_ind,threshold_P=threshold_P))
      }
    new_values <- tryCatch(normalDo(payData),
      error= exceptionalDo)
    
    error_ind <-  new_values$error_ind
    threshold_P <- new_values$threshold_P
    
    
    print(nrow(payData[outComeT<0,]))
    if(nrow(payData[outComeT<0,])>0){
      threshold_N <- -tea::ggplot(-1*payData[outComeT<0, outComeT])[["threshold"]][1]
      uniqueSplitPoints <- sort(unique(c(splitPoints,threshold_P,threshold_N)))
    }
    else{
      uniqueSplitPoints <- sort(unique(c(splitPoints,threshold_P,-1e9)))
    }
    
    
    
    
    
    fitDatList <- list()
    for(splitInd in 1:(length(uniqueSplitPoints)-1)){
      fitDatList[[splitInd]] <- copy(payData[outComeT <= uniqueSplitPoints[splitInd + 1] & outComeT >= uniqueSplitPoints[splitInd], ])
      
      if (splitInd == 1){## negative pareto fit 
        if(nrow(fitDatList[[splitInd]])>30){
          print(paste0('lower limit N1 ',uniqueSplitPoints[splitInd], ' upper limit ',uniqueSplitPoints[splitInd + 1]  ))
        gpdN <- ismev::gpd.fit(xdat = -1*fitDatList[[splitInd]][,outComeT] , threshold = threshold_N, show = F)
        meanGPDN <- -1* (abs((gpdN[["mle"]][1]/(1-gpdN[["mle"]][2])) )-threshold_N)# to check if shape parameter is greater than one
        par1N1 <- gpdN[["mle"]][1]
        par2N1 <- gpdN[["mle"]][2]
        
        }
        else{
          meanGPDN <- 0
        }
      }
      
      else if(splitInd == 2){## negative normal fit 
        
        if(nrow(fitDatList[[splitInd]])>30){
          print(paste0('lower limit N2 ',uniqueSplitPoints[splitInd], ' upper limit ',uniqueSplitPoints[splitInd + 1]  ))
          premeanN<- mean(fitDatList[[splitInd]][,outComeT])
          preStd <- sd(fitDatList[[splitInd]][,outComeT])
          xi <- ((fitDatList[[splitInd]][,outComeT])- premeanN)/preStd
          a <- uniqueSplitPoints[splitInd]
          b <- uniqueSplitPoints[splitInd + 1]
          bet <- (b - premeanN)/preStd
          alph <- (a - premeanN)/preStd
          Z <-     pnorm(bet) - pnorm(alph)
          meanN <- premeanN + ((dnorm(alph)-dnorm(bet))*preStd/Z)
          stdN <- sqrt((preStd^2)*( (1) + (((alph *dnorm(alph)) - (bet*dnorm(bet)))/(Z)) -(((dnorm(alph) - dnorm(bet))/(Z))^2)))
          
          par1N2 <- premeanN
          par2N2 <- preStd
          
          
        }
        else{
          meanN <- 0
        }
        
      }
      
      else if(splitInd == 3){## positive normal fit
        # if(error_ind){
        #   premeanP<- mean(fitDatList[[splitInd]][,outComeT])
        #   preStd <- sd(fitDatList[[splitInd]][,outComeT])
        #   xi <- ((fitDatList[[splitInd]][,outComeT])- premeanP)/preStd
        #   a <- uniqueSplitPoints[splitInd]
        #   b <- 1e9
        #   bet <- (b - premeanP)/preStd
        #   alph <- (a - premeanP)/preStd
        #   Z <-     pnorm(bet) - pnorm(alph)
        #   meanP <- premeanP + ((dnorm(alph)-dnorm(bet))*preStd/Z)
        #   
        #   par1P1 <- premeanP
        #   par2P1 <- preStd
        #   
        # }
        #else{
        print(paste0('lower limit P1 ',uniqueSplitPoints[splitInd], ' upper limit ',uniqueSplitPoints[splitInd + 1]  ))
        premeanP<- mean(fitDatList[[splitInd]][,outComeT])
        preStd <- sd(fitDatList[[splitInd]][,outComeT])
        xi <- ((fitDatList[[splitInd]][,outComeT])- premeanP)/preStd
        a <- uniqueSplitPoints[splitInd]
        b <- uniqueSplitPoints[splitInd + 1]
        bet <- (b - premeanP)/preStd
        alph <- (a - premeanP)/preStd
        Z <-     pnorm(bet) - pnorm(alph)
        meanP <- premeanP + ((dnorm(alph)-dnorm(bet))*preStd/Z)
        par1P1 <- premeanP
        par2P1 <- preStd
        #}
      }
      else if(splitInd == 4){## positive pareto fit 
        # if(error_ind){# there is no suitable pareto fit, the mean is just the mean of all the positive data 
        #   
        #   premeanP<- mean(fitDatList[[splitInd-1]][,outComeT])
        #   preStd <- sd(fitDatList[[splitInd-1]][,outComeT])
        #   xi <- ((fitDatList[[splitInd-1]][,outComeT])- premeanP)/preStd
        #   a <- uniqueSplitPoints[splitInd-1]
        #   b <- 1e9
        #   bet <- (b - premeanP)/preStd
        #   alph <- (a - premeanP)/preStd
        #   Z <-     pnorm(bet) - pnorm(alph)
        #   meanGPDP <- premeanP + ((dnorm(alph)-dnorm(bet))*preStd/Z)
        #   par1P2 <- premeanP
        #   par2P2 <- preStd
        #   }
        #else{
        print(paste0('lower limit P2 ',uniqueSplitPoints[splitInd], ' upper limit ',uniqueSplitPoints[splitInd + 1]  ))
          gpdP <- ismev::gpd.fit(xdat = fitDatList[[splitInd]][,outComeT] , threshold = threshold_P, show = F)
          meanGPDP <- abs(gpdP[["mle"]][1]/(1-gpdP[["mle"]][2]) ) + threshold_P # to check if shape parameter is greater than one
          par1P2 <- gpdP[["mle"]][1]
          par2P2 <- gpdP[["mle"]][2]
        #}
      }
      else{## function needs to be made better 
        
      }
      
    } 
    
    if(nrow(payData[outComeT<0,])>0){
      if(meanGPDN != 0){
      meanVecs <- c(meanGPDN,meanN, meanP, meanGPDP)
      par1Vec <- c(par1N1, par1N2, par1P1,par1P2)
      par2Vec <- c(par2N1, par2N2, par2P1,par2P2)
      }
      else{
        meanVecs <- c(meanN, meanP, meanGPDP)
        par1Vec <- c( par1N2, par1P1,par1P2)
        par2Vec <- c( par2N2, par2P1,par2P2)
        uniqueSplitPoints <- uniqueSplitPoints[uniqueSplitPoints> min(uniqueSplitPoints)]
      }
    }
    else{
      uniqueSplitPoints <- sort(unique(c(splitPoints,threshold_P)))
      uniqueSplitPoints <- uniqueSplitPoints[uniqueSplitPoints >= 0]
      meanVecs <- c(meanP, meanGPDP)
      par1Vec <- c(  par1P1,par1P2)
      par2Vec <- c(  par2P1,par2P2)
    }
    
    payData[, outComeTrans:= cut(outComeT,breaks = uniqueSplitPoints)]
    result[[iPay]]$fitPay <-   fitMN(payData, form, NULL, NULL, min(nrow(payData), 50000), 10000, T)
    result[[iPay]]$trainDT <- payData
    result[[iPay]]$form <- form
    result[[iPay]]$meanVecs <- meanVecs
    result[[iPay]]$par1 <- par1Vec
    result[[iPay]]$par2 <- par2Vec
    result[[iPay]]$uniqueSplitPoints <- uniqueSplitPoints
  }
  
  return(result)
  
  
}



fitSplicedPareto <- function(inputDT,form1,form2,form3,splitPoints, verboseConv = T){
  #TODO: document input arguments; assert that the length of splitPoints is 3;  change the ifelses by something better 
  meanVals <- list()
  result <- list()
  
  
  for(iPay in 1:length(inputDT)){
    
    if(iPay  == 1){
      form <- form1
    }
    else if(iPay == 2){
      form <- form2
    }
    else{
      form <- form3  
    }
    if(verboseConv) print(paste0('>>> Fitting payment distr ', iPay))
    
    result[[iPay]] <- list()
    payData <- copy(inputDT[[iPay]]) 
    
    #select only lines where there was a payment
    payData <- payData[(transMN %in% c('P', 'TP','PI')), ]
    
    # bin necessary variables 
    
    #if(isFactorDT(payData, 'inStateTimeTrans')) asNumericDT(payData, 'inStateTimeTrans') #changed 050520
    #if(isFactorDT(payData, 'inProcTimeTrans')) asNumericDT(payData, 'inProcTimeTrans') #changed 050520
    #catMerge(payData, 'inStateTimeTrans', nMinLev, nMinMod) #changed 050520
    #catMerge(payData, 'inProcTimeTrans', nMinLev, nMinMod) #changed 050520
    # if(sum(names(payData) == 'inStateTimeTransTrans')){ #changed 050520
    #   payData[, inStateTimeTrans := NULL] #changed 050520
    #   setnames(payData, 'inStateTimeTransTrans', 'inStateTimeTrans') #changed 050520
    # } #changed 050520
    # if(sum(names(payData) == 'inProcTimeTransTrans')){ #changed 050520
    #   payData[, inProcTimeTrans := NULL] #changed 050520
    #   setnames(payData, 'inProcTimeTransTrans', 'inProcTimeTrans') #changed 050520
    # } #changed 050520
    # if(!isFactorDT(payData, 'inStateTimeTrans')) asFactorDT(payData, 'inStateTimeTrans') #changed 050520
    # if(!isFactorDT(payData, 'inProcTimeTrans')) asFactorDT(payData, 'inProcTimeTrans') #changed 050520
    
    # Define split points 
    threshold_P <- tea::ggplot(payData[outComeT>0, outComeT])[["threshold"]][1]
    threshold_N <- -tea::ggplot(-1*payData[outComeT<0, outComeT])[["threshold"]][1]
    uniqueSplitPoints <- sort(unique(c(splitPoints,threshold_P,threshold_N)))
    
    
    
    
    
    fitDatList <- list()
    for(splitInd in 1:(length(uniqueSplitPoints)-1)){
      fitDatList[[splitInd]] <- copy(payData[outComeT <= uniqueSplitPoints[splitInd + 1] & outComeT >= uniqueSplitPoints[splitInd], ])
      
      if(splitInd == 1){## negative pareto fit 
        gpdN <- ismev::gpd.fit(xdat = -1*fitDatList[[splitInd]][,outComeT] , threshold = threshold_N, show = F)
        meanGPDN <- -1* (abs((gpdN[["mle"]][1]/(1-gpdN[["mle"]][2])) )-threshold_N)# to check if shape parameter is greater than one
      }
      
      else if(splitInd == 2){## negative normal fit 
        meanN<- mean(fitDatList[[splitInd]][,outComeT])
        
        
      }
      
      else if(splitInd == 3){## positive normal fit
        meanP<- mean(fitDatList[[splitInd]][,outComeT])
      }
      else if(splitInd == 4){## positive pareto fit 
        gpdP <- ismev::gpd.fit(xdat = fitDatList[[splitInd]][,outComeT] , threshold = threshold_P, show = F)
        meanGPDP <- abs(gpdP[["mle"]][1]/(1-gpdP[["mle"]][2]) ) + threshold_P # to check if shape parameter is greater than one
        
      }
      else{## function needs to be made better 
        
      }
      
    } 
    
    meanVecs <- c(meanGPDN,meanN, meanP, meanGPDP)
    
    payData[, outComeTrans:= cut(outComeT,breaks = uniqueSplitPoints)]
    result[[iPay]]$fitPay <-   fitMN(payData, form, NULL, NULL, min(nrow(payData), 50000), 10000, T)
    result[[iPay]]$trainDT <- payData
    result[[iPay]]$form <- form
    result[[iPay]]$meanVecs <- meanVecs
    
  }
  
  return(result)
  
  
}


predictSplicedPareto <- function(sampDT, finPayMods, listNr){
  
  trainDT <- finPayMods[[listNr]]$trainDT
  meanVecs <- finPayMods[[listNr]]$meanVecs
  
  if(nrow(sampDT) == 1){
    predProbs <- t(as.matrix(predict(finPayMods[[listNr]]$fitPay, newdata = sampDT, type = "probs")))
  } else {
    predProbs <- predict(finPayMods[[listNr]]$fitPay, newdata = sampDT, type = "probs")
  }
  
  if(length(meanVecs) == 2){
    predProbs <- as.matrix(cbind(1-predProbs, predProbs))
  }
  nSamps <- nrow(sampDT)
  
  sampsObs <- unname(rowSums(predProbs %*% diag(meanVecs)))
  
  return(sampsObs)
  
}

fitTruncNorm <- function(inputDT,form1,form2,form3,splitPoints, verboseConv = T){
  
  meanVals <- list()
  result <- list()
  
  
  for(iPay in 1:length(inputDT)){
    
    if(iPay  == 1){
      form <- form1
    }
    else if(iPay == 2){
      form <- form2
    }
    else{
      form <- form3  
    }
    if(verboseConv) print(paste0('>>> Fitting payment distr ', iPay))
    
    result[[iPay]] <- list()
    payData <- copy(inputDT[[iPay]]) 
    
    #select only lines where there was a payment
    payData <- payData[(transMN %in% c('P', 'TP','PI')), ]
    
    # bin necessary variables 
    
    # if(isFactorDT(payData, 'inStateTimeTrans')) asNumericDT(payData, 'inStateTimeTrans') #changed 050520
    # if(isFactorDT(payData, 'inProcTimeTrans')) asNumericDT(payData, 'inProcTimeTrans') #changed 050520
    # catMerge(payData, 'inStateTimeTrans', nMinLev, nMinMod) #changed 050520
    # catMerge(payData, 'inProcTimeTrans', nMinLev, nMinMod) #changed 050520
    # if(sum(names(payData) == 'inStateTimeTransTrans')){ #changed 050520
    #   payData[, inStateTimeTrans := NULL] #changed 050520
    #   setnames(payData, 'inStateTimeTransTrans', 'inStateTimeTrans') #changed 050520
    # } #changed 050520
    # if(sum(names(payData) == 'inProcTimeTransTrans')){ #changed 050520
    #   payData[, inProcTimeTrans := NULL] #changed 050520
    #   setnames(payData, 'inProcTimeTransTrans', 'inProcTimeTrans') #changed 050520
    # } #changed 050520
    # if(!isFactorDT(payData, 'inStateTimeTrans')) asFactorDT(payData, 'inStateTimeTrans') #changed 050520
    # if(!isFactorDT(payData, 'inProcTimeTrans')) asFactorDT(payData, 'inProcTimeTrans') #changed 050520
    # 
    # Define split points 
    
    
    print(nrow(payData[outComeT<0,]))
    print(nrow(payData[outComeT>0,]))
    #normalDo <- function(payData){
    #   error_ind <- 0
    #   threshold_P <- tea::ggplot(sample(payData[outComeT>0, outComeT],min(nrow(payData[outComeT>0,]),nrow(payData[outComeT>0,]))))[["threshold"]][1]
    #   return(list(error_ind=error_ind,threshold_P=threshold_P))
    #   
    # }
    # exceptionalDo <- function(err){
    #   error_ind <- 1
    #   threshold_P <- tea::ggplot(sample(payData[outComeT>0, outComeT],min(5000,nrow(payData[outComeT>0,]))))[["threshold"]][1]
    #   return(list(error_ind=error_ind,threshold_P=threshold_P))
    # }
    # new_values <- tryCatch(normalDo(payData),
    #                        error= exceptionalDo)
    # 
    # error_ind <-  new_values$error_ind
    # threshold_P <- new_values$threshold_P
    # 
    # 
    # print(nrow(payData[outComeT<0,]))
    # if(nrow(payData[outComeT<0,])>0){
    #   threshold_N <- -tea::ggplot(-1*payData[outComeT<0, outComeT])[["threshold"]][1]
    #   uniqueSplitPoints <- sort(unique(c(splitPoints,threshold_P,threshold_N)))
    # }
    # else{
    #   uniqueSplitPoints <- sort(unique(c(splitPoints,threshold_P,-1e9)))
    # }
      
    uniqueSplitPoints <- sort(unique(c(splitPoints)))
    
    
    
    
    
    
    fitDatList <- list()
    for(splitInd in 1:(length(uniqueSplitPoints)-1)){
      fitDatList[[splitInd]] <- copy(payData[outComeT <= uniqueSplitPoints[splitInd + 1] & outComeT >= uniqueSplitPoints[splitInd], ])
      
      if (splitInd == 1){##  first negative truncated normal fit
        
        if(nrow(fitDatList[[splitInd]])>30){
          print(paste0('nrow cur dat ', nrow(fitDatList[[splitInd]])))
          print(paste0('lower limit ',uniqueSplitPoints[splitInd], ' upper limit ',uniqueSplitPoints[splitInd + 1]  ))
          premeanN<- mean(fitDatList[[splitInd]][,outComeT])
          preStd <- sd(fitDatList[[splitInd]][,outComeT])
          xi <- ((fitDatList[[splitInd]][,outComeT])- premeanN)/preStd
          a <- uniqueSplitPoints[splitInd]
          b <- uniqueSplitPoints[splitInd + 1]
          bet <- (b - premeanN)/preStd
          alph <- (a - premeanN)/preStd
          Z <-     pnorm(bet) - pnorm(alph)
          meanN1 <- premeanN + ((dnorm(alph)-dnorm(bet))*preStd/Z)
          stdN1 <- sqrt((preStd^2)*( (1) + (((alph *dnorm(alph)) - (bet*dnorm(bet)))/(Z)) -(((dnorm(alph) - dnorm(bet))/(Z))^2)))
          par1N1 <- premeanN
          par2N1 <- preStd
        }
        else{
          meanN1 <- 0
          stdN1 <- 0
        }
      }
      
      else if(splitInd == 2){## negative normal fit 
        
        if(nrow(fitDatList[[splitInd]])>30){
          print(paste0('nrow cur dat ', nrow(fitDatList[[splitInd]])))
          print(paste0('lower limit ',uniqueSplitPoints[splitInd], ' upper limit ',uniqueSplitPoints[splitInd + 1]  ))
          premeanN<- mean(fitDatList[[splitInd]][,outComeT])
          preStd <- sd(fitDatList[[splitInd]][,outComeT])
          xi <- ((fitDatList[[splitInd]][,outComeT])- premeanN)/preStd
          a <- uniqueSplitPoints[splitInd]
          b <- uniqueSplitPoints[splitInd + 1]
          bet <- (b - premeanN)/preStd
          alph <- (a - premeanN)/preStd
          Z <-     pnorm(bet) - pnorm(alph)
          meanN2 <- premeanN + ((dnorm(alph)-dnorm(bet))*preStd/Z)
          stdN2 <- sqrt((preStd^2)*( (1) + (((alph *dnorm(alph)) - (bet*dnorm(bet)))/(Z)) -(((dnorm(alph) - dnorm(bet))/(Z))^2)))
          par1N2 <- premeanN
          par2N2 <- preStd
        }
        else{
          meanN2 <- 0
          stdN2 <- 0
        }
        
      }
      
      else if(splitInd == 3){## negative normal fit 
        
        if(nrow(fitDatList[[splitInd]])>30){
          print(paste0('nrow cur dat ', nrow(fitDatList[[splitInd]])))
          print(paste0('lower limit ',uniqueSplitPoints[splitInd], ' upper limit ',uniqueSplitPoints[splitInd + 1]  ))
          premeanP<- mean(fitDatList[[splitInd]][,outComeT])
          preStd <- sd(fitDatList[[splitInd]][,outComeT])
          xi <- ((fitDatList[[splitInd]][,outComeT])- premeanP)/preStd
          a <- uniqueSplitPoints[splitInd]
          b <- uniqueSplitPoints[splitInd + 1]
          bet <- (b - premeanP)/preStd
          alph <- (a - premeanP)/preStd
          Z <-     pnorm(bet) - pnorm(alph)
          meanP1 <- premeanP + ((dnorm(alph)-dnorm(bet))*preStd/Z)
          stdP1 <- sqrt((preStd^2)*( (1) + (((alph *dnorm(alph)) - (bet*dnorm(bet)))/(Z)) -(((dnorm(alph) - dnorm(bet))/(Z))^2)))
          par1P1 <- premeanP
          par2P1 <- preStd
        }
        else{
          meanP1 <- 0
          stdP1 <- 0
        }
        
      }
      
      else if(splitInd == 4){
        
        if(nrow(fitDatList[[splitInd]])>30){
          print(paste0('nrow cur dat ', nrow(fitDatList[[splitInd]])))
          print(paste0('lower limit ',uniqueSplitPoints[splitInd], ' upper limit ',uniqueSplitPoints[splitInd + 1]  ))
          premeanP<- mean(fitDatList[[splitInd]][,outComeT])
          preStd <- sd(fitDatList[[splitInd]][,outComeT])
          xi <- ((fitDatList[[splitInd]][,outComeT])- premeanP)/preStd
          a <- uniqueSplitPoints[splitInd]
          b <- uniqueSplitPoints[splitInd + 1]
          bet <- (b - premeanP)/preStd
          alph <- (a - premeanP)/preStd
          Z <-     pnorm(bet) - pnorm(alph)
          meanP2 <- premeanP + ((dnorm(alph)-dnorm(bet))*preStd/Z)
          stdP2 <- sqrt((preStd^2)*( (1) + (((alph *dnorm(alph)) - (bet*dnorm(bet)))/(Z)) -(((dnorm(alph) - dnorm(bet))/(Z))^2)))
          par1P2 <- premeanP
          par2P2 <- preStd
        }
        else{
          meanP2 <- 0
          stdP2 <- 0
        }
        
        
      }
      

      else{## function needs to be made better 
        print("please make funct better")
      }
      
    } 
    
    if(nrow(payData[outComeT<0,])>0){
      if(meanN1 != 0){
      meanVecs <- c(meanN1,meanN2,meanP1,meanP2)
      stdVecs <- c(stdN1,stdN2,stdP1,stdP2)
      par1Vecs <- c(par1N1, par1N2,par1P1,par1P2)
      par2Vecs <- c(par2N1, par2N2,par2P1,par2P2)
      uniqueSplitPoints <- sort(unique((splitPoints)))
      }
      else{
        meanVecs <- c(meanN2,meanP1,meanP2)
        stdVecs <- c(stdN2,stdP1,stdP2)
        uniqueSplitPoints <- sort(unique((splitPoints[splitPoints>min(splitPoints)])))
        par1Vecs <- c( par1N2,par1P1,par1P2)
        par2Vecs <- c( par2N2,par2P1,par2P2)
      }
      }
    else{
      uniqueSplitPoints <- sort(unique((splitPoints[splitPoints>=0])))
      meanVecs <- c(meanP1,meanP2)
      stdVecs <- c(stdP1,stdP2)
      par1Vecs <- c( par1P1,par1P2)
      par2Vecs <- c( par2P1,par2P2)
    }
    
    payData[, outComeTrans:= cut(outComeT,breaks = uniqueSplitPoints)]
    result[[iPay]]$fitPay <-   fitMN(payData, form, NULL, NULL, min(nrow(payData), 50000), 10000, T)
    result[[iPay]]$trainDT <- payData
    result[[iPay]]$form <- form
    result[[iPay]]$meanVecs <- meanVecs
    result[[iPay]]$stdVecs <- stdVecs
    result[[iPay]]$uniqueSplitPoints <- uniqueSplitPoints
    result[[iPay]]$par1Vecs <- par1Vecs
    result[[iPay]]$par2Vecs <- par2Vecs
    
    
  }
  
  return(result)
  
}

predictTruncNorm <- function(sampDT, finPayMods, listNr){
  
  trainDT <- finPayMods[[listNr]]$trainDT
  meanVecs <- finPayMods[[listNr]]$meanVecs
  
  if(nrow(sampDT) == 1){
    predProbs <- t(as.matrix(predict(finPayMods[[listNr]]$fitPay, newdata = sampDT, type = "probs")))
  } else {
    predProbs <- predict(finPayMods[[listNr]]$fitPay, newdata = sampDT, type = "probs")
  }
  
  if(length(meanVecs) == 2){
    predProbs <- as.matrix(cbind(1-predProbs, predProbs))
  }
  nSamps <- nrow(sampDT)
  
  sampsObs <- unname(rowSums(predProbs %*% diag(meanVecs)))
  
  return(sampsObs)
  
}

predictTruncNormSim <- function(sampDT, finPayMods, listNr){

  trainDT <- finPayMods[[listNr]]$trainDT
  meanVecs <- finPayMods[[listNr]]$par1Vecs
  sdVecs <- finPayMods[[listNr]]$par2Vecs
  
  if(nrow(sampDT) == 1){
    predProbs <- t(as.matrix(predict(finPayMods[[listNr]]$fitPay, newdata = sampDT, type = "probs")))
  } else {
    predProbs <- predict(finPayMods[[listNr]]$fitPay, newdata = sampDT, type = "probs")
  }
  
  nSamps <- nrow(sampDT)
  sampsObs <- rep(NA, nSamps)
  splits <- c(finPayMods[[listNr]]$uniqueSplitPoints)
  
  if(length(splits) == 3){# only positive data is present 
    predProbs <- as.matrix(cbind(1-predProbs, predProbs))
    for(iObs in 1:nSamps){
      
      sampMN <- rmultinom(1, 1, predProbs[iObs, ])
      
      sampleRow <- which(sampMN[, 1] == 1)
      if(sampleRow == 1){
        sampsObs[iObs] <- rtruncnorm(1, splits[1], splits[2],meanVecs[1], sdVecs[1] )
      } 
      else {
        sampsObs[iObs] <- rtruncnorm(1, splits[2], splits[3],meanVecs[2], sdVecs[2] )
      }
    }
    
  }
  
  else if(length(splits) == 4){#only small payments are present for negative data
    
    for(iObs in 1:nSamps){
      sampMN <- rmultinom(1, 1, predProbs[iObs, ])
      
      sampleRow <- which(sampMN[, 1] == 1)
      if(sampleRow == 1){
        sampsObs[iObs] <- rtruncnorm(1, splits[1], splits[2],meanVecs[1], sdVecs[1] )
      }
      else if(sampleRow == 2){
        sampsObs[iObs] <- rtruncnorm(1, splits[2], splits[3],meanVecs[2], sdVecs[2] )
      }
      else {
        sampsObs[iObs] <- rtruncnorm(1, splits[3], splits[4],meanVecs[3], sdVecs[3] )
      }
    }
    
  }
  
  else{# small and large payments are present for both positive and negative data
    
    for(iObs in 1:nSamps){
      sampMN <- rmultinom(1, 1, predProbs[iObs, ])
      
      sampleRow <- which(sampMN[, 1] == 1)
      if(sampleRow == 1){
        sampsObs[iObs] <- rtruncnorm(1, splits[1], splits[2],meanVecs[1], sdVecs[1] )
      }
      else if(sampleRow == 2){
        sampsObs[iObs] <- rtruncnorm(1, splits[2], splits[3],meanVecs[2], sdVecs[2] )
      }
      else if(sampleRow == 3){
        sampsObs[iObs] <- rtruncnorm(1, splits[3], splits[4],meanVecs[3], sdVecs[3] )
      }
      else {
        sampsObs[iObs] <- rtruncnorm(1, splits[4], splits[5],meanVecs[4], sdVecs[4] )
      }
    }
  }
  
 return(sampsObs)
}


predictSplicedParetoSim <- function(sampDT, finPayMods, listNr){
  
  trainDT <- finPayMods[[listNr]]$trainDT
  par1Vecs <- finPayMods[[listNr]]$par1
  par2Vecs <- finPayMods[[listNr]]$par2
  
  if(nrow(sampDT) == 1){
    predProbs <- t(as.matrix(predict(finPayMods[[listNr]]$fitPay, newdata = sampDT, type = "probs")))
  } else {
    predProbs <- predict(finPayMods[[listNr]]$fitPay, newdata = sampDT, type = "probs")
  }
  
  nSamps <- nrow(sampDT)
  sampsObs <- rep(NA, nSamps)
  splits <- c(finPayMods[[listNr]]$uniqueSplitPoints)
  
  if(length(splits) == 3){# only positive data is present 
    predProbs <- as.matrix(cbind(1-predProbs, predProbs))
    for(iObs in 1:nSamps){
      sampMN <- rmultinom(1, 1, predProbs[iObs, ])
      sampleRow <- which(sampMN[, 1] == 1)
      if(sampleRow == 1){
        sampsObs[iObs] <- rtruncnorm(1, splits[1], splits[2],par1Vecs[1], par2Vecs[1] )
      } 
      else {
        sampsObs[iObs] <- rgpd(1, splits[2],par1Vecs[2], par2Vecs[2] )
      }
    }
    
  }
  
  else if(length(splits) == 4){#only small payments are present for negative data
    
    for(iObs in 1:nSamps){
      sampMN <- rmultinom(1, 1, predProbs[iObs, ])
      
      sampleRow <- which(sampMN[, 1] == 1)
      if(sampleRow == 1){
        sampsObs[iObs] <- rtruncnorm(1, splits[1], splits[2],par1Vecs[1], par2Vecs[1] )
      }
      else if(sampleRow == 2){
        sampsObs[iObs] <- rtruncnorm(1, splits[2], splits[3],par1Vecs[2], par2Vecs[2] )
      }
      else {
        sampsObs[iObs] <- rgpd(1, splits[3],par1Vecs[3], par2Vecs[3] )
      }
    }
    
  }
  
  else{# small and large payments are present for both positive and negative data
    
    for(iObs in 1:nSamps){
      sampMN <- rmultinom(1, 1, predProbs[iObs, ])
      
      sampleRow <- which(sampMN[, 1] == 1)
      if(sampleRow == 1){
        sampsObs[iObs] <- -1*rgpd(1, splits[2],par1Vecs[1], par2Vecs[1] )
      }
      else if(sampleRow == 2){
        sampsObs[iObs] <- rtruncnorm(1, splits[2], splits[3],meanVecs[2], sdVecs[2] )
      }
      else if(sampleRow == 3){
        sampsObs[iObs] <- rtruncnorm(1, splits[3], splits[4],meanVecs[3], sdVecs[3] )
      }
      else {
        sampsObs[iObs] <- rgpd(1, splits[4],par1Vecs[4], par2Vecs[4] )
      }
    }
  }
  
  return(sampsObs)
}


#to extract the observed reserve for the test data set
extractObsResJordy <- function(predSim, fullDT, inflRates, raw2 = FALSE, totalCost = FALSE,testSimsDT = NULL){
  # Check input parameters.
  if(!is.list(predSim)) stop("The argument 'predSim' is a list of lists, containing data.table object. An output object of the microModel::simPaths function.")
  if(!is.list(fullDT)) stop("The argument 'predSim' is a list")
  
  checkNumOrIntVec(list(inflRates))
  checkLogicVec(list(raw2))
  checkEqualLength(list(inflRates, fullDT))
  checkLength(list(raw2), 1)
  
  polNumbsCurr <- c()
  obsCost <- list()
  obsCostRaw <- list()
  nTrans <- length(fullDT)
  
  for(iList in 1:(length(fullDT))){
    
    #get inflation rates
    selInflRate <- inflRates[min(length(inflRates), iList)] #changed 180520
    
    if(iList == 1){
      #initialise  all the polNumbs that were used in the simpaths function 
      polNumbs <- llply(predSim, names)
      polNumbs <- unique(unlist(polNumbs))
      
      length(obsCost) <- length(polNumbs)# will hold the reserve values for each polNumb 
      names(obsCost) <- polNumbs
      obsCost <- llply(obsCost, function(xx) xx <- 0.0)#initialised for each polNumb in predSim to 0
      
      if(raw2){
        length(obsCostRaw) <- length(polNumbs)
        names(obsCostRaw) <- polNumbs
        obsCostRaw <- llply(obsCostRaw, function(xx) xx <- 0.0)
      }
      
    }
    
    if(totalCost){
      polNumbsCurr <- polNumbs
    } else {
      polNumbsCurr <- unique(c(polNumbsCurr, names(predSim[[iList]])))# add polNumbs from the each previous predSimDT to the current predSimDT polNumbs
    }
    
    for(iPol in polNumbsCurr){
      if(nrow(fullDT[[iList]][polNumb == iPol, ])){# if the polNumb is in the current fullDT
        ind <- which(names(obsCost) == iPol)# get the index in obsCost of the polNumb 
        temp_DT <- unique(fullDT[[iList]][polNumb == iPol, .(finYear, outComeT,inProcTime,transStat)])# get each unique row of the polNumb in each DT 
        
        if((iList == nTrans) & (iPol %in% testSimsDT[[nTrans]][["polNumb"]])){## the first time the polnumb appears is in the last dataset so  you need to take into account what is inprocTime for that line
          #need correction in case the sampled row in testSimsDT is not the first row (with payment) of the polNumb in fullDT[[iList]]
          inProcTimeCur <- testSimsDT[[iList]][polNumb == iPol,inProcTime]
          temp_DT <- temp_DT[inProcTime >=inProcTimeCur& transStat ==1 ,]
        }
        
        temp_DT <- temp_DT[outComeT != 0 & transStat ==1, ]
        if(nrow(temp_DT) > 0){
          for(iTemp in 1:nrow(temp_DT)){
            obsCost[[ind]] <- obsCost[[ind]] + (temp_DT[iTemp, 2]* (((selInflRate/100) + 1)^(currentYear - temp_DT[iTemp, 1])))
            if(raw2) obsCostRaw[[ind]] <- obsCostRaw[[ind]] + temp_DT[iTemp, 2]
          }
        }
      }
    }
  }
  if(raw2){
    return(list(obsResRaw = obsCostRaw, obsRes = obsCost))
  } else {
    return(list(obsRes = obsCost))
  }
}

#timeMods <- finTimeMods; payMods <- finPayMods; trainData <- trainDT; timeSplits <- NULL; predData <- copy(calibDT2); fixedTimeMax <- rep(24,6);  perLen <- 30; nSims <- 3; lastTrans <- 10; verbose <- TRUE; payModType = "splicedGPD"
simPathsJolien <- function(timeMods, payMods, trainData, timeSplits, predData, fixedTimeMax, perLen, nSims, lastTrans, payModType,verbose,maxProc = 12){ #changed 080520
  # Check input parameters and certain variables in 'trainData' and 'predData'.
  if(!is.list(timeMods)) stop("Argument 'timeMods' is a list containing the different time model fits.")
  if(!is.list(payMods)) stop("Argument 'payMods' is a list containing the different payment model fits.")
  # checkEqualLength(list(timeMods, trainData, predData))
  # checkEqualLength(list(payMods, trainData, predData))
  llply(trainData, function(xx) checkDT(xx))
  llply(predData, function(xx) checkDT(xx))
  checkWholeNumb(list(nSims, lastTrans)) #changed 080520
  checkLogicVec(list(verbose))
  checkLength(list(nSims, verbose, lastTrans), 1) #changed 080520
  checkRanges(list(nSims, lastTrans), list(c('>', 0), c('>', 0))) #changed 080520
  
  nNonNs <- laply(predData, function(xx) ifelse(nrow(xx) > 0, nrow(xx[transMN != 'N', ]), 0)) #changed 150420
  if(sum(nNonNs) > 0) stop("The transMN column of at least one element of the 'predData' argument did not all just contain rows with value 'N'. Please correct for this.") #changed 150420
  nNonPs <- laply(predData, function(xx) ifelse(nrow(xx) > 0, nrow(xx[transType != 'P', ]), 0)) #changed 290420
  if(sum(nNonPs) > 0) stop("The transMN column of at least one element of the 'testDT' argument did not all just contain rows with value 'P'. Please correct for this.") #changed 150420
  
  if(verbose) print('Setting the stage')
  
  predSim <- list()
  nSim <- list()
  predTransAll <- list()
  nTrans <- min(length(timeMods), length(predData))#Jordy
  
  # Defining the initial nSim and predSim objects
  # nSim indicates how many sims still need to be taken for the polNumb of interest,
  # while predSim contains the data sets that were used for the timeMod and payMod.
  # both objects are a list, of length the number of transitions and they contain as
  # elements as many polNumbs as still need simulations.
  
  minTrans <- NA
  
  for(iTrans in 1:nTrans){
    
    if(verbose) print(iTrans)
    #pour chaque numéro de sinistre vous pouvez voir combien de simulations il vous reste à faire (liste des éléments par claimNumb)
    nSim[[iTrans]] <- as.list(rep(nSims, nrow(predData[[iTrans]]))) #Jordy: a claimNr will just appear for the transition where it is still open (later on, not)
    predSim[[iTrans]] <- nSim[[iTrans]]#Jordy: nSim[[iTrans]]  is a list, where for each transition number, you for each polNumb how many times you need to simulate it 
    # predTransALL := données pour lesquelles vous devez prévoir
    predTransAll[[iTrans]] <- predData[[iTrans]]
    
    if(nrow(predData[[iTrans]]) > 0){
      
      if(is.na(minTrans)) minTrans <- iTrans
      
      for(iRun in 1:length(nSim[[iTrans]])){ #pour tout polNumb que vous devez simuler pour
        predSim[[iTrans]][[iRun]] <- predTransAll[[iTrans]][rep(iRun, nSim[[iTrans]][iRun]),]#Jordy: repeat each line from predData for the current polNumb, nSim times
        predSim[[iTrans]][[iRun]]$inStateTimeTrans <- as.numeric(as.character(predSim[[iTrans]][[iRun]]$inStateTimeTrans))
        #predSim[[iTrans]][[iRun]]$inStateTimeTransGrPay   <- as.numeric(as.character(predSim[[iTrans]][[iRun]]$inStateTimeTransGrPay  ))
        predSim[[iTrans]][[iRun]]$inProcTimeTrans <- as.numeric(as.character(predSim[[iTrans]][[iRun]]$inProcTimeTrans))
        predSim[[iTrans]][[iRun]]$inStateTime <- as.numeric(as.character(predSim[[iTrans]][[iRun]]$inStateTime))
        predSim[[iTrans]][[iRun]]$inProcTime <- as.numeric(as.character(predSim[[iTrans]][[iRun]]$inProcTime))
        predSim[[iTrans]][[iRun]][, simPer := 0]#Jordy: simPer is 0 because at this moment there has been no 1 step ahead simulation 
        #predsim[[iTrans]] contient la liste. Chaque liste appartient à 1 polNumb et répète les données nSims fois
      }
      predTransAll[[iTrans]] <- predData[[iTrans]][rep(1:nrow(predData[[iTrans]]), nSims), ]
      predTransAll[[iTrans]] <- predTransAll[[iTrans]][order(polNumb), ] #predTransAll contient toutes les données de predsim, sans listes
      
      names(nSim[[iTrans]]) <- predData[[iTrans]]$polNumb
      names(predSim[[iTrans]]) <- predData[[iTrans]]$polNumb
      
    }
  }
  
  addDT <- data.table()
  
  if(!is.na(minTrans)){
    
    iTrans <- minTrans
    goOn <- TRUE
    
    if(verbose) print('Looping over the different transitions')
    
    while(goOn){ #while(iTrans <= 6)
      if(verbose) cat('\n')
      if(verbose) print('////||||\\\\')
      if(verbose) print(paste('TRANSITION', iTrans))
      if(verbose) print('\\\\||||////')
      if(verbose) cat('\n')
      
      if(verbose) print(paste('nSimTot: ', sum(unlist(nSim[[iTrans]]))))
      
      #Prepping and augmenting the data for the simulations
      
      #Jolien 
      #Jordy: change trainData to the data that was use when fitting the time model, because with the sampling in fitMN it's possible that not all classes are sampled.
      # inProcTimeTransMax = max(as.numeric(as.character(trainData[[min(nTrans, iTrans)]]$inProcTimeTrans)))
      # inProcTimeTransMin= min(as.numeric(as.character(trainData[[min(nTrans, iTrans)]]$inProcTimeTrans)))
      # 
      # inStateTimeTransMax = max(as.numeric(as.character(trainData[[min(nTrans, iTrans)]]$inStateTimeTrans)))
      # inStateTimeTransMin = min(as.numeric(as.character(trainData[[min(nTrans, iTrans)]]$inStateTimeTrans)))
      # 
      # inStateTimeTransMaxPay = max(as.numeric(as.character(trainData[[min(nTrans, iTrans)]]$inStateTimeTransGrPay)))
      # inStateTimeTransMinPay = max(min(as.numeric(as.character(trainData[[min(nTrans, iTrans)]]$inStateTimeTransGrPay))),1)
      
      inStateTimeTransMaxPay <- max(as.numeric(as.character(names(table(payMods[[min(nTrans, iTrans)]]$trainDT$inStateTimeTrans))))) #changed 150420
      inProcTimeTransMaxPay <- max(as.numeric(as.character(names(table(payMods[[min(nTrans, iTrans)]]$trainDT$inProcTimeTrans))))) #changed 150420
      
      inStateTimeTransMinPay <- min(as.numeric(as.character(names(table(payMods[[min(nTrans, iTrans)]]$trainDT$inStateTimeTrans))))) #changed 150420
      inProcTimeTransMinPay <- min(as.numeric(as.character(names(table(payMods[[min(nTrans, iTrans)]]$trainDT$inProcTimeTrans))))) #changed 150420
      
      inStateTimeTransMaxTime <- max(as.numeric(as.character(names(table(timeMods[[min(nTrans, iTrans)]]$trainDT$inStateTimeTrans))))) #changed 150420
      inProcTimeTransMaxTime <- max(as.numeric(as.character(names(table(timeMods[[min(nTrans, iTrans)]]$trainDT$inProcTimeTrans))))) #changed 150420
      
      inStateTimeTransMinTime <- min(as.numeric(as.character(names(table(timeMods[[min(nTrans, iTrans)]]$trainDT$inStateTimeTrans))))) #changed 150420
      inProcTimeTransMinTime <- min(as.numeric(as.character(names(table(timeMods[[min(nTrans, iTrans)]]$trainDT$inProcTimeTrans))))) #changed 150420
      
      inStateTimeTransMin <- max(inStateTimeTransMinTime, inStateTimeTransMinPay) #changed 150420
      inProcTimeTransMin <- max(inProcTimeTransMinTime, inProcTimeTransMinPay) #changed 150420
      
      inStateTimeTransMax <- min(inStateTimeTransMaxTime, inStateTimeTransMaxPay) #changed 150420
      inProcTimeTransMax <- min(inProcTimeTransMaxTime, inProcTimeTransMaxPay) #changed 150420
      
      
      deltRepTimeTransMax = max(as.numeric(as.character(trainData[[min(nTrans, iTrans)]]$deltRepTrans)))
      deltRepTimeTransMin = min(as.numeric(as.character(trainData[[min(nTrans, iTrans)]]$deltRepTrans)))
      
      asNumericDT(trainData[[min(nTrans, iTrans)]], c('inStateTimeTrans', 'inProcTimeTrans'))
      
      if(nTrans >= iTrans){
        if(nrow(addDT) != 0){
          addDT[, inStateTimeMax := inStateTimeTransMax]
        }
        predTrans <- rbind(predTransAll[[iTrans]], addDT, fill = T) #JOLIEN: fill =T, to have no issues when merging (columns for first dataset are different from the ones afterwards)
      } else{
        predTrans <- addDT
        #We need to reset the predSim data
        polNumbUniq <- unique(predTrans$polNumb)# all unique polNumbs from that pred transition  dataset 
        predSim[[iTrans]] <- list()
        
        for(iRun in 1:length(polNumbUniq)){
          predSim[[iTrans]][[iRun]] <- predTrans[polNumb == polNumbUniq[iRun],]
        }
        names(predSim[[iTrans]]) <- polNumbUniq
      }
      
      predTransL <- split(predTrans, by = 'polNumb')
      #inStateTimeMax <- min(trainData[[min(nTrans, iTrans)]][1, inStateTimeMax], fixedTimeMax) 
      inStateTimeMax = fixedTimeMax[min(nTrans, iTrans)] #JOLIEN
      if(verbose) print(paste('Number of open claims of this transition:', nrow(predTrans)))
      
      nSimTrans <- nSim[[iTrans]]
      addPer <- 0# number of periods that were added to the simulations for each polNumb 
      stillOpen <- TRUE
      
      # looping until all claims have their simulated transition, i.e. untill there are no more transMN == N 
      
      while(stillOpen){
        
        ptm <- proc.time()
        #addPer <- pmin(addPer + 1, inStateTimeMax)
        addPer <- addPer + 1 #Jolien
        if(verbose) print(paste('Run', addPer))
        
        selPolNumbs <- names(unlist(nSimTrans))[unlist(nSimTrans) != 0]# the polNumbs that will be used 
        
        predTransDT <- rbindlist(predTransL, fill = T)
        runData <- predTransDT[polNumb %in% selPolNumbs, ][transMN == 'N', ]# only the lines of predTransDT where transMN is N
        
        if(addPer == 1){ #changed 050520 # Jolien, not really necessary, we did this before test set was given to this function
          asNumericDT(runData, c('inStateTimeTrans',   'inProcTimeTrans',"deltRepTrans")) #changed 050520
          runData[inStateTimeTrans > inStateTimeTransMax, inStateTimeTrans := inStateTimeTransMax] #changed 050520
          runData[inStateTimeTrans < inStateTimeTransMin, inStateTimeTrans := inStateTimeTransMin] #changed 050520
          runData[inProcTimeTrans > inProcTimeTransMax, inProcTimeTrans := inProcTimeTransMax] #changed 050520
          runData[inProcTimeTrans < inProcTimeTransMin, inProcTimeTrans := inProcTimeTransMin] #changed 050520
          runData[deltRepTrans > deltRepTimeTransMax, deltRepTrans := deltRepTimeTransMax] #changed 050520
          runData[deltRepTrans < deltRepTimeTransMin, deltRepTrans := deltRepTimeTransMin] #changed 050520
          asFactorDT(runData, c('inStateTimeTrans', 'inProcTimeTrans','fastRep', 'deltRepTrans')) #changed 050520
        } #changed 050520
        
        predTransL <- split(runData, by = 'polNumb')
        
        asFactorDT(runData, c('inStateTimeTrans', 'inProcTimeTrans'))
        if(sum(names(runData) == 'delt1PayTimeTrans') != 0) asFactorDT(runData, c('delt1PayTimeTrans'))
        if(sum(names(runData) == 'delt2PayTimeTrans') != 0) asFactorDT(runData, c('delt2PayTimeTrans'))
        
        if(nrow(runData) > 1){
          predProb <- predict(timeMods[[min(length(timeMods), min(nTrans, iTrans))]]$fitTime, newdata = runData, type = 'probs')
        } else {
          predProb <- t(as.matrix(predict(timeMods[[min(length(timeMods), min(nTrans, iTrans))]]$fitTime, newdata = runData, type = 'probs')))
        }
        
        if(ncol(predProb) < 4){ #een van de categorien die niet in de training zit
          
          fixedColNames <- c('N', 'P', 'TN', 'TP')
          missingCol <- !(fixedColNames %in% colnames(predProb))
          
          fixedColNames[missingCol]
          for(iLoop in 1:sum(missingCol)){
            predProb <- cbind(predProb, rep(0, nrow(predProb)))
            colnames(predProb)[ncol(predProb)] <- fixedColNames[missingCol][iLoop]
          }
          predProb <- predProb[, order(colnames(predProb), fixedColNames)]
        }
        
        if(verbose) print(paste0('The expected number of lines after this run: ', floor(mean(predProb[,1])*nrow(runData))))
        
        # No more non terminal payments when lastTrans is reached #changed 080520
        if(iTrans >= lastTrans){ #changed 080520
          predProb[, 4] <- predProb[, 4] + predProb[, 2] #changed 080520 - payment w terminal payment
          predProb[, 2] <- 0 #changed 080520 - payment krijgt kans nul
        } #changed 080520
        # No more staying in the state without a transition when inStateTimeMax is reached #changed 080520
        # should be replaced  inside the loop
        # if(addPer >= inStateTimeMax & iTrans < lastTrans){ #changed 080520
        #   predProb[, 2] <- predProb[, 2] + predProb[, 1]/3 #changed 080520
        #   predProb[, 3] <- predProb[, 3] + predProb[, 1]/3 #changed 080520
        #   predProb[, 4] <- predProb[, 4] + predProb[, 1]/3 #changed 080520
        #   predProb[, 1] <- 0 #changed 080520
        # } else if(addPer >= inStateTimeMax & iTrans >= lastTrans){ #changed 080520
        #   predProb[, 3] <- predProb[, 3] + predProb[, 1]/2 #changed 080520
        #   predProb[, 4] <- predProb[, 4] + predProb[, 1]/2 #changed 080520
        #   predProb[, 1] <- 0 #changed 080520
        # }
        # if(addPer >= inStateTimeMax){ #removed 080520
        #   for(iRow in 1:nrow(predProb)){ #removed 080520
        #     if(sum(predProb[iRow, ][2:4]) < 0.05){ #removed 080520
        #       predProb[iRow, 2:4] <- 1/3 #removed 080520
        #       predProb[iRow, 1] <- 0 #removed 080520
        #     } #removed 080520
        #   } #removed 080520
        # } #removed 080520
        
        if(iTrans > 1){
          pastedPredProb <- aaply(predProb, 1, function(xx) paste(xx, collapse = ''))# Jordy: transform nx4 matrix to nx1 matrix. Used to compare afterwards what are the unique probabilties 
        }
        
        runData$inStateTimeTrans <- as.numeric(as.character(runData$inStateTimeTrans))
        #runData$inStateTimeTransGrPay <- as.numeric(as.character(runData$inStateTimeTransGrPay))
        runData$inProcTimeTrans <- as.numeric(as.character(runData$inProcTimeTrans))
        
        simRun <- as.list(rep(1, nrow(runData)))
        
        # The actual simulation of the transition
        for(iRun in 1:length(selPolNumbs)){ # pour chaque polnumb unique à simuler
          if(iRun %% 1000 == 0) print(iRun)
          # print(iRun)
          indNPol <- which(predTransL[[which(names(predTransL) == selPolNumbs[iRun])]][, transMN] == 'N')
          sel <- which(runData$polNumb == selPolNumbs[iRun])
          uPredProb <- predProb[sel, ] # les prédictions pour le polnumb selectionnée
          
          #Jolien: something you've to check for each claim seperately
          #before it was done outside this loop of iRun
          if(!is.null(maxProc)){
            if(any(as.numeric(as.character(runData[polNumb == selPolNumbs[iRun]]$inProcTime))) >= maxProc){
              if((sum(is.null(nrow(uPredProb))) + sum(nrow(uPredProb) == 1)) >= 1){ #one row only
                uPredProb[3] <- uPredProb[3] + uPredProb[1]/2+ uPredProb[2]/2 #changed 080520
                uPredProb[4] <- uPredProb[4] + uPredProb[1]/2 + uPredProb[2]/2 #changed 080520
                uPredProb[1] <- 0 #changed 080520
                uPredProb[2] <- 0 #changed 080520
              }else{
                sel2 = which(as.numeric(as.character(runData[polNumb == selPolNumbs[iRun]]$inProcTime))) >= maxProx
                uPredProb[sel2,3] <- uPredProb[sel2,3] + uPredProb[sel2,1]/2+ uPredProb[sel2,2]/2 #changed 080520
                uPredProb[sel2,4] <- uPredProb[sel2,4] + uPredProb[sel2,1]/2 + uPredProb[sel2,2]/2 #changed 080520
                uPredProb[sel2,1] <- 0 #changed 080520
                uPredProb[sel2,2] <- 0 #changed 080520
              }
            }
          }
          
          
          
          if(any(as.numeric(as.character(runData[polNumb == selPolNumbs[iRun]]$inStateTime)) >= inStateTimeMax) & iTrans < lastTrans){ #changed 080520 #Jolien
            if((sum(is.null(nrow(uPredProb))) + sum(nrow(uPredProb) == 1)) >= 1){ #one row only
              uPredProb[2] <- uPredProb[ 2] + uPredProb[ 1]/3 #changed 080520
              uPredProb[3] <- uPredProb[ 3] + uPredProb[ 1]/3 #changed 080520
              uPredProb[4] <- uPredProb[4] + uPredProb[ 1]/3 #changed 080520
              uPredProb[1] <- 0 #changed 080520
            }else{
              uPredProb[, 2] <- uPredProb[, 2] + uPredProb[, 1]/3 #changed 080520
              uPredProb[, 3] <- uPredProb[, 3] + uPredProb[, 1]/3 #changed 080520
              uPredProb[, 4] <- uPredProb[, 4] + uPredProb[, 1]/3 #changed 080520
              uPredProb[, 1] <- 0 #changed 080520
            }
          } else if(any(as.numeric(as.character(runData[polNumb == selPolNumbs[iRun]]$inStateTime)) >= inStateTimeMax) & iTrans >= lastTrans){ #changed 080520
            if((sum(is.null(nrow(uPredProb))) + sum(nrow(uPredProb) == 1)) >= 1){ #one row only
              #volgende transitie mag niet meer
              uPredProb[ 3] <- uPredProb[3] + uPredProb[1]/2 #changed 080520
              uPredProb[ 4] <- uPredProb[4] + uPredProb[1]/2 #changed 080520
              uPredProb[ 1] <- 0 #changed 080520
            }else{
              #volgende transitie mag niet meer
              uPredProb[, 3] <- uPredProb[, 3] + uPredProb[, 1]/2 #changed 080520
              uPredProb[, 4] <- uPredProb[, 4] + uPredProb[, 1]/2 #changed 080520
              uPredProb[, 1] <- 0 #changed 080520
            }
          }
          
          predProb[sel, ] <- uPredProb # update les predictions pour ce polnumb
          
          if(iTrans > 1){
            if(length(pastedPredProb[which(runData$polNumb == selPolNumbs[iRun])]==1)){
              pastedPredProb[which(runData$polNumb == selPolNumbs[iRun])] <- paste(predProb[sel, ], collapse = '')
            }else{
              pastedPredProb[which(runData$polNumb == selPolNumbs[iRun])] <- aaply(predProb[sel, ], 1, function(xx) paste(xx, collapse = ''))
            } 
          }
          
          if(length(sel) > 1){
            uPredProb <- unique(uPredProb)# the unique predicted probabilities
          }
          
          if((sum(is.null(nrow(uPredProb))) + sum(nrow(uPredProb) == 1)) >= 1){ #hence one row only
            
            simRun[[iRun]] <- rmultinom(1, as.numeric(nSimTrans[selPolNumbs[iRun]]), prob = uPredProb) #eigenlijke simulatie
            rownames(simRun[[iRun]]) <- colnames(predProb)
            
            nSimTrans[selPolNumbs[iRun]] <- simRun[[iRun]]['N', ]# Jordy: nsimtrans keeps only the number of remaining transitions in the current state to simulate 
            
            rowInds <- cumsum(simRun[[iRun]])#Jordy: rowInds contains the repartition of the simulated transitions in the trajectories 
            x <- copy(predSim[[iTrans]][selPolNumbs[iRun]][[1]])# Jordy: x contains all predData of the polNumb for the given transition 
            
            indsN <- which(x$transMN == 'N')
            # déterminer ce qui va à P, TP ou TN
            if(simRun[[iRun]][2] != 0) x[indsN, ] <- x[indsN, ][(rowInds[1] + 1):rowInds[2], transMN := 'P']
            if(simRun[[iRun]][3] != 0) x[indsN, ] <- x[indsN, ][(rowInds[2] + 1):rowInds[3], transMN := 'TN']
            if(simRun[[iRun]][4] != 0) x[indsN, ] <- x[indsN, ][(rowInds[3] + 1):rowInds[4], transMN := 'TP']
            
            if(sum(simRun[[iRun]][2:4]) != 0){
              if(sum(names(x) == 'simPer') == 0){
                x[, simPer := NA]
                asNumericDT(x, 'simPer')
              }
              #Jolien, ne mettre à jour que les lignes qui partent i.e ou transMN %in% P, TP, TN ; rowInds[1]+1 is the index where the first transition out of the state happens
              x[indsN, ] <- x[indsN, ][(rowInds[1] + 1):rowInds[4], simPer := addPer]#Jordy: simPer is how many periods were simulated before we had a transition 
              x[indsN, ] <- x[indsN, ][(rowInds[1] + 1):rowInds[4], inStateTimeTrans := max(c(min(c(inStateTimeTrans + addPer - 1, inStateTimeTransMax)), inStateTimeTransMin))] #changed 150420
              x[indsN, ] <- x[indsN, ][(rowInds[1] + 1):rowInds[4], inStateTime := inStateTime + addPer -1]#changed 150420 #Jordy: Why are we still updating instatetime?
              #x[indsN, ] <- x[indsN, ][(rowInds[1] + 1):rowInds[4], inStateTimeTransGrPay  := max(c(min(c(inStateTimeTransGrPay  + addPer -1, inStateTimeTransMaxPay)), inStateTimeTransMinPay))] #changed 150420
              x[indsN, ] <- x[indsN, ][(rowInds[1] + 1):rowInds[4], inProcTimeTrans := max(c(min(c(inProcTimeTrans + addPer-1 , inProcTimeTransMax)), inProcTimeTransMin))] #changed 150420
              x[indsN, ] <- x[indsN, ][(rowInds[1] + 1):rowInds[4], inProcTime := inProcTime + addPer -1]#Jordy: Why are we still updating inProcTime? => used to update inprocTimeTrans later on 
            }
            predSim[[iTrans]][selPolNumbs[iRun]][[1]] <- copy(x)#Jordy: because x  now contains updated covariates and we are looping over the polNumbs untill each polNumb exits the state 
            predTransL[[which(names(predTransL) == selPolNumbs[iRun])]][indNPol, transMN := x[indsN, transMN]]#Jordy: need to update transMN with the transitions (or not) that happened 
          } else {
            pastedPredProbTemp <- pastedPredProb[which(runData$polNumb == selPolNumbs[iRun])]
            x <- copy(predSim[[iTrans]][selPolNumbs[iRun]][[1]])
            indsN <- which(x$transMN == 'N')
            
            pastedPredProbTempUniq <- unique(pastedPredProbTemp)
            nUniq <- length(pastedPredProbTempUniq)
            predProbTemp <- predProb[which(runData$polNumb == selPolNumbs[iRun]),]
            
            for(iUniq in 1:nUniq){
              uniqRows <- which(pastedPredProbTemp == pastedPredProbTempUniq[iUniq])
              simRunTemp <- rmultinom(1, length(uniqRows), prob = predProbTemp[uniqRows[1], ])
              x <- x[indsN[uniqRows], transMN := rep(rownames(simRunTemp), simRunTemp)]
            }
            
            predSim[[iTrans]][selPolNumbs[iRun]][[1]] <- copy(x)
            nSimTrans[selPolNumbs[iRun]] <- nrow(predSim[[iTrans]][selPolNumbs[iRun]][[1]][transMN == 'N', ]) #aantal dat er met dit model nog dienen gesimuleerd te worden
            
            predSim[[iTrans]][selPolNumbs[iRun]][[1]][indsN, simPer := addPer]
            predSim[[iTrans]][selPolNumbs[iRun]][[1]][indsN, inStateTimeTrans := max(c(min(c(inStateTimeTrans + addPer -1, inStateTimeTransMax)), inStateTimeTransMin))] #changed 150420
            predSim[[iTrans]][selPolNumbs[iRun]][[1]][indsN, inStateTime := inStateTime+addPer -1] #changed 150420
            #predSim[[iTrans]][selPolNumbs[iRun]][[1]][indsN, inStateTimeTransGrPay  := max(c(min(c(inStateTimeTransGrPay  + addPer -1 , inStateTimeTransMaxPay)), inStateTimeTransMinPay))] #changed 150420
            predSim[[iTrans]][selPolNumbs[iRun]][[1]][indsN, inProcTimeTrans := max(c(min(c(inProcTimeTrans + addPer -1 , inProcTimeTransMax)), inProcTimeTransMin))] #changed 150420
            predSim[[iTrans]][selPolNumbs[iRun]][[1]][indsN, inProcTime := inProcTime +addPer-1] #changed 150420
            
            simRun[[iRun]] <- simRunTemp
            predTransL[[which(names(predTransL) == selPolNumbs[iRun])]][, transMN := x[indsN, transMN]]
            
          }
        }
        
        # Updating the values of the time vars that are in the time model
        
        predTransDT <- rbindlist(predTransL, fill = T)
        
        predTransDT$inStateTimeTrans <- as.numeric(as.character(predTransDT$inStateTimeTrans))
        predTransDT$inStateTimeTransGrPay  <- as.numeric(as.character(predTransDT$inStateTimeTransGrPay))
        predTransDT$inProcTimeTrans <- as.numeric(as.character(predTransDT$inProcTimeTrans))
        predTransDT$inStateTime <- as.numeric(as.character(predTransDT$inStateTime))
        predTransDT$inProcTime <- as.numeric(as.character(predTransDT$inProcTime))
        predTransDT[, inStateTimeTrans := max(c(min(c(inStateTimeTrans + 1, inStateTimeTransMax)), inStateTimeTransMin))] #changed 150420
        #predTransDT[, inStateTimeTransGrPay := max(c(min(c(inStateTimeTransGrPay  + 1, inStateTimeTransMaxPay)), inStateTimeTransMinPay))] #changed 150420
        predTransDT[, inProcTimeTrans := max(c(min(c(inProcTimeTrans + 1, inProcTimeTransMax)), inProcTimeTransMin))] #changed 150420
        predTransDT[, inStateTime := inStateTime + 1] #changed 150420
        predTransDT[, inProcTime := inProcTime + 1] #changed 150420
        
        asFactorDT(predTransDT, c('inStateTimeTrans', 'inProcTimeTrans'))
        
        predTransL <- split(predTransDT, by = 'polNumb')
        
        if(verbose) print(paste('Number of polNumbs with sims:', sum(unlist(nSimTrans) != 0)))
        if(verbose) print(paste('Number of remaining sims:', sum(unlist(nSimTrans[selPolNumbs]))))
        if(verbose) print(paste('Time in secs of this run:', round(as.numeric((proc.time() - ptm)[3]), digits = 3)))
        if(sum(unlist(nSimTrans) != 0) == 0) stillOpen <- FALSE
      }
      
      
      if(verbose) print('Simulating payments for those transitions that require a payment')
      
      iPol <- names(predSim[[iTrans]])[1]
      count <- 1
      # @ JORDY; You will need this for loop instead of the one i have below 
      for(iPol in names(predSim[[iTrans]])[count:length(predSim[[iTrans]])]){
        
        if(count %% 1000 == 0) print(count)
        
        payInds <- which(predSim[[iTrans]][iPol][[1]]$transMN %in% c('TP', 'P'))
        noPayInds <- setdiff(1:nrow(predSim[[iTrans]][iPol][[1]]), payInds)
        predSim[[iTrans]][iPol][[1]][noPayInds, outComeT := NA]
        
        #selecting the ones that still need a payment simulation with nSimTrans or so
        
        # x[indsN, ][(rowInds[1] + 1):rowInds[4], inStateTimeTrans := max(c(min(c(inStateTimeTrans + addPer - 1, inStateTimeTransMax)), inStateTimeTransMin))] #removed 210420
        # predSim[[iTrans]][iPol][[1]][, inStateTimeTrans := max(c(min(c(inStateTimeTrans + addPer - 1, inStateTimeTransMax)), inStateTimeTransMin))] #removed 210420
        
        if(length(payInds) != 0){
          sampDT <- predSim[[iTrans]][iPol][[1]][payInds, ]
          asFactorDT(sampDT, names(sampDT)[names(sampDT) %in% c('inStateTimeTrans', 'inProcTimeTrans', 'delt1PayTimeTrans', 'delt2PayTimeTrans','deltRepTrans','fastRep')]) #added
          sampDT[is.na(sampDT)] <- 0
          if(payModType == "MN"){
            samps <- rSplicingNP_fast(sampDT, payMods, min(nTrans, iTrans))
          }
          else if(payModType == "GLM"){
            samps <- predict(payMods[[min(nTrans, iTrans)]]$fitPay, newdata =sampDT ,type= "response")
          }
          else if(payModType == "normix"){
            samps <- unlist(predict(payMods[[min(nTrans, iTrans)]]$fitPay, newdata = sampDT, aggregate = TRUE))
          }
          else if(payModType == "splicedGPD"){
            samps <- predictSplicedPareto(sampDT, payMods, min(nTrans, iTrans))
          }
          else if(payModType == "splicedGPDSim"){
            samps <- predictSplicedParetoSim(sampDT, payMods, min(nTrans, iTrans))
          }
          else if(payModType == "truncNorm"){
            samps <- predictTruncNorm(sampDT, payMods, min(nTrans, iTrans))
          }
          else if(payModType == "truncNormSim"){
            samps <- predictTruncNormSim(sampDT, payMods, min(nTrans, iTrans))
          }
          
          else{
            
          }
          if(is.logical(samps)){
            print('breaking')
            break
          }
          predSim[[iTrans]][iPol][[1]][payInds, outComeT := samps]
        }
        
        count <- count + 1
        
      }
      
      
      # for(iPol in names(predSim[[iTrans]])[count:length(predSim[[iTrans]])]){
      #   
      #   if(count %% 1000 == 0) print(count)
      #   
      #   payInds <- which(predSim[[iTrans]][iPol][[1]]$transMN %in% c('TP', 'P'))
      #   noPayInds <- setdiff(1:nrow(predSim[[iTrans]][iPol][[1]]), payInds)
      #   
      #   if(iTrans == 1){
      #     predSim[[iTrans]][iPol][[1]][noPayInds, outComeT := NA]
      #     #selecting the ones that still need a payment simulation with nSimTrans or so
      #     
      #     # x[indsN, ][(rowInds[1] + 1):rowInds[4], inStateTimeTrans := max(c(min(c(inStateTimeTrans + addPer - 1, inStateTimeTransMax)), inStateTimeTransMin))] #removed 210420
      #     # predSim[[iTrans]][iPol][[1]][, inStateTimeTrans := max(c(min(c(inStateTimeTrans + addPer - 1, inStateTimeTransMax)), inStateTimeTransMin))] #removed 210420
      #     
      #     if(length(payInds) != 0){
      #       sampDT <- predSim[[iTrans]][iPol][[1]][payInds, ]
      #       asFactorDT(sampDT, c('inStateTimeTransGrPay')) #added
      #       
      #       payData = trainData[[ min(nTrans, iTrans)]][(transType == 'P' | transType == 'TP') & transStat ==1]
      #       payData = payData[,c('inStateTimeTransGrPay', 'outComeT' )]
      #       
      #       linMu1_test = predict(payMods[[min(nTrans, iTrans)]], type="response", what = "mu", K = 1, newdata = sampDT[, .SD, .SDcol = c(names(payData))], data= payData)
      #       linMu2_test = predict(payMods[[min(nTrans, iTrans)]], type="response", what = "mu", K = 2, newdata = sampDT[, .SD, .SDcol = c(names(payData))], data= payData)
      #       sigma1_test = predict(payMods[[min(nTrans, iTrans)]], type="response", what = "sigma", K = 1, newdata = sampDT[, .SD, .SDcol = c(names(payData))], data= payData)
      #       sigma2_test = predict(payMods[[min(nTrans, iTrans)]], type="response", what = "sigma", K = 2, newdata = sampDT[, .SD, .SDcol = c(names(payData))], data= payData)
      #       
      #       samps = payMods[[min(nTrans, iTrans)]]$prob[1]*exp(linMu1_test+sigma1_test^2/2) + payMods[[min(nTrans, iTrans)]]$prob[2]*exp(linMu2_test+sigma2_test^2/2)
      #       
      #       predSim[[iTrans]][iPol][[1]][payInds, outComeT := samps]
      #     }
      #   }else{
      #     predSim[[iTrans]][iPol][[1]][noPayInds, linkRatio  := NA]
      #     asNumericDT(predSim[[iTrans]][iPol][[1]], 'linkRatio')
      #     #selecting the ones that still need a payment simulation with nSimTrans or so
      #     
      #     # x[indsN, ][(rowInds[1] + 1):rowInds[4], inStateTimeTrans := max(c(min(c(inStateTimeTrans + addPer - 1, inStateTimeTransMax)), inStateTimeTransMin))] #removed 210420
      #     # predSim[[iTrans]][iPol][[1]][, inStateTimeTrans := max(c(min(c(inStateTimeTrans + addPer - 1, inStateTimeTransMax)), inStateTimeTransMin))] #removed 210420
      #     
      #     if(length(payInds) != 0){
      #       sampDT <- predSim[[iTrans]][iPol][[1]][payInds, ]
      #       asFactorDT(sampDT, c('inStateTimeTransGrPay', 'cumInd')) #added
      #       
      #       payData = trainData[[ min(nTrans, iTrans) ]][(transType == 'P' | transType == 'TP') & transStat ==1]
      #       payData = payData[,c('inStateTimeTransGrPay', 'cumInd', 'linkRatio' )]
      #       
      #       linMu1_testL5 = predict(payMods[[min(nTrans, iTrans)]], type="response", what = "mu", K = 1, newdata = sampDT[, .SD, .SDcol = c(names(payData))], data= payData)
      #       linMu2_testL5 = predict(payMods[[min(nTrans, iTrans)]], type="response", what = "mu", K = 2, newdata =  sampDT[, .SD, .SDcol = c(names(payData))], data= payData)
      #       linMu3_testL5 = predict(payMods[[min(nTrans, iTrans)]], type="response", what = "mu", K = 3, newdata = sampDT[, .SD, .SDcol = c(names(payData))], data= payData)
      #       linMu4_testL5 = predict(payMods[[min(nTrans, iTrans)]], type="response", what = "mu", K = 4, newdata =  sampDT[, .SD, .SDcol = c(names(payData))], data= payData)
      #       linMu5_testL5 = predict(payMods[[min(nTrans, iTrans)]], type="response", what = "mu", K = 5, newdata = sampDT[, .SD, .SDcol = c(names(payData))], data= payData)
      #       
      #       sigma1_testL5 = predict(payMods[[min(nTrans, iTrans)]], type="response", what = "sigma", K = 1, newdata =  sampDT[, .SD, .SDcol = c(names(payData))], data= payData)
      #       sigma2_testL5 = predict(payMods[[min(nTrans, iTrans)]], type="response", what = "sigma", K = 2, newdata =  sampDT[, .SD, .SDcol = c(names(payData))], data= payData)
      #       sigma3_testL5 = predict(payMods[[min(nTrans, iTrans)]], type="response", what = "sigma", K = 3, newdata =  sampDT[, .SD, .SDcol = c(names(payData))], data= payData)
      #       sigma4_testL5 = predict(payMods[[min(nTrans, iTrans)]], type="response", what = "sigma", K = 4, newdata =  sampDT[, .SD, .SDcol = c(names(payData))], data= payData)
      #       sigma5_testL5 = predict(payMods[[min(nTrans, iTrans)]], type="response", what = "sigma", K = 5, newdata =  sampDT[, .SD, .SDcol = c(names(payData))], data= payData)
      #       
      #       samps = payMods[[min(nTrans, iTrans)]]$prob[1]*exp(linMu1_testL5+sigma1_testL5^2/2) +
      #         payMods[[min(nTrans, iTrans)]]$prob[2]*exp(linMu2_testL5+sigma2_testL5^2/2) +
      #         payMods[[min(nTrans, iTrans)]]$prob[3]*exp(linMu3_testL5+sigma3_testL5^2/2) +
      #         payMods[[min(nTrans, iTrans)]]$prob[4]*exp(linMu4_testL5+sigma4_testL5^2/2) +
      #         payMods[[min(nTrans, iTrans)]]$prob[5]*exp(linMu5_testL5+sigma5_testL5^2/2) 
      #       
      #       predSim[[iTrans]][iPol][[1]][payInds, linkRatio := samps]
      #     }
      #   }
      #   count <- count + 1
      #   
      # }
      
      # Basically, the object 'addDT' is created which will be added to the polnumbs
      # that still need simulations for the next transition
      
      if(verbose) print('Adding non terminal (P) simulations to the next transition')
      
      addDT <- data.table()
      
      if(length(nSim) < (iTrans + 1)){
        nSim[[iTrans + 1]] <- list()
      }
      if(length(predSim) < (iTrans + 1)){
        predSim[[iTrans + 1]] <- list()
      }
      
      inProcTimeTransPlus1Max <- max(as.numeric(trainData[[min(nTrans, iTrans + 1)]][, inProcTimeTrans]))
      
      colNamesFitPlus1 <- colnames(coef(timeMods[[min(length(timeMods), min(nTrans, iTrans + 1))]]$fitTime))
      inProcTimeTransPlus1Min <- min(as.numeric(str_replace_all(colNamesFitPlus1[grep('inProcTimeTrans', colNamesFitPlus1)], "[a-zA-Z\\s]", ""))) - 1
      if(inProcTimeTransPlus1Min == 2) inProcTimeTransPlus1Min <- 1
      if(sum(names(trainData[[min(nTrans, iTrans + 1)]]) == 'deltRepTrans') == 1){
        deltRepTransPlus1Max <- max(as.numeric(as.character(trainData[[min(nTrans, iTrans + 1)]][, deltRepTrans])))
        deltRepTransPlus1Min <- min(as.numeric(as.character(trainData[[min(nTrans, iTrans + 1)]][, deltRepTrans])))
      }
      
      count <- 1
      
      iPol <- names(predSim[[iTrans]])[1]
      
      for(iPol in names(predSim[[iTrans]])){
        
        if(count %% 1000 == 0) print(count)
        
        nOpen <- sum(predSim[[iTrans]][iPol][[1]]$transMN == 'P')
        
        if(nOpen > 0){
          
          namesDT <- names(trainData[[min(nTrans, iTrans + 1)]])
          cumDeltNames <- namesDT[grep('cumDelt', namesDT)]
          namesDT <- namesDT[!namesDT %in% cumDeltNames]
          deltRepNames <- namesDT[grep('deltRep', namesDT)]
          namesDT <- namesDT[!namesDT %in% deltRepNames]
          deltPayNames <- namesDT[grep('delt', namesDT)]
          namesDT <- namesDT[!namesDT %in% deltPayNames]
          namesDT <- c(namesDT, deltRepNames)
          
          predSim[[iTrans]][iPol][[1]][, bookDate := bookDate + simPer*perLen]
          predSim[[iTrans]][iPol][[1]][, finYear := year(bookDate)]
          predSim[[iTrans + 1]][iPol][[1]] <- predSim[[iTrans]][iPol][[1]][transMN == 'P', ]
          
          
          
          if(sum(deltPayNames == 'delt1Pay') != 0){ #changed 040520
            if(iTrans>=1){
              predSim[[iTrans + 1]][iPol][[1]][, delt1Pay := outComeT] #changed 040520
            }else{
              # JORDY: you don't need this else statement
              #predSim[[iTrans + 1]][iPol][[1]][, delt1Pay := (linkRatio-1) * cumDelt1Pay] #changed 040520
            }
            if(!(is.null(timeSplits[[min(nTrans, iTrans + 1)]]))){
              selEl <- which(laply(timeSplits[[min(nTrans, iTrans + 1)]], function(xx) grepl('delt1Pay', names(xx)[1]))) #changed 040520
              if(length(selEl) != 0){ #changed 180520
                splits <- list(timeSplits[[min(nTrans, iTrans + 1)]][[selEl]]) #changed 040520
                names(splits)[1] <- 'delt1Pay' #changed 040520
                transform2BinnedVar(predSim[[iTrans + 1]][iPol][[1]], splits) #changed 040520
              } #changed 180520
            }
          } #changed 040520
          
          if(sum(cumDeltNames == 'cumDelt1Pay') != 0){
            if(sum(names(predSim[[iTrans + 1]][iPol][[1]]) == 'cumDelt1Pay') == 1){ #JOLIEN TODO CONTROLEER WAT DIT DOET VOOR I TRANS >1
              #JORDY, you will need the one below instead of what i have
              predSim[[iTrans + 1]][iPol][[1]][, cumDelt1Pay := outComeT + cumDelt1Pay]
              #predSim[[iTrans + 1]][iPol][[1]][, cumDelt1Pay := linkRatio * cumDelt1Pay]
            } else {
              predSim[[iTrans + 1]][iPol][[1]][, cumDelt1Pay := outComeT] 
            }
            if(!(is.null(timeSplits[[min(nTrans, iTrans + 1)]]))){
              selEl <- which(laply(timeSplits[[min(nTrans, iTrans + 1)]], function(xx) grepl('cumDelt1Pay', names(xx)[1])))
              splits <- list(timeSplits[[min(nTrans, iTrans + 1)]][[selEl]])
              names(splits)[1] <- 'cumDelt1Pay'
              transform2BinnedVar(predSim[[iTrans + 1]][iPol][[1]], splits)
            }
          }
          
          #JORDY: you can remove line below
          #predSim[[iTrans + 1]][iPol][[1]][,  cumInd := 1*(cumDelt1Pay>10000)]
          
          if(sum(deltPayNames == 'delt1PayTimeTrans') == 1){
            predSim[[iTrans + 1]][iPol][[1]][, delt1PayTimeTrans := 1]
          }
          if(sum(deltPayNames == 'delt2PayTimeTrans') == 1){
            predSim[[iTrans + 1]][iPol][[1]][, delt2PayTimeTrans := inStateTime + 1]
          }
          
          predSim[[iTrans + 1]][iPol][[1]][, inProcTimeTrans := pmin(inProcTime + 1, inProcTimeTransPlus1Max)] # Jolien should be pmax and pmin instead of max and min; should be +1; should be inProcTime
          predSim[[iTrans + 1]][iPol][[1]][, inProcTimeTrans := pmax(inProcTimeTrans, inProcTimeTransPlus1Min)]
          predSim[[iTrans + 1]][iPol][[1]][, inProcTime := inProcTime + 1]
          
          if(sum(deltPayNames == 'delt2PayTimeTrans') == 1){
            predSim[[iTrans + 1]][iPol][[1]][, delt2PayTimeTrans := min(delt2PayTimeTrans, delt2PayTimeTransPlus1Max)]
            predSim[[iTrans + 1]][iPol][[1]][, delt2PayTimeTrans := max(delt2PayTimeTrans, delt2PayTimeTransPlus1Min)]
          }
          if(sum(deltRepNames == 'deltRepTrans') == 1){
            asNumericDT(predSim[[iTrans + 1]][iPol][[1]], 'deltRepTrans')
            predSim[[iTrans + 1]][iPol][[1]][, deltRepTrans := min(deltRepTrans, deltRepTransPlus1Max)]
            predSim[[iTrans + 1]][iPol][[1]][, deltRepTrans := max(deltRepTrans, deltRepTransPlus1Min)]
            asFactorDT(predSim[[iTrans + 1]][iPol][[1]], 'deltRepTrans')
          }
          
          predSim[[iTrans + 1]][iPol][[1]][, simPer := NULL]
          predSim[[iTrans + 1]][iPol][[1]]$inStateTimeTrans <- 1
          predSim[[iTrans + 1]][iPol][[1]]$inStateTime <- 1
          predSim[[iTrans + 1]][iPol][[1]]$delt1PayTime<- 1
          #predSim[[iTrans + 1]][iPol][[1]]$inStateTimeTransGrPay  <- 1
          predSim[[iTrans + 1]][iPol][[1]]$transMN <- 'N'
          predSim[[iTrans + 1]][iPol][[1]]$outComeT <- 0.0
          predSim[[iTrans + 1]][iPol][[1]]$linkRatio <- 0.0
          predSim[[iTrans + 1]][iPol][[1]]$delt0Pay <- 0.0
          predSim[[iTrans + 1]][iPol][[1]]$delt0PayTime <- 1
          
          if(nrow(addDT) == 0){
            addDT <- predSim[[iTrans + 1]][iPol][[1]]
          } else {
            addDT <- rbind(addDT, predSim[[iTrans + 1]][iPol][[1]], fill = T )
          }
          nSim[[iTrans + 1]][iPol] <- nOpen
        }
        
        count <- count + 1
      }
      
      if(nrow(addDT) == 0){
        goOn <- FALSE
      } else {
        iTrans <- iTrans + 1
      }
    }
    return(list(predSim = predSim, nSim = nSim))
  }
}

simPathsJolien2 <- function(timeMods, payMods, trainData, timeSplits, predData, fixedTimeMax, perLen, nSims, lastTrans, payModType,verbose,maxProc = 72){ #changed 080520
  # Check input parameters and certain variables in 'trainData' and 'predData'.
  if(!is.list(timeMods)) stop("Argument 'timeMods' is a list containing the different time model fits.")
  if(!is.list(payMods)) stop("Argument 'payMods' is a list containing the different payment model fits.")
  # checkEqualLength(list(timeMods, trainData, predData))
  # checkEqualLength(list(payMods, trainData, predData))
  llply(trainData, function(xx) checkDT(xx))
  llply(predData, function(xx) checkDT(xx))
  checkWholeNumb(list(nSims, lastTrans)) #changed 080520
  checkLogicVec(list(verbose))
  checkLength(list(nSims, verbose, lastTrans), 1) #changed 080520
  checkRanges(list(nSims, lastTrans), list(c('>', 0), c('>', 0))) #changed 080520
  
  nNonNs <- laply(predData, function(xx) ifelse(nrow(xx) > 0, nrow(xx[transMN != 'N', ]), 0)) #changed 150420
  if(sum(nNonNs) > 0) stop("The transMN column of at least one element of the 'predData' argument did not all just contain rows with value 'N'. Please correct for this.") #changed 150420
  nNonPs <- laply(predData, function(xx) ifelse(nrow(xx) > 0, nrow(xx[transType != 'P', ]), 0)) #changed 290420
  if(sum(nNonPs) > 0) stop("The transMN column of at least one element of the 'testDT' argument did not all just contain rows with value 'P'. Please correct for this.") #changed 150420
  
  if(verbose) print('Setting the stage')
  
  predSim <- list()
  nSim <- list()
  predTransAll <- list()
  nTrans <- min(length(timeMods), length(predData))#Jordy
  
  # Defining the initial nSim and predSim objects
  # nSim indicates how many sims still need to be taken for the polNumb of interest,
  # while predSim contains the data sets that were used for the timeMod and payMod.
  # both objects are a list, of length the number of transitions and they contain as
  # elements as many polNumbs as still need simulations.
  
  minTrans <- NA
  
  for(iTrans in 1:nTrans){
    
    if(verbose) print(iTrans)
    #pour chaque numéro de sinistre vous pouvez voir combien de simulations il vous reste à faire (liste des éléments par claimNumb)
    nSim[[iTrans]] <- as.list(rep(nSims, nrow(predData[[iTrans]]))) #Jordy: a claimNr will just appear for the transition where it is still open (later on, not)
    predSim[[iTrans]] <- nSim[[iTrans]]#Jordy: nSim[[iTrans]]  is a list, where for each transition number, you for each polNumb how many times you need to simulate it 
    # predTransALL := données pour lesquelles vous devez prévoir
    predTransAll[[iTrans]] <- predData[[iTrans]]
    
    if(nrow(predData[[iTrans]]) > 0){
      
      if(is.na(minTrans)) minTrans <- iTrans
      
      for(iRun in 1:length(nSim[[iTrans]])){ #pour tout polNumb que vous devez simuler pour
        predSim[[iTrans]][[iRun]] <- predTransAll[[iTrans]][rep(iRun, nSim[[iTrans]][iRun]),]#Jordy: repeat each line from predData for the current polNumb, nSim times
        predSim[[iTrans]][[iRun]]$inStateTimeTrans <- as.numeric(as.character(predSim[[iTrans]][[iRun]]$inStateTimeTrans))
        #predSim[[iTrans]][[iRun]]$inStateTimeTransGrPay   <- as.numeric(as.character(predSim[[iTrans]][[iRun]]$inStateTimeTransGrPay  ))
        predSim[[iTrans]][[iRun]]$inProcTimeTrans <- as.numeric(as.character(predSim[[iTrans]][[iRun]]$inProcTimeTrans))
        predSim[[iTrans]][[iRun]]$inStateTime <- as.numeric(as.character(predSim[[iTrans]][[iRun]]$inStateTime))
        predSim[[iTrans]][[iRun]]$inProcTime <- as.numeric(as.character(predSim[[iTrans]][[iRun]]$inProcTime))
        predSim[[iTrans]][[iRun]][, simPer := 0]#Jordy: simPer is 0 because at this moment there has been no 1 step ahead simulation 
        #predsim[[iTrans]] contient la liste. Chaque liste appartient à 1 polNumb et répète les données nSims fois
      }
      predTransAll[[iTrans]] <- predData[[iTrans]][rep(1:nrow(predData[[iTrans]]), nSims), ]
      predTransAll[[iTrans]] <- predTransAll[[iTrans]][order(polNumb), ] #predTransAll contient toutes les données de predsim, sans listes
      
      names(nSim[[iTrans]]) <- predData[[iTrans]]$polNumb
      names(predSim[[iTrans]]) <- predData[[iTrans]]$polNumb
      
    }
  }
  
  addDT <- data.table()
  
  if(!is.na(minTrans)){
    
    iTrans <- minTrans
    goOn <- TRUE
    
    if(verbose) print('Looping over the different transitions')
    
    while(goOn){ #while(iTrans <= 6)
      if(verbose) cat('\n')
      if(verbose) print('////||||\\\\')
      if(verbose) print(paste('TRANSITION', iTrans))
      if(verbose) print('\\\\||||////')
      if(verbose) cat('\n')
      
      if(verbose) print(paste('nSimTot: ', sum(unlist(nSim[[iTrans]]))))
      
      #Prepping and augmenting the data for the simulations
      
      #Jolien 
      #Jordy: change trainData to the data that was use when fitting the time model, because with the sampling in fitMN it's possible that not all classes are sampled.
      # inProcTimeTransMax = max(as.numeric(as.character(trainData[[min(nTrans, iTrans)]]$inProcTimeTrans)))
      # inProcTimeTransMin= min(as.numeric(as.character(trainData[[min(nTrans, iTrans)]]$inProcTimeTrans)))
      # 
      # inStateTimeTransMax = max(as.numeric(as.character(trainData[[min(nTrans, iTrans)]]$inStateTimeTrans)))
      # inStateTimeTransMin = min(as.numeric(as.character(trainData[[min(nTrans, iTrans)]]$inStateTimeTrans)))
      # 
      # inStateTimeTransMaxPay = max(as.numeric(as.character(trainData[[min(nTrans, iTrans)]]$inStateTimeTransGrPay)))
      # inStateTimeTransMinPay = max(min(as.numeric(as.character(trainData[[min(nTrans, iTrans)]]$inStateTimeTransGrPay))),1)
      
      inStateTimeTransMaxPay <- max(as.numeric(as.character(names(table(payMods[[min(nTrans, iTrans)]]$trainDT$inStateTimeTrans))))) #changed 150420
      inProcTimeTransMaxPay <- max(as.numeric(as.character(names(table(payMods[[min(nTrans, iTrans)]]$trainDT$inProcTimeTrans))))) #changed 150420
      
      inStateTimeTransMinPay <- min(as.numeric(as.character(names(table(payMods[[min(nTrans, iTrans)]]$trainDT$inStateTimeTrans))))) #changed 150420
      inProcTimeTransMinPay <- min(as.numeric(as.character(names(table(payMods[[min(nTrans, iTrans)]]$trainDT$inProcTimeTrans))))) #changed 150420
      
      inStateTimeTransMaxTime <- max(as.numeric(as.character(names(table(timeMods[[min(nTrans, iTrans)]]$trainDT$inStateTimeTrans))))) #changed 150420
      inProcTimeTransMaxTime <- max(as.numeric(as.character(names(table(timeMods[[min(nTrans, iTrans)]]$trainDT$inProcTimeTrans))))) #changed 150420
      
      inStateTimeTransMinTime <- min(as.numeric(as.character(names(table(timeMods[[min(nTrans, iTrans)]]$trainDT$inStateTimeTrans))))) #changed 150420
      inProcTimeTransMinTime <- min(as.numeric(as.character(names(table(timeMods[[min(nTrans, iTrans)]]$trainDT$inProcTimeTrans))))) #changed 150420
      
      inStateTimeTransMin <- max(inStateTimeTransMinTime, inStateTimeTransMinPay) #changed 150420
      inProcTimeTransMin <- max(inProcTimeTransMinTime, inProcTimeTransMinPay) #changed 150420
      
      inStateTimeTransMax <- min(inStateTimeTransMaxTime, inStateTimeTransMaxPay) #changed 150420
      inProcTimeTransMax <- min(inProcTimeTransMaxTime, inProcTimeTransMaxPay) #changed 150420
      
      
      deltRepTimeTransMaxTime = max(as.numeric(as.character(names(table(timeMods[[min(nTrans, iTrans)]]$trainDT$deltRepTrans))))) #changed 150420
      deltRepTimeTransMinTime = min(as.numeric(as.character(names(table(timeMods[[min(nTrans, iTrans)]]$trainDT$deltRepTrans))))) #changed 150420
      
      deltRepTimeTransMaxPay = max(as.numeric(as.character(names(table(payMods[[min(nTrans, iTrans)]]$trainDT$deltRepTrans))))) #changed 150420
      deltRepTimeTransMinPay = min(as.numeric(as.character(names(table(payMods[[min(nTrans, iTrans)]]$trainDT$deltRepTrans))))) #changed 150420
      
      deltRepTimeTransMin <- max(deltRepTimeTransMinTime,deltRepTimeTransMinPay )
      print(deltRepTimeTransMin)
      deltRepTimeTransMax <- min(deltRepTimeTransMaxTime,deltRepTimeTransMaxPay )
      print(deltRepTimeTransMax)
      
      asNumericDT(trainData[[min(nTrans, iTrans)]], c('inStateTimeTrans', 'inProcTimeTrans'))
      
      if(nTrans >= iTrans){
        if(nrow(addDT) != 0){
          addDT[, inStateTimeMax := inStateTimeTransMax]
        }
        predTrans <- rbind(predTransAll[[iTrans]], addDT, fill = T) #JOLIEN: fill =T, to have no issues when merging (columns for first dataset are different from the ones afterwards)
      } else{
        predTrans <- addDT
        #We need to reset the predSim data
        polNumbUniq <- unique(predTrans$polNumb)# all unique polNumbs from that pred transition  dataset 
        predSim[[iTrans]] <- list()
        
        for(iRun in 1:length(polNumbUniq)){
          predSim[[iTrans]][[iRun]] <- predTrans[polNumb == polNumbUniq[iRun],]
        }
        names(predSim[[iTrans]]) <- polNumbUniq
      }
      
      predTransL <- split(predTrans, by = 'polNumb')
      #inStateTimeMax <- min(trainData[[min(nTrans, iTrans)]][1, inStateTimeMax], fixedTimeMax) 
      inStateTimeMax = fixedTimeMax[min(nTrans, iTrans)] #JOLIEN
      if(verbose) print(paste('Number of open claims of this transition:', nrow(predTrans)))
      
      nSimTrans <- nSim[[iTrans]]
      addPer <- 0# number of periods that were added to the simulations for each polNumb 
      stillOpen <- TRUE
      
      # looping until all claims have their simulated transition, i.e. untill there are no more transMN == N 
      
      while(stillOpen){
        
        ptm <- proc.time()
        #addPer <- pmin(addPer + 1, inStateTimeMax)
        addPer <- addPer + 1 #Jolien
        if(verbose) print(paste('Run', addPer))
        
        selPolNumbs <- names(unlist(nSimTrans))[unlist(nSimTrans) != 0]# the polNumbs that will be used 
        
        predTransDT <- rbindlist(predTransL, fill = T)
        runData <- predTransDT[polNumb %in% selPolNumbs, ][transMN == 'N', ]# only the lines of predTransDT where transMN is N
        
        if(addPer == 1){ #changed 050520 # Jolien, not really necessary, we did this before test set was given to this function
          asNumericDT(runData, c('inStateTimeTrans',   'inProcTimeTrans',"deltRepTrans")) #changed 050520
          runData[inStateTimeTrans > inStateTimeTransMax, inStateTimeTrans := inStateTimeTransMax] #changed 050520
          runData[inStateTimeTrans < inStateTimeTransMin, inStateTimeTrans := inStateTimeTransMin] #changed 050520
          runData[inProcTimeTrans > inProcTimeTransMax, inProcTimeTrans := inProcTimeTransMax] #changed 050520
          runData[inProcTimeTrans < inProcTimeTransMin, inProcTimeTrans := inProcTimeTransMin] #changed 050520
          runData[deltRepTrans > deltRepTimeTransMax, deltRepTrans := deltRepTimeTransMax] #changed 050520
          runData[deltRepTrans < deltRepTimeTransMin, deltRepTrans := deltRepTimeTransMin] #changed 050520
          asFactorDT(runData, c('inStateTimeTrans', 'inProcTimeTrans','fastRep', 'deltRepTrans')) #changed 050520
        } #changed 050520
        
        predTransL <- split(runData, by = 'polNumb')
        
        asFactorDT(runData, c('inStateTimeTrans', 'inProcTimeTrans'))
        if(sum(names(runData) == 'delt1PayTimeTrans') != 0) asFactorDT(runData, c('delt1PayTimeTrans'))
        if(sum(names(runData) == 'delt2PayTimeTrans') != 0) asFactorDT(runData, c('delt2PayTimeTrans'))
        
        if(nrow(runData) > 1){
          predProb <- predict(timeMods[[min(length(timeMods), min(nTrans, iTrans))]]$fitTime, newdata = runData, type = 'probs')
        } else {
          predProb <- t(as.matrix(predict(timeMods[[min(length(timeMods), min(nTrans, iTrans))]]$fitTime, newdata = runData, type = 'probs')))
        }
        
        if(ncol(predProb) < 4){ #een van de categorien die niet in de training zit
          
          fixedColNames <- c('N', 'P', 'TN', 'TP')
          missingCol <- !(fixedColNames %in% colnames(predProb))
          
          fixedColNames[missingCol]
          for(iLoop in 1:sum(missingCol)){
            predProb <- cbind(predProb, rep(0, nrow(predProb)))
            colnames(predProb)[ncol(predProb)] <- fixedColNames[missingCol][iLoop]
          }
          predProb <- predProb[, order(colnames(predProb), fixedColNames)]
        }
        
        if(verbose) print(paste0('The expected number of lines after this run: ', floor(mean(predProb[,1])*nrow(runData))))
        
        # No more non terminal payments when lastTrans is reached #changed 080520
        if(iTrans >= lastTrans){ #changed 080520
          predProb[, 4] <- predProb[, 4] + predProb[, 2] #changed 080520 - payment w terminal payment
          predProb[, 2] <- 0 #changed 080520 - payment krijgt kans nul
        } #changed 080520
        # No more staying in the state without a transition when inStateTimeMax is reached #changed 080520
        # should be replaced  inside the loop
        # if(addPer >= inStateTimeMax & iTrans < lastTrans){ #changed 080520
        #   predProb[, 2] <- predProb[, 2] + predProb[, 1]/3 #changed 080520
        #   predProb[, 3] <- predProb[, 3] + predProb[, 1]/3 #changed 080520
        #   predProb[, 4] <- predProb[, 4] + predProb[, 1]/3 #changed 080520
        #   predProb[, 1] <- 0 #changed 080520
        # } else if(addPer >= inStateTimeMax & iTrans >= lastTrans){ #changed 080520
        #   predProb[, 3] <- predProb[, 3] + predProb[, 1]/2 #changed 080520
        #   predProb[, 4] <- predProb[, 4] + predProb[, 1]/2 #changed 080520
        #   predProb[, 1] <- 0 #changed 080520
        # }
        # if(addPer >= inStateTimeMax){ #removed 080520
        #   for(iRow in 1:nrow(predProb)){ #removed 080520
        #     if(sum(predProb[iRow, ][2:4]) < 0.05){ #removed 080520
        #       predProb[iRow, 2:4] <- 1/3 #removed 080520
        #       predProb[iRow, 1] <- 0 #removed 080520
        #     } #removed 080520
        #   } #removed 080520
        # } #removed 080520
        
        if(iTrans > 1){
          pastedPredProb <- aaply(predProb, 1, function(xx) paste(xx, collapse = ''))# Jordy: transform nx4 matrix to nx1 matrix. Used to compare afterwards what are the unique probabilties 
        }
        
        runData$inStateTimeTrans <- as.numeric(as.character(runData$inStateTimeTrans))
        #runData$inStateTimeTransGrPay <- as.numeric(as.character(runData$inStateTimeTransGrPay))
        runData$inProcTimeTrans <- as.numeric(as.character(runData$inProcTimeTrans))
        
        simRun <- as.list(rep(1, nrow(runData)))
        
        # The actual simulation of the transition
        for(iRun in 1:length(selPolNumbs)){ # pour chaque polnumb unique à simuler
          if(iRun %% 1000 == 0) print(iRun)
          # print(iRun)
          indNPol <- which(predTransL[[which(names(predTransL) == selPolNumbs[iRun])]][, transMN] == 'N')
          sel <- which(runData$polNumb == selPolNumbs[iRun])
          uPredProb <- predProb[sel, ] # les prédictions pour le polnumb selectionnée
          
          #Jolien: something you've to check for each claim seperately
          #before it was done outside this loop of iRun
          if(!is.null(maxProc)){
            if(any(as.numeric(as.character(runData[polNumb == selPolNumbs[iRun]]$inProcTime))) >= maxProc){
              if((sum(is.null(nrow(uPredProb))) + sum(nrow(uPredProb) == 1)) >= 1){ #one row only
                uPredProb[3] <- uPredProb[3] + uPredProb[1]/2+ uPredProb[2]/2 #changed 080520
                uPredProb[4] <- uPredProb[4] + uPredProb[1]/2 + uPredProb[2]/2 #changed 080520
                uPredProb[1] <- 0 #changed 080520
                uPredProb[2] <- 0 #changed 080520
              }else{
                sel2 = which(as.numeric(as.character(runData[polNumb == selPolNumbs[iRun]]$inProcTime))) >= maxProx
                uPredProb[sel2,3] <- uPredProb[sel2,3] + uPredProb[sel2,1]/2+ uPredProb[sel2,2]/2 #changed 080520
                uPredProb[sel2,4] <- uPredProb[sel2,4] + uPredProb[sel2,1]/2 + uPredProb[sel2,2]/2 #changed 080520
                uPredProb[sel2,1] <- 0 #changed 080520
                uPredProb[sel2,2] <- 0 #changed 080520
              }
            }
          }
          if(any(as.numeric(as.character(runData[polNumb == selPolNumbs[iRun]]$inStateTime)) >= inStateTimeMax) & iTrans < lastTrans){ #changed 080520 #Jolien
            if((sum(is.null(nrow(uPredProb))) + sum(nrow(uPredProb) == 1)) >= 1){ #one row only
              uPredProb[2] <- uPredProb[ 2] + uPredProb[ 1]/3 #changed 080520
              uPredProb[3] <- uPredProb[ 3] + uPredProb[ 1]/3 #changed 080520
              uPredProb[4] <- uPredProb[4] + uPredProb[ 1]/3 #changed 080520
              uPredProb[1] <- 0 #changed 080520
            }else{
              uPredProb[, 2] <- uPredProb[, 2] + uPredProb[, 1]/3 #changed 080520
              uPredProb[, 3] <- uPredProb[, 3] + uPredProb[, 1]/3 #changed 080520
              uPredProb[, 4] <- uPredProb[, 4] + uPredProb[, 1]/3 #changed 080520
              uPredProb[, 1] <- 0 #changed 080520
            }
          } else if(any(as.numeric(as.character(runData[polNumb == selPolNumbs[iRun]]$inStateTime)) >= inStateTimeMax) & iTrans >= lastTrans){ #changed 080520
            if((sum(is.null(nrow(uPredProb))) + sum(nrow(uPredProb) == 1)) >= 1){ #one row only
              #volgende transitie mag niet meer
              uPredProb[ 3] <- uPredProb[3] + uPredProb[1]/2 #changed 080520
              uPredProb[ 4] <- uPredProb[4] + uPredProb[1]/2 #changed 080520
              uPredProb[ 1] <- 0 #changed 080520
            }else{
              #volgende transitie mag niet meer
              uPredProb[, 3] <- uPredProb[, 3] + uPredProb[, 1]/2 #changed 080520
              uPredProb[, 4] <- uPredProb[, 4] + uPredProb[, 1]/2 #changed 080520
              uPredProb[, 1] <- 0 #changed 080520
            }
          }
          
          predProb[sel, ] <- uPredProb # update les predictions pour ce polnumb
          
          if(iTrans > 1){
            if(length(pastedPredProb[which(runData$polNumb == selPolNumbs[iRun])]==1)){
              pastedPredProb[which(runData$polNumb == selPolNumbs[iRun])] <- paste(predProb[sel, ], collapse = '')
            }else{
              pastedPredProb[which(runData$polNumb == selPolNumbs[iRun])] <- aaply(predProb[sel, ], 1, function(xx) paste(xx, collapse = ''))
            } 
          }
          
          if(length(sel) > 1){
            uPredProb <- unique(uPredProb)# the unique predicted probabilities
          }
          
          if((sum(is.null(nrow(uPredProb))) + sum(nrow(uPredProb) == 1)) >= 1){ #hence one row only
            
            simRun[[iRun]] <- rmultinom(1, as.numeric(nSimTrans[selPolNumbs[iRun]]), prob = uPredProb) #eigenlijke simulatie
            rownames(simRun[[iRun]]) <- colnames(predProb)
            
            nSimTrans[selPolNumbs[iRun]] <- simRun[[iRun]]['N', ]# Jordy: nsimtrans keeps only the number of remaining transitions in the current state to simulate 
            
            rowInds <- cumsum(simRun[[iRun]])#Jordy: rowInds contains the repartition of the simulated transitions in the trajectories 
            x <- copy(predSim[[iTrans]][selPolNumbs[iRun]][[1]])# Jordy: x contains all predData of the polNumb for the given transition 
            
            indsN <- which(x$transMN == 'N')
            # déterminer ce qui va à P, TP ou TN
            if(simRun[[iRun]][2] != 0) x[indsN, ] <- x[indsN, ][(rowInds[1] + 1):rowInds[2], transMN := 'P']
            if(simRun[[iRun]][3] != 0) x[indsN, ] <- x[indsN, ][(rowInds[2] + 1):rowInds[3], transMN := 'TN']
            if(simRun[[iRun]][4] != 0) x[indsN, ] <- x[indsN, ][(rowInds[3] + 1):rowInds[4], transMN := 'TP']
            
            if(sum(simRun[[iRun]][2:4]) != 0){
              if(sum(names(x) == 'simPer') == 0){
                x[, simPer := NA]
                asNumericDT(x, 'simPer')
              }
              #Jolien, ne mettre à jour que les lignes qui partent i.e ou transMN %in% P, TP, TN ; rowInds[1]+1 is the index where the first transition out of the state happens
              x[indsN, ] <- x[indsN, ][(rowInds[1] + 1):rowInds[4], simPer := addPer]#Jordy: simPer is how many periods were simulated before we had a transition 
              x[indsN, ] <- x[indsN, ][(rowInds[1] + 1):rowInds[4], inStateTimeTrans := pmin(inStateTimeTrans + addPer - 1, inStateTimeTransMax)] #changed 040122
              x[indsN, ] <- x[indsN, ][(rowInds[1] + 1):rowInds[4], inStateTimeTrans := pmax(inStateTimeTrans, inStateTimeTransMin)] #changed 040122
              x[indsN, ] <- x[indsN, ][(rowInds[1] + 1):rowInds[4], inStateTime := inStateTime + addPer -1]#changed 040122
              #changed 150420 #Jordy: Why are we still updating instatetime?
              #x[indsN, ] <- x[indsN, ][(rowInds[1] + 1):rowInds[4], inStateTimeTransGrPay  := max(c(min(c(inStateTimeTransGrPay  + addPer -1, inStateTimeTransMaxPay)), inStateTimeTransMinPay))] #changed 150420
              x[indsN, ] <- x[indsN, ][(rowInds[1] + 1):rowInds[4], inProcTimeTrans := pmin(inProcTimeTrans + addPer-1 , inProcTimeTransMax)] #changed 040122
              x[indsN, ] <- x[indsN, ][(rowInds[1] + 1):rowInds[4], inProcTimeTrans := pmax(inProcTimeTrans, inProcTimeTransMin)] #changed 040122
              x[indsN, ] <- x[indsN, ][(rowInds[1] + 1):rowInds[4], inProcTime := inProcTime + addPer -1]#Jordy: Why are we still updating inProcTime? => used to update inprocTimeTrans later on 
            }
            predSim[[iTrans]][selPolNumbs[iRun]][[1]] <- copy(x)#Jordy: because x  now contains updated covariates and we are looping over the polNumbs untill each polNumb exits the state 
            predTransL[[which(names(predTransL) == selPolNumbs[iRun])]][indNPol, transMN := x[indsN, transMN]]#Jordy: need to update transMN with the transitions (or not) that happened 
          } else {
            pastedPredProbTemp <- pastedPredProb[which(runData$polNumb == selPolNumbs[iRun])]
            x <- copy(predSim[[iTrans]][selPolNumbs[iRun]][[1]])
            indsN <- which(x$transMN == 'N')
            
            pastedPredProbTempUniq <- unique(pastedPredProbTemp)
            nUniq <- length(pastedPredProbTempUniq)
            predProbTemp <- predProb[which(runData$polNumb == selPolNumbs[iRun]),]
            
            for(iUniq in 1:nUniq){
              uniqRows <- which(pastedPredProbTemp == pastedPredProbTempUniq[iUniq])
              simRunTemp <- rmultinom(1, length(uniqRows), prob = predProbTemp[uniqRows[1], ])
              x <- x[indsN[uniqRows], transMN := rep(rownames(simRunTemp), simRunTemp)]
            }
            
            predSim[[iTrans]][selPolNumbs[iRun]][[1]] <- copy(x)
            nSimTrans[selPolNumbs[iRun]] <- nrow(predSim[[iTrans]][selPolNumbs[iRun]][[1]][transMN == 'N', ]) #aantal dat er met dit model nog dienen gesimuleerd te worden
            
            predSim[[iTrans]][selPolNumbs[iRun]][[1]][indsN, simPer := addPer]
            predSim[[iTrans]][selPolNumbs[iRun]][[1]][indsN, inStateTimeTrans := pmin(inStateTimeTrans + addPer -1, inStateTimeTransMax)] #changed 040122
            predSim[[iTrans]][selPolNumbs[iRun]][[1]][indsN, inStateTimeTrans := pmax(inStateTimeTrans, inStateTimeTransMin)] #changed 040122
            predSim[[iTrans]][selPolNumbs[iRun]][[1]][indsN, inStateTime := inStateTime+addPer -1] #changed 040122
            
            #predSim[[iTrans]][selPolNumbs[iRun]][[1]][indsN, inStateTimeTransGrPay  := max(c(min(c(inStateTimeTransGrPay  + addPer -1 , inStateTimeTransMaxPay)), inStateTimeTransMinPay))] #changed 150420
            predSim[[iTrans]][selPolNumbs[iRun]][[1]][indsN, inProcTimeTrans := pmin(inProcTimeTrans + addPer -1 , inProcTimeTransMax)] #changed 040122
            predSim[[iTrans]][selPolNumbs[iRun]][[1]][indsN, inProcTimeTrans := pmax(inProcTimeTransMax, inProcTimeTransMin)] #changed 040122
            predSim[[iTrans]][selPolNumbs[iRun]][[1]][indsN, inProcTime := inProcTime +addPer-1] #changed 040122
            
            simRun[[iRun]] <- simRunTemp
            predTransL[[which(names(predTransL) == selPolNumbs[iRun])]][, transMN := x[indsN, transMN]]
            
          }
        }
        
        # Updating the values of the time vars that are in the time model
        
        predTransDT <- rbindlist(predTransL, fill = T)
        
        predTransDT$inStateTimeTrans <- as.numeric(as.character(predTransDT$inStateTimeTrans))
        #predTransDT$inStateTimeTransGrPay  <- as.numeric(as.character(predTransDT$inStateTimeTransGrPay))
        predTransDT$inProcTimeTrans <- as.numeric(as.character(predTransDT$inProcTimeTrans))
        predTransDT$inStateTime <- as.numeric(as.character(predTransDT$inStateTime))
        predTransDT$inProcTime <- as.numeric(as.character(predTransDT$inProcTime))
        #predTransDT[, inStateTimeTrans := max(c(min(c(inStateTimeTrans + 1, inStateTimeTransMax)), inStateTimeTransMin))] #changed 150420
        #predTransDT[, inStateTimeTransGrPay := max(c(min(c(inStateTimeTransGrPay  + 1, inStateTimeTransMaxPay)), inStateTimeTransMinPay))] #changed 150420
        #predTransDT[, inProcTimeTrans := max(c(min(c(inProcTimeTrans + 1, inProcTimeTransMax)), inProcTimeTransMin))] #changed 150420
        predTransDT[, inStateTimeTrans := pmin(inStateTimeTrans + 1, inStateTimeTransMax)] #changed 040122
        predTransDT[, inStateTimeTrans := pmax(inStateTimeTrans, inStateTimeTransMin)] #changed 040122
        #predTransDT[, inStateTimeTransGrPay := pmin(inStateTimeTransGrPay  + 1, inStateTimeTransMaxPay)] #changed 040122
        #predTransDT[, inStateTimeTransGrPay := pmax(inStateTimeTransGrPay, inStateTimeTransMinPay)] #changed 040122
        predTransDT[, inProcTimeTrans := pmin(inProcTimeTrans + 1, inProcTimeTransMax)] #changed 040122
        predTransDT[, inProcTimeTrans := pmax(inProcTimeTrans,  inProcTimeTransMin)] #changed 040122
        predTransDT[, inStateTime := inStateTime + 1] #changed 150420
        predTransDT[, inProcTime := inProcTime + 1] #changed 150420
        
        asFactorDT(predTransDT, c('inStateTimeTrans', 'inProcTimeTrans'))
        
        predTransL <- split(predTransDT, by = 'polNumb')
        
        if(verbose) print(paste('Number of polNumbs with sims:', sum(unlist(nSimTrans) != 0)))
        if(verbose) print(paste('Number of remaining sims:', sum(unlist(nSimTrans[selPolNumbs]))))
        if(verbose) print(paste('Time in secs of this run:', round(as.numeric((proc.time() - ptm)[3]), digits = 3)))
        if(sum(unlist(nSimTrans) != 0) == 0) stillOpen <- FALSE
      }
      
      
      if(verbose) print('Simulating payments for those transitions that require a payment')
      
      iPol <- names(predSim[[iTrans]])[1]
      count <- 1
      # @ JORDY; You will need this for loop instead of the one i have below 
      for(iPol in names(predSim[[iTrans]])[count:length(predSim[[iTrans]])]){
        
        if(count %% 1000 == 0) print(count)
        
        payInds <- which(predSim[[iTrans]][iPol][[1]]$transMN %in% c('TP', 'P'))
        noPayInds <- setdiff(1:nrow(predSim[[iTrans]][iPol][[1]]), payInds)
        predSim[[iTrans]][iPol][[1]][noPayInds, outComeT := NA]
        
        #selecting the ones that still need a payment simulation with nSimTrans or so
        
        # x[indsN, ][(rowInds[1] + 1):rowInds[4], inStateTimeTrans := max(c(min(c(inStateTimeTrans + addPer - 1, inStateTimeTransMax)), inStateTimeTransMin))] #removed 210420
        # predSim[[iTrans]][iPol][[1]][, inStateTimeTrans := max(c(min(c(inStateTimeTrans + addPer - 1, inStateTimeTransMax)), inStateTimeTransMin))] #removed 210420
        
        if(length(payInds) != 0){
          sampDT <- predSim[[iTrans]][iPol][[1]][payInds, ]
          asNumericDT(sampDT, names(sampDT)[names(sampDT) %in% c('deltRepTrans','inProcTimeTrans')]) #added
          
          sampDT[deltRepTrans > deltRepTimeTransMax, deltRepTrans := deltRepTimeTransMax] #changed 050520
          sampDT[deltRepTrans < deltRepTimeTransMin, deltRepTrans := deltRepTimeTransMin] #changed 050520
          
          sampDT[inProcTimeTrans > 4, inProcTimeTrans := 4]
          
          asFactorDT(sampDT, names(sampDT)[names(sampDT) %in% c('inStateTimeTrans', 'inProcTimeTrans', 'delt1PayTimeTrans', 'delt2PayTimeTrans','deltRepTrans','fastRep')]) #added
          sampDT[is.na(sampDT)] <- 0
          if(payModType == "MN"){
            samps <- rSplicingNP_fast(sampDT, payMods, min(nTrans, iTrans))
          }
          else if(payModType == "GLM"){
            samps <- predict(payMods[[min(nTrans, iTrans)]]$fitPay, newdata =sampDT ,type= "response")
          }
          else if(payModType == "normix"){
            samps <- unlist(predict(payMods[[min(nTrans, iTrans)]]$fitPay, newdata = sampDT, aggregate = TRUE))
          }
          else if(payModType == "splicedGPD"){
            samps <- predictSplicedPareto(sampDT, payMods, min(nTrans, iTrans))
          }
          else if(payModType == "splicedGPDSim"){
            samps <- predictSplicedParetoSim(sampDT, payMods, min(nTrans, iTrans))
          }
          else if(payModType == "truncNorm"){
            samps <- predictTruncNorm(sampDT, payMods, min(nTrans, iTrans))
          }
          else if(payModType == "truncNormSim"){
            samps <- predictTruncNormSim(sampDT, payMods, min(nTrans, iTrans))
          }
          else{
            
          }
          if(is.logical(samps)){
            print('breaking')
            break
          }
          predSim[[iTrans]][iPol][[1]][payInds, outComeT := samps]
        }
        
        count <- count + 1
        
      }
      
      
      # for(iPol in names(predSim[[iTrans]])[count:length(predSim[[iTrans]])]){
      #   
      #   if(count %% 1000 == 0) print(count)
      #   
      #   payInds <- which(predSim[[iTrans]][iPol][[1]]$transMN %in% c('TP', 'P'))
      #   noPayInds <- setdiff(1:nrow(predSim[[iTrans]][iPol][[1]]), payInds)
      #   
      #   if(iTrans == 1){
      #     predSim[[iTrans]][iPol][[1]][noPayInds, outComeT := NA]
      #     #selecting the ones that still need a payment simulation with nSimTrans or so
      #     
      #     # x[indsN, ][(rowInds[1] + 1):rowInds[4], inStateTimeTrans := max(c(min(c(inStateTimeTrans + addPer - 1, inStateTimeTransMax)), inStateTimeTransMin))] #removed 210420
      #     # predSim[[iTrans]][iPol][[1]][, inStateTimeTrans := max(c(min(c(inStateTimeTrans + addPer - 1, inStateTimeTransMax)), inStateTimeTransMin))] #removed 210420
      #     
      #     if(length(payInds) != 0){
      #       sampDT <- predSim[[iTrans]][iPol][[1]][payInds, ]
      #       asFactorDT(sampDT, c('inStateTimeTransGrPay')) #added
      #       
      #       payData = trainData[[ min(nTrans, iTrans)]][(transType == 'P' | transType == 'TP') & transStat ==1]
      #       payData = payData[,c('inStateTimeTransGrPay', 'outComeT' )]
      #       
      #       linMu1_test = predict(payMods[[min(nTrans, iTrans)]], type="response", what = "mu", K = 1, newdata = sampDT[, .SD, .SDcol = c(names(payData))], data= payData)
      #       linMu2_test = predict(payMods[[min(nTrans, iTrans)]], type="response", what = "mu", K = 2, newdata = sampDT[, .SD, .SDcol = c(names(payData))], data= payData)
      #       sigma1_test = predict(payMods[[min(nTrans, iTrans)]], type="response", what = "sigma", K = 1, newdata = sampDT[, .SD, .SDcol = c(names(payData))], data= payData)
      #       sigma2_test = predict(payMods[[min(nTrans, iTrans)]], type="response", what = "sigma", K = 2, newdata = sampDT[, .SD, .SDcol = c(names(payData))], data= payData)
      #       
      #       samps = payMods[[min(nTrans, iTrans)]]$prob[1]*exp(linMu1_test+sigma1_test^2/2) + payMods[[min(nTrans, iTrans)]]$prob[2]*exp(linMu2_test+sigma2_test^2/2)
      #       
      #       predSim[[iTrans]][iPol][[1]][payInds, outComeT := samps]
      #     }
      #   }else{
      #     predSim[[iTrans]][iPol][[1]][noPayInds, linkRatio  := NA]
      #     asNumericDT(predSim[[iTrans]][iPol][[1]], 'linkRatio')
      #     #selecting the ones that still need a payment simulation with nSimTrans or so
      #     
      #     # x[indsN, ][(rowInds[1] + 1):rowInds[4], inStateTimeTrans := max(c(min(c(inStateTimeTrans + addPer - 1, inStateTimeTransMax)), inStateTimeTransMin))] #removed 210420
      #     # predSim[[iTrans]][iPol][[1]][, inStateTimeTrans := max(c(min(c(inStateTimeTrans + addPer - 1, inStateTimeTransMax)), inStateTimeTransMin))] #removed 210420
      #     
      #     if(length(payInds) != 0){
      #       sampDT <- predSim[[iTrans]][iPol][[1]][payInds, ]
      #       asFactorDT(sampDT, c('inStateTimeTransGrPay', 'cumInd')) #added
      #       
      #       payData = trainData[[ min(nTrans, iTrans) ]][(transType == 'P' | transType == 'TP') & transStat ==1]
      #       payData = payData[,c('inStateTimeTransGrPay', 'cumInd', 'linkRatio' )]
      #       
      #       linMu1_testL5 = predict(payMods[[min(nTrans, iTrans)]], type="response", what = "mu", K = 1, newdata = sampDT[, .SD, .SDcol = c(names(payData))], data= payData)
      #       linMu2_testL5 = predict(payMods[[min(nTrans, iTrans)]], type="response", what = "mu", K = 2, newdata =  sampDT[, .SD, .SDcol = c(names(payData))], data= payData)
      #       linMu3_testL5 = predict(payMods[[min(nTrans, iTrans)]], type="response", what = "mu", K = 3, newdata = sampDT[, .SD, .SDcol = c(names(payData))], data= payData)
      #       linMu4_testL5 = predict(payMods[[min(nTrans, iTrans)]], type="response", what = "mu", K = 4, newdata =  sampDT[, .SD, .SDcol = c(names(payData))], data= payData)
      #       linMu5_testL5 = predict(payMods[[min(nTrans, iTrans)]], type="response", what = "mu", K = 5, newdata = sampDT[, .SD, .SDcol = c(names(payData))], data= payData)
      #       
      #       sigma1_testL5 = predict(payMods[[min(nTrans, iTrans)]], type="response", what = "sigma", K = 1, newdata =  sampDT[, .SD, .SDcol = c(names(payData))], data= payData)
      #       sigma2_testL5 = predict(payMods[[min(nTrans, iTrans)]], type="response", what = "sigma", K = 2, newdata =  sampDT[, .SD, .SDcol = c(names(payData))], data= payData)
      #       sigma3_testL5 = predict(payMods[[min(nTrans, iTrans)]], type="response", what = "sigma", K = 3, newdata =  sampDT[, .SD, .SDcol = c(names(payData))], data= payData)
      #       sigma4_testL5 = predict(payMods[[min(nTrans, iTrans)]], type="response", what = "sigma", K = 4, newdata =  sampDT[, .SD, .SDcol = c(names(payData))], data= payData)
      #       sigma5_testL5 = predict(payMods[[min(nTrans, iTrans)]], type="response", what = "sigma", K = 5, newdata =  sampDT[, .SD, .SDcol = c(names(payData))], data= payData)
      #       
      #       samps = payMods[[min(nTrans, iTrans)]]$prob[1]*exp(linMu1_testL5+sigma1_testL5^2/2) +
      #         payMods[[min(nTrans, iTrans)]]$prob[2]*exp(linMu2_testL5+sigma2_testL5^2/2) +
      #         payMods[[min(nTrans, iTrans)]]$prob[3]*exp(linMu3_testL5+sigma3_testL5^2/2) +
      #         payMods[[min(nTrans, iTrans)]]$prob[4]*exp(linMu4_testL5+sigma4_testL5^2/2) +
      #         payMods[[min(nTrans, iTrans)]]$prob[5]*exp(linMu5_testL5+sigma5_testL5^2/2) 
      #       
      #       predSim[[iTrans]][iPol][[1]][payInds, linkRatio := samps]
      #     }
      #   }
      #   count <- count + 1
      #   
      # }
      
      # Basically, the object 'addDT' is created which will be added to the polnumbs
      # that still need simulations for the next transition
      
      if(verbose) print('Adding non terminal (P) simulations to the next transition')
      
      addDT <- data.table()
      
      if(length(nSim) < (iTrans + 1)){
        nSim[[iTrans + 1]] <- list()
      }
      if(length(predSim) < (iTrans + 1)){
        predSim[[iTrans + 1]] <- list()
      }
      
      inProcTimeTransPlus1Max <- max(as.numeric(trainData[[min(nTrans, iTrans + 1)]][, inProcTimeTrans]))
      
      colNamesFitPlus1 <- colnames(coef(timeMods[[min(length(timeMods), min(nTrans, iTrans + 1))]]$fitTime))
      inProcTimeTransPlus1Min <- min(as.numeric(str_replace_all(colNamesFitPlus1[grep('inProcTimeTrans', colNamesFitPlus1)], "[a-zA-Z\\s]", ""))) - 1
      if(inProcTimeTransPlus1Min == 2) inProcTimeTransPlus1Min <- 1
      if(sum(names(trainData[[min(nTrans, iTrans + 1)]]) == 'deltRepTrans') == 1){
        deltRepTransPlus1Max <- max(as.numeric(as.character(trainData[[min(nTrans, iTrans + 1)]][, deltRepTrans])))
        deltRepTransPlus1Min <- min(as.numeric(as.character(trainData[[min(nTrans, iTrans + 1)]][, deltRepTrans])))
      }
      
      count <- 1
      
      iPol <- names(predSim[[iTrans]])[1]
      
      for(iPol in names(predSim[[iTrans]])){
        
        if(count %% 1000 == 0) print(count)
        
        nOpen <- sum(predSim[[iTrans]][iPol][[1]]$transMN == 'P')
        
        if(nOpen > 0){
          
          namesDT <- names(trainData[[min(nTrans, iTrans + 1)]])
          cumDeltNames <- namesDT[grep('cumDelt', namesDT)]
          namesDT <- namesDT[!namesDT %in% cumDeltNames]
          deltRepNames <- namesDT[grep('deltRep', namesDT)]
          namesDT <- namesDT[!namesDT %in% deltRepNames]
          deltPayNames <- namesDT[grep('delt', namesDT)]
          namesDT <- namesDT[!namesDT %in% deltPayNames]
          namesDT <- c(namesDT, deltRepNames)
          
          predSim[[iTrans]][iPol][[1]][, bookDate := bookDate + simPer*perLen]
          predSim[[iTrans]][iPol][[1]][, finYear := year(bookDate)]
          predSim[[iTrans + 1]][iPol][[1]] <- predSim[[iTrans]][iPol][[1]][transMN == 'P', ]
          
          
          
          if(sum(deltPayNames == 'delt1Pay') != 0){ #changed 040520
            if(iTrans>=1){
              predSim[[iTrans + 1]][iPol][[1]][, delt1Pay := outComeT] #changed 040520
            }else{
              # JORDY: you don't need this else statement
              #predSim[[iTrans + 1]][iPol][[1]][, delt1Pay := (linkRatio-1) * cumDelt1Pay] #changed 040520
            }
            if(!(is.null(timeSplits[[min(nTrans, iTrans + 1)]]))){
              selEl <- which(laply(timeSplits[[min(nTrans, iTrans + 1)]], function(xx) grepl('delt1Pay', names(xx)[1]))) #changed 040520
              if(length(selEl) != 0){ #changed 180520
                splits <- list(timeSplits[[min(nTrans, iTrans + 1)]][[selEl]]) #changed 040520
                names(splits)[1] <- 'delt1Pay' #changed 040520
                transform2BinnedVar(predSim[[iTrans + 1]][iPol][[1]], splits) #changed 040520
              } #changed 180520
            }
          } #changed 040520
          
          if(sum(cumDeltNames == 'cumDelt1Pay') != 0){
            if(sum(names(predSim[[iTrans + 1]][iPol][[1]]) == 'cumDelt1Pay') == 1){ #JOLIEN TODO CONTROLEER WAT DIT DOET VOOR I TRANS >1
              #JORDY, you will need the one below instead of what i have
              predSim[[iTrans + 1]][iPol][[1]][, cumDelt1Pay := outComeT + cumDelt1Pay]
              #predSim[[iTrans + 1]][iPol][[1]][, cumDelt1Pay := linkRatio * cumDelt1Pay]
            } else {
              predSim[[iTrans + 1]][iPol][[1]][, cumDelt1Pay := outComeT] 
            }
            if(!(is.null(timeSplits[[min(nTrans, iTrans + 1)]]))){
              selEl <- which(laply(timeSplits[[min(nTrans, iTrans + 1)]], function(xx) grepl('cumDelt1Pay', names(xx)[1])))
              splits <- list(timeSplits[[min(nTrans, iTrans + 1)]][[selEl]])
              names(splits)[1] <- 'cumDelt1Pay'
              transform2BinnedVar(predSim[[iTrans + 1]][iPol][[1]], splits)
            }
          }
          
          #JORDY: you can remove line below
          #predSim[[iTrans + 1]][iPol][[1]][,  cumInd := 1*(cumDelt1Pay>10000)]
          
          if(sum(deltPayNames == 'delt1PayTimeTrans') == 1){
            predSim[[iTrans + 1]][iPol][[1]][, delt1PayTimeTrans := 1]
          }
          if(sum(deltPayNames == 'delt2PayTimeTrans') == 1){
            predSim[[iTrans + 1]][iPol][[1]][, delt2PayTimeTrans := inStateTime + 1]
          }
          
          predSim[[iTrans + 1]][iPol][[1]][, inProcTimeTrans := pmin(inProcTime + 1, inProcTimeTransPlus1Max)] # Jolien should be pmax and pmin instead of max and min; should be +1; should be inProcTime
          predSim[[iTrans + 1]][iPol][[1]][, inProcTimeTrans := pmax(inProcTimeTrans, inProcTimeTransPlus1Min)]
          predSim[[iTrans + 1]][iPol][[1]][, inProcTime := inProcTime + 1]
          
          if(sum(deltPayNames == 'delt2PayTimeTrans') == 1){
            #predSim[[iTrans + 1]][iPol][[1]][, delt2PayTimeTrans := min(delt2PayTimeTrans, delt2PayTimeTransPlus1Max)]
            #predSim[[iTrans + 1]][iPol][[1]][, delt2PayTimeTrans := max(delt2PayTimeTrans, delt2PayTimeTransPlus1Min)]
            predSim[[iTrans + 1]][iPol][[1]][, delt2PayTimeTrans := pmin(delt2PayTimeTrans, delt2PayTimeTransPlus1Max)]#changed 040122 (was not used in thesis)
            predSim[[iTrans + 1]][iPol][[1]][, delt2PayTimeTrans := pmax(delt2PayTimeTrans, delt2PayTimeTransPlus1Min)]#changed 040122 (was not used in thesis)
          }
          if(sum(deltRepNames == 'deltRepTrans') == 1){
            asNumericDT(predSim[[iTrans + 1]][iPol][[1]], 'deltRepTrans')
            #predSim[[iTrans + 1]][iPol][[1]][, deltRepTrans := min(deltRepTrans, deltRepTransPlus1Max)]
            #predSim[[iTrans + 1]][iPol][[1]][, deltRepTrans := max(deltRepTrans, deltRepTransPlus1Min)]
            predSim[[iTrans + 1]][iPol][[1]][, deltRepTrans := pmin(deltRepTrans, deltRepTransPlus1Max)]#changed 040122 (not really necessary i think, since for each simulation of iPol it's the same; but in this way we're sure :)
            predSim[[iTrans + 1]][iPol][[1]][, deltRepTrans := pmax(deltRepTrans, deltRepTransPlus1Min)]#changed 040122
            asFactorDT(predSim[[iTrans + 1]][iPol][[1]], 'deltRepTrans')
          }
          
          predSim[[iTrans + 1]][iPol][[1]][, simPer := NULL]
          predSim[[iTrans + 1]][iPol][[1]]$inStateTimeTrans <- 1
          predSim[[iTrans + 1]][iPol][[1]]$inStateTime <- 1
          predSim[[iTrans + 1]][iPol][[1]]$delt1PayTime<- 1
          #predSim[[iTrans + 1]][iPol][[1]]$inStateTimeTransGrPay  <- 1
          predSim[[iTrans + 1]][iPol][[1]]$transMN <- 'N'
          predSim[[iTrans + 1]][iPol][[1]]$outComeT <- 0.0
          predSim[[iTrans + 1]][iPol][[1]]$linkRatio <- 0.0
          predSim[[iTrans + 1]][iPol][[1]]$delt0Pay <- 0.0
          predSim[[iTrans + 1]][iPol][[1]]$delt0PayTime <- 1
          
          if(nrow(addDT) == 0){
            addDT <- predSim[[iTrans + 1]][iPol][[1]]
          } else {
            addDT <- rbind(addDT, predSim[[iTrans + 1]][iPol][[1]], fill = T )
          }
          nSim[[iTrans + 1]][iPol] <- nOpen
        }
        
        count <- count + 1
      }
      
      if(nrow(addDT) == 0){
        goOn <- FALSE
      } else {
        iTrans <- iTrans + 1
      }
    }
    return(list(predSim = predSim, nSim = nSim))
  }
}