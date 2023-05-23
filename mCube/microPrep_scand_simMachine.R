## todo: uniformize the lag defs of addLaggedVars and addActualsPart1

## microPrep
#' Note that the term 'period' refers to the value of the \code{perLen} argument in this description.
#' microPrep is a function that first changes some variable names in case \code{inputDT} has 'DANS' as \code{source}.
#' Next, the following new variables are added to the given dataset:
#'    \code{inProcTime}: Number of periods between \code{bookDate} and \code{repDate}, hence it corresponds to the total number of periods since the claim is reported.
#'    \code{inProcTimeMax}: Max number of periods between \code{bookDate} and \code{repDate} for the claim of interest (\code{polNumb}).
#'                          This corresponds to the number of periods between the reporting date and the closing date if the claim is closed, or
#'                          if the claim is open, the latest date for which a line corresponding to the claim of interest was found in the database.
#'    \code{procStat}: Censoring/status indicator of the complete process of the lifetime of the claim.
#'                     If equal to 1 (0), the claim is closed (open) at the moment that the evaluation moment.
#'    \code{ind}: Index of the chronological order of each line for a given \code{polNumb}, starting from value 1.
#'    \code{nLines}: Total number of lines observed in the database for a given \code{polNumb}.
#'    \code{vzCodeGroup}: The group to which the verzendcode belongs.
#'    \code{TotNumbVzCodeGroups}: Total number of verzendcodes seen for the policynumber.
#'    \code{CumNumbVzCodeGroups}: Cumulative number of verzendcodes seen for the policynumber.
#'    \code{expectIncur}: The expected value of the incurred, defined as expectIncur[t]=incur[t-1] - delt1Pay[t].
#'    \code{excessIncur}: The excess in incurred, defined as excessIncur[t]=expectIncur[t] - incur[t].
#'    \code{excessIncurInd}: Indicator variable showing whether or not the excessIncur is different from zero.
#'                           If equal to 1 (0), the excessIncur is different from (equal to) zero.
#'    \code{deltRep}: Number of periods between \code{accDate} and \code{repDate}.
#'    \code{fastRep}: Indicator variable showing whether or not the claim was reported on exactly the same date as the accident had occurred.
#'    \code{lastPay}: Indicator variable showing at which line the last payment for the given dump from the database occurred (value = 1 if the lines corresponds to the last payment, 0 elsewise).
#'    \code{maxNumbIncur}: Total number of changes in incurred recorded in the database for the given \code{polNumb}.
#'    \code{lastIncur}: Indicator variable showing at which line the last change in incurred for the given dump from the database occurred (value = 1 if the lines corresponds to the last change in incurred, 0 elsewise).
#'    \code{transType}: Categorical variable showing which kind of transition took place for the line in question.
#'                      If \code{transType} equals 'P', then only a payment took place.
#'                      If \code{transType} equals 'I', then only a change in incurred took place.
#'                      If \code{transType} equals 'PI', then a payment and a change in incurred took place.
#'                      If \code{transType} equals 'TP', then only a terminal payment took place.
#'                      If \code{transType} equals 'TN', then a transition to a terminal state without payment took place.
#'    \code{indCum}: Indicator variable showing the cumulative number of time points at which payment was done and/or a change in the incurred loss took place.
#' Afterwards, some faulty observations are removed and the dataset is ordered.
#' In case of multiple lines pertaining to the same polNumb and inProcTime combination, only the last line is kept.
#' Concerning the verzendCodes:
#' - The latest value for the \code{vzCode} is generalized for all the lines for each \code{polNumb}, if the argument \code{lastVzCode} is TRUE.
#' - Only the verzendcodes mentioned in \code{selVzCode} are selected in the final dataset. In case \code{selVzCode} is NULL, no selection is made and all verzendCodes are still present.
#' - Only the recent cases, e.g. the ones without an old claim number are selected in the final dataset.
#' The small payments (indicated by \code{minPayVal}) are removed from the dataset. Afterwards, to make sure that all variables are well defined,
#' the function createCleanIncrVars is applied again (see its description for details).
#'
#' @param inputDT Dataset used in this function.
#' @param perLen Length of the timeframe expressed in number of days - default value is 30.
#' @param maxLag Maximal lag - default value is 1.
#' @param minPayVal Smallest value from which on a payment is considered - default value is NULL.
#' @param verbose Boolean indicating whether output should be printed or not - default value is TRUE.
#' @param remLoneChgIncur Boolean that indicates whether the lines with a change in the incurred without a payment being done, should be removed or not - default value is TRUE.
#' @param justLag1Pay Boolean indicating whether the small payments (indicated by \code{minPayVal}) are only removed for \code{delt1Pay} and not for the higher lags (\code{delt2Pay},..) - default value is TRUE.
#' @param lastVzCode Boolean indicating whether or not the latest value of the verzendcode for the polNumb is generalized for all lines of that polNumb.
#' @param selVzCode List of the verzendcodes that should be added to the data.
#' @param source Source from which the \code{inputDT} comes - default value is 'DANS'.
#'
#' @return inputDT_DC, the original dataset on which some data prepping is done
#
microPrep <- function(inputDT, perLen = 30, maxLag = 1, minPayVal = NULL, verbose = TRUE, remLoneChgIncur = TRUE, justLag1Pay = TRUE, lastVzCode = TRUE, selVzCode = NULL, source = 'DANS'){ #changed 290420
  # Check input parameters and certain variables in 'inputDT'.
  checkDT(inputDT, c('cumPay', 'incur'))
  checkNumVec(list(perLen, maxLag))
  checkCharVec(list(source))
  checkLogicVec(list(verbose, remLoneChgIncur))
  checkLength(list(perLen, maxLag, source, verbose, remLoneChgIncur), 1)
  checkRanges(list(maxLag, perLen), list(c('>', -1), c('>', 0)))
  checkWholeNumb(list(maxLag))
  if(!is.null(minPayVal)){
    checkNumVec(list(minPayVal))
    checkLength(list(minPayVal), 1)
    checkRanges(list(minPayVal), list(c('>', 0)))
  }
  if(!is.null(selVzCode)){
    checkCharVec(list(selVzCode))
  }
  
  inputDT_DC <- copy(inputDT)
  
  if(source == 'DANS'){
    
    if(verbose) print("This will take a while. Why don't you grab a cup/glass of your favorite beverage? ;-)")
    
    # Changing variable names.
    if(verbose) print("Changing variable names.")
    setnames(inputDT_DC, 'schadenummer', 'polNumb', skip_absent = TRUE) #changed 230420
    asCharacterDT(inputDT_DC, 'polNumb')
    setnames(inputDT_DC, 'staat_schade', 'procStat', skip_absent = TRUE) #changed 230420
    setnames(inputDT_DC, 'vz_code_schade', 'vzCode', skip_absent = TRUE) #changed 230420
    setnames(inputDT_DC, 'schade_dat', 'accDate', skip_absent = TRUE) #changed 230420
    setnames(inputDT_DC, 'meld_dat', 'repDate', skip_absent = TRUE) #changed 230420
    setnames(inputDT_DC, 'comp_dat', 'bookDate', skip_absent = TRUE) #changed 230420
    setnames(inputDT_DC, 'afm_dat', 'closedDate', skip_absent = TRUE) #changed 230420
    
    #Defining new variables.
    if(verbose) print("Defining new variables.")
    inputDT_DC[, inProcTime := ceiling(diffDays/perLen)]
    inputDT_DC[, inProcTimeMax := max(inProcTime), by = polNumb]
    inputDT_DC[, diffDaysRep := (repDate - accDate)]
    asNumericDT(inputDT_DC, 'diffDaysRep')
    inputDT_DC[, repPeriod := ceiling(diffDaysRep/perLen)]
    
    asCharacterDT(inputDT_DC, 'procStat')
    inputDT_DC[procStat == 'A', procStat := '1']
    inputDT_DC[procStat == 'L', procStat := '0']
    asNumericDT(inputDT_DC, c('procStat', 'inProcTime', 'inProcTimeMax', 'repPeriod'))
    
    # Removing some faulty observations and ordering the dataset.
    if(verbose) print("Removing some faulty observations and ordering the dataset.")
    # A faulty observation is one
    inputDT_DC <- inputDT_DC[diffDays >= 0 & diffDaysRep >= 0, ]
    inputDT_DC <- unique(inputDT_DC)
    inputDT_DC <- inputDT_DC[order(polNumb, accDate, inProcTime, inProcTimeMax),]
    
    # In case of multiple lines pertaining to the same 'polNumb' and 'inProcTime' combination, only the last line is kept.
    if(verbose) print("In case of multiple lines pertaining to the same 'polNumb' and 'inProcTime' combination, only the last line is kept.")
    # Through the function addFirstLast, the data is sorted based on the 'polNumb' and 'inProcTime'.
    # Moreover, a new variable 'last' is defined, which is one for each last unique row, and zero otherwise.
    # Selecting the lines with 'last' being equal to 1, gives the desired result.
    inputDT_DC <- addFirstLast(inputDT_DC, c('polNumb', 'inProcTime'), F, T)
    inputDT_DC <- inputDT_DC[last == 1, .(diffDays, incur, verw_regres, verw_regres_WA, bedr_res_derden, bedr_res_eigen,
                                          cumPay, bet_eigen, tot_bet_derden, tot_kosten_eigen, tot_kstn_bgk, tot_kstn_derden, tot_kstn_verh, tot_ontv_regres,
                                          bookDate, repDate, accDate, procStat, closedDate, inProcTime, inProcTimeMax, diffDaysRep, repPeriod, schnr_oud,
                                          vzCode), by = polNumb]
    
    # Defining the variables 'ind' and 'nLines'.
    inputDT_DC[, ind := 1:.N, by = polNumb]
    inputDT_DC <- merge(inputDT_DC, inputDT_DC[, .(nLines = .N), by = polNumb], all.x = TRUE)
    
    # Making sure that the first payment is sufficiently large. #changed 12052020
    if(verbose) print("Making sure that the first payment is sufficiently.")  #changed 12052020
    inputDT_DC <- mergeFirstCumPay(inputDT_DC, minPayVal, verbose)
    
    # Select only the recent cases, e.g. the ones without an old claim number. Afterwards, the variable 'schnr_oud' is removed.
    # inputDT_DC <- copy(inputDT_DC[is.na(schnr_oud), ])
    # inputDT_DC[, schnr_oud := NULL]
    
    # Define the temporary variable 'lastVzCodeVar', which is for each 'polNumb' its latest 'vzCode'.
    inputDT_DC[ind == nLines, lastVzCodeVar := vzCode, by = polNumb] #changed 110520
    inputDT_DC[, lastVzCodeVar := na.locf(lastVzCodeVar, fromLast = TRUE), by = polNumb] #changed 110520
    
    # If 'lastVzCode' is TRUE, the latest value for the 'vzCode' is generalized for all the lines for each 'polNumb'.
    if(lastVzCode == TRUE){ #changed 290420
      inputDT_DC <- inputDT_DC[, vzCode := lastVzCodeVar] #changed 110520
    } #changed 290420
    # Select the verzendcodes of interest.
    if(!is.null(selVzCode)){ #changed 290420
      inputDT_DC <- copy(inputDT_DC[vzCode %in% selVzCode, ]) #changed 290420
    } #changed 290420
    inputDT_DC[,lastVzCodeVar := NULL] #changed 110520
    
    inputDT_DC[vzCode %in% c('CN', 'EN', 'DN'), vzCodeGroup := 'WD'] #changed 290420
    inputDT_DC[vzCode %in% c('ED', 'CS', 'CC', 'CE', 'CD', 'DC', 'DD', 'DE', 'EE', 'CF', 'DF', 'EF'), vzCodeGroup := 'MD'] #changed 290420
    inputDT_DC[vzCode %in% c('EL', 'DL'), vzCodeGroup := 'BID'] #changed 290420
    inputDT_DC[vzCode %in% c('NA', 'NC', 'NL', 'NK'), vzCodeGroup := 'BIANS'] #changed 290420
    inputDT_DC[vzCode %in% c('FH', 'FL'), vzCodeGroup := 'BIAVB'] #changed 290420
    inputDT_DC[vzCode %in% c('HC', 'HM', 'HT', 'IP', 'IS'), vzCodeGroup := 'BUS'] #changed 290420
    inputDT_DC[vzCode %in% c('GD'), vzCodeGroup := 'COM'] #changed 290420
    asFactorDT(inputDT_DC, 'vzCodeGroup') #changed 290420
    
    # if(!is.null(selVzCode)){
    #   if(lastVzCode = TRUE){
    #     inputDT_DC <- copy(inputDT_DC[lastVzCode %in% selVzCode, ])
    #   } else {
    #     inputDT_DC <- copy(inputDT_DC[vzCode %in% selVzCode, ])
    #   }
    # }
    
    # Verbose statements are embedded in the next functions.
    
    # The next line of code is needed for the indicator variables and 'delt0pay'. #changed 200520
    # Moreover, the lines where only the incurred changes are removed (when 'remLoneChgIncur' is TRUE).
    inputDT_DC <- createCleanIncrVars(inputDT_DC, 0, maxLag, perLen, remLoneChgIncur, verbose) #changed 200520
    
    # Update the incurred after a payment in case this was not done by the claim handlers.
    inputDT_DC[incurInd == 0 & payInd == 1, newCol := delt0Pay] #changed 200520
    # Note that the first value of delt0Pay is NA for new 'polNumb's.
    inputDT_DC[,newGroup := cumsum(is.na(newCol))]
    inputDT_DC[!is.na(newCol),cum_sum := cumsum(newCol), by = newGroup]
    inputDT_DC[!is.na(cum_sum), newIncur := incur - cum_sum]
    inputDT_DC[is.na(cum_sum), newIncur := incur]
    inputDT_DC[,incur:=newIncur ]
    # In case there is no change in icurred / a payment is done on the last line of a 'polNumb',
    # the incurred on the last line should equal the incurred on the line above.
    inputDT_DC[,beforeLastIncur := head(tail(incur, n = 2), n = 1), by = polNumb]
    inputDT_DC[incurInd == 0 & payInd == 0 & ind == nLines, incur := beforeLastIncur, by = polNumb]## this line should be made more robust but is ok for now
    # inputDT_DC[ind==nLines,] #removed 11052020
    inputDT_DC[, newGroup := NULL]
    inputDT_DC[, newCol := NULL]
    inputDT_DC[, newIncur := NULL]
    inputDT_DC[, cum_sum := NULL]
    inputDT_DC[, beforeLastIncur := NULL]
    
    # Define the variable 'expectIncur'.
    inputDT_DC[incurInd == 1, expectIncur := shift(incur) - delt0Pay, by = polNumb] #changed 200520
    inputDT_DC[, expectIncur := na.locf(expectIncur, na.rm = FALSE), by = polNumb]
    inputDT_DC[ind == 1, expectIncur := 0, by = polNumb]
    
    #inputDT_DC[is.na(delt0Pay),expectIncur := shift(incur), by = 'polNumb'] #changed 200520
    
    # Define the variables 'excessIncur' and 'excessIncurInd'.
    inputDT_DC[, excessIncur := incur - expectIncur]
    inputDT_DC[, excessIncurInd := as.integer(abs(excessIncur) > minPayVal)]
    
    # After redefining the incurred, the variables 'incurInd', 'incurIndCum',
    # 'delt1Incur', 'delt1IncurTime', ... should be redefined as well.
    for(iLag in 0:maxLag){ #changed 200520
      inputDT_DC[, paste0(paste0('delt', iLag), 'Pay') := NULL]
      inputDT_DC[, paste0(paste0('delt', iLag), 'Incur'):= NULL]
      inputDT_DC[, paste0(paste0('delt', iLag), 'PayTime'):= NULL]
      inputDT_DC[, paste0(paste0('delt', iLag), 'IncurTime'):= NULL]
    }
    # inputDT_DC[, delt0PayTime := NULL] #removed 200520
    # inputDT_DC[ ,delt0IncurTime := NULL] #removed 200520
    inputDT_DC[, ind := NULL]
    inputDT_DC[, maxNumbPay := NULL]
    inputDT_DC[, payInd := NULL]
    inputDT_DC[, incurInd := NULL]
    inputDT_DC[,incurIndCum := NULL]
    inputDT_DC[, payIndCum := NULL]
    
    # createCleanIncrVars is function that first applies the function 'remLinesNoCh' (see its description for details) using
    # the paramater 'remLoneChgIncur'. Next, incremental variables are created by applying the function 'addLaggedVars'
    # (see its description for details), using the parameters 'minLag' (being 1 here), 'maxLag', 'perLen' and 'verbose'.
    # Finally, the variable 'maxNumbPay' is defined, which is the total number of payments recorded in the database for
    # the given 'polNumb'.
    inputDT_DC <- createCleanIncrVars(inputDT_DC, 0, maxLag, perLen, remLoneChgIncur, verbose) #changed 200520
    
    # Removing the small payments (indicated by minPayVal) from the database.
    if(verbose) print("Removing the small payments (indicated by minPayVal) from the database.")
    # This should go through the while loop up until no changes are observed.
    polNumbs <- unique(inputDT_DC$polNumb)
    if(!is.null(minPayVal)){
      if(justLag1Pay){
        res <- mergePayIncr(inputDT_DC, polNumbs, minPayVal, perLen, 0, verbose) #changed 200520
        inputDT_DC <- res$inputDT
      } else {
        # This part can still be optimized by adapting the function mergePayIncr to take just the 'polNumbs' into account
        # for all steps in this function, and then to create temporary DT objects that collect the finalized part
        # of the dataset and the part that still need some work.
        polNumbsList <- list()
        goOn <- TRUE
        while(goOn){
          nBefore <- nrow(inputDT_DC)
          for(iLag in 0:maxLag){ #changed 200520
            res <- mergePayIncr(inputDT_DC, polNumbs, minPayVal, perLen, iLag, verbose)
            inputDT_DC <- res$inputDT
            if(length(res$polNumbs)){
              polNumbsList[[iLag]] <- res$polNumbs
            } else {
              polNumbsList[[iLag]] <- NA
            }
          }
          
          nAfter <- nrow(inputDT_DC)
          # if(verbose) print(paste0('nRows before: ', nBefore))
          # if(verbose) print(paste0('nRows after: ', nAfter))
          
          if(nAfter == nBefore){
            goOn <- FALSE
          } else {
            polNumbs <- unique(unlist(polNumbsList))
            polNumbs <- polNumbs[!is.na(polNumbs)]
          }
        }
      }
    }
    
    # Create total and cumulative number of groups of verzendcodes.
    #inputDT_DC[,TotNumbVzCodes := length(unique(vzCode)), by = 'polNumb'] #removed 290420
    #inputDT_DC[,CumNumbVzCodes:= cumsum((vzCode != shift(vzCode,  fill = "", type = "lag"))), by = polNumb] #removed 290420
    inputDT_DC[, TotNumbVzCodeGroups := length(unique(vzCodeGroup)), by = 'polNumb'] #changed 290420
    inputDT_DC[, CumNumbVzCodeGroups := cumsum((vzCode != shift(vzCodeGroup,  fill = "", type = "lag"))), by = polNumb] #changed 290420
    
    ##update incur and add expected incur and the difference between expected and observed incur
    ## for cases where the incurred stays fixed during multiple (> 2) payments, have to run  a while loop until there is no more line with only payments
    # dailydata[incurInd == 0 & payInd == 1, newCol := delt0Pay]
    # dailydata[, newGroup := cumsum(is.na(newCol))]
    # dailydata[!is.na(newCol), cum_sum := cumsum(newCol), by = newGroup]
    #
    # dailydata [!is.na(cum_sum), newIncur := incur - cum_sum]
    
    # After redefining the incurred, the variables 'incurInd', 'incurIndCum',
    # 'delt1Incur', 'delt1IncurTime', ... should be redefined as well.
    for(iLag in 0:maxLag){ #changed 250520
      inputDT_DC[, paste0(paste0('delt', iLag), 'Pay') := NULL] #changed 250520
      inputDT_DC[, paste0(paste0('delt', iLag), 'Incur') := NULL] #changed 250520
      inputDT_DC[, paste0(paste0('delt', iLag), 'PayTime') := NULL] #changed 250520
      inputDT_DC[, paste0(paste0('delt', iLag), 'IncurTime') := NULL] #changed 250520
    } #changed 250520
    inputDT_DC[, ind := NULL] #changed 250520
    inputDT_DC[, maxNumbPay := NULL] #changed 250520
    inputDT_DC[, payInd := NULL] #changed 250520
    inputDT_DC[, incurInd := NULL] #changed 250520
    inputDT_DC[, incurIndCum := NULL] #changed 250520
    inputDT_DC[, payIndCum := NULL] #changed 250520
    
    # Making sure that all variables are well defined (after the clean-up in the above while loop).
    inputDT_DC <- createCleanIncrVars(inputDT_DC, 0, maxLag, perLen, remLoneChgIncur, verbose) #changed 200520
    
    # Some final definitions and removals of unnecessary variables.
    inputDT_DC[, deltRep := ceiling(diffDaysRep/perLen)]
    inputDT_DC[, fastRep := 0]
    inputDT_DC[deltRep == 0, fastRep := 1]
    inputDT_DC[deltRep == 0, deltRep := 1]
    
    inputDT_DC[, repPeriod := NULL]
    inputDT_DC[, diffDays := NULL]
    inputDT_DC[, diffDaysRep := NULL]
    
    inputDT_DC[, lastPay := 0]
    inputDT_DC[maxNumbPay == payIndCum & payInd == 1, lastPay := 1]
    inputDT_DC[, maxNumbIncur := sum(incurInd), by = polNumb]
    inputDT_DC[, lastIncur := 0]
    inputDT_DC[maxNumbIncur == incurIndCum & incurInd == 1, lastIncur := 1]
    
    inputDT_DC[, transType := NA]
    asFactorDT(inputDT_DC, 'transType')
    inputDT_DC[payInd == 1 & incurInd == 0, transType := 'P']
    inputDT_DC[payInd == 1, transType := 'P']
    inputDT_DC[payInd == 1 & incurInd == 1, transType := 'PI']
    inputDT_DC[payInd == 0 & incurInd == 1, transType := 'I']
    inputDT_DC[ind == nLines & procStat == 1 & lastPay == 0, transType := 'TN']
    inputDT_DC[ind == nLines & procStat == 1 & lastPay == 1, transType := 'TP']
    
    if(remLoneChgIncur){
      inputDT_DC[, delt0IncurTemp := delt0Incur] #changed 200520
      inputDT_DC[incurInd == 0, delt0IncurTemp := 0] #changed 200520
      inputDT_DC[, incurTemp := incur - delt0IncurTemp] #changed 200520
      inputDT_DC[ind == nLines & transType == 'I' & ind != 1 & procStat == 0, incur := incurTemp]
      inputDT_DC[, c('incurTemp', 'delt0IncurTemp') := NULL] #changed 200520
    }
    
    inputDT_DC[, indCum := 0]
    inputDT_DC[payInd == 1L | incurInd == 1L, indCum := 1]
    inputDT_DC[, indCum := cumsum(indCum), by = polNumb]
    
    payName <- paste0(paste0('delt', 0:maxLag), 'Pay') #changed 200520
    incurName <- paste0(paste0('delt', 0:maxLag), 'Incur') #changed 200520
    payTimeName <- paste0(paste0('delt', 0:maxLag), 'PayTime')
    incurTimeName <- paste0(paste0('delt', 0:maxLag), 'IncurTime')
    
    for(iLag in 1:maxLag){ #changed 300420
      deltPayName <- paste0(paste0('delt', iLag - 1), 'Pay') #changed 200520
      inputDT_DC[, temp := cumPay - get(deltPayName)] #changed 300420
      cumPayName <- paste0(paste0('cumDelt', iLag), 'Pay') #changed 300420
      setnames(inputDT_DC, 'temp', cumPayName) #changed 300420
    } #changed 300420
    
    #inputDT_DC
    #setcolorder(inputDT_DC, c('polNumb', 'ind', 'nLines', 'accDate', 'repDate', 'bookDate', 'closedDate', 'transType', 'procStat', 'incur', 'cumPay', 'linkRatioT', 'linkRatioI', 'inProcTime', 'inProcTimeMax', 'deltRep', 'fastRep', payTimeName, incurTimeName, payName, incurName, 'payInd', 'incurInd', 'indCum', 'payIndCum', 'incurIndCum', 'maxNumbPay', 'maxNumbIncur', 'lastPay', 'lastIncur'))
    # setcolorder(inputDT_DC, c('polNumb', 'ind', 'nLines', 'accDate', 'repDate', 'bookDate', 'closedDate', 'transType', 'procStat', 'incur', 'cumPay', 'linkRatioT', 'linkRatioI, 'inProcTime', 'inProcTimeMax', 'deltRep', 'fastRep', 'delt0PayTime', 'delt0IncurTime', 'delt1PayTime', 'delt1IncurTime', 'delt2PayTime', 'delt2IncurTime', 'delt0Pay', 'delt1Pay', 'delt0Incur', 'delt1Incur', 'delt2Pay', 'delt2Incur', 'payInd', 'incurInd', 'payIndCum', 'incurIndCum', 'maxNumbPay', 'maxNumbIncur', 'lastPay', 'lastIncur')) #changed 200520
  }
  return(inputDT_DC)
}

## createCleanIncrVars
#' createCleanIncrVars is function that first applies the function \code{remLinesNoCh} (see its description for details) using
#' the paramater \code{remLoneChgIncur}. Next, incremental variables are created by applying the function \code{addLaggedVars}
#' (see its description for details), using the parameters \code{minLag}, \code{maxLag}, \code{perLen} and \code{verbose}.
#' After all those changes, the variables \code{ind} and \code{nLines} are (re)defined.
#' Finally, the variable \code{maxNumbPay} is defined, which is the total number of payments recorded in the database for
#' the given \code{polNumb}.
#'
#' @param inputDT Dataset used in this function.
#' @param minLag Minimal lag - default value is 1.
#' @param maxLag Maximal lag.
#' @param perLen Length of the timeframe expressed in number of days.
#' @param remLoneChgIncur Boolean that indicates whether the lines with a change in the incurred without a payment being done, should be removed or not.
#' @param verbose Boolean indicating whether output should be printed or not.
#'
#' @return The original dataset \code{inputDT} on which the changes explained in the desciption are applied.
#
createCleanIncrVars <- function(inputDT, minLag, maxLag, perLen, remLoneChgIncur, verbose){
  # Check input parameters and certain variables in 'inputDT'.
  checkDT(inputDT, c('polNumb', 'cumPay', 'incur', 'bookDate', 'closedDate', 'repDate'))
  checkNumVec(list(perLen, minLag, maxLag))
  checkLogicVec(list(verbose, remLoneChgIncur))
  checkLength(list(perLen, minLag, maxLag, remLoneChgIncur, verbose), 1)
  checkRanges(list(minLag, maxLag, perLen), list(c('>', -1), c('>', -1), c('>', 0)))
  checkWholeNumb(list(minLag, maxLag))
  
  # Removing the lines where no change in payment or incurred was observed.
  if(verbose) print("Removing the lines where no change in payment or incurred was observed.")
  inputDT <- remLinesNoChg(inputDT, remLoneChgIncur)
  
  # (Re)Defining the variables 'ind' and 'nLines'.
  if(verbose) print("Defining the incremental variables.")
  if(sum(names(inputDT) == 'ind') == 1) inputDT[, ind := NULL]
  if(sum(names(inputDT) == 'nLines') == 1) inputDT[, nLines := NULL]
  inputDT[, ind := 1:.N, by = polNumb]
  inputDT <- merge(inputDT, inputDT[, .(nLines = .N), by = polNumb], all.x = TRUE)
  
  # Adding the incremental variables (see explanation of 'addLaggedVars'), together with 'maxNumbPay'.
  inputDT <- addLaggedVars(inputDT, minLag, maxLag, perLen, verbose)
  if(sum(names(inputDT) == 'maxNumbPay') == 1) inputDT[, maxNumbPay := NULL]
  inputDT[, maxNumbPay := sum(payInd), by = polNumb]
  return(inputDT)
}

## remLinesNoChg
#' remLinesNoChg is a function that adds the following new variables to the given dataset:
#'    \code{ind}: Index of the chronological order of each line for a given "polNumb", starting from value 1.
#'    \code{payInd}: Indicator variable showing whether or not a payment was done at \code{bookDate}.
#'    \code{incurInd}: Indicator variable showing whether or not a change in the incurred loss took place at \code{bookDate}.
#' Moreover, it removes the lines where nothing happens, e.g. no payment or change in incurred, when it is not the last line of that \code{polNumb}.
#' If \code{remLoneChgIncur} is TRUE, it also removes the lines where there is a change in the incurred without a payment being done.
#'
#' @param inputDT Dataset used in this function.
#' @param remLoneChgIncur Boolean that indicates whether the lines with a change in the incurred without a payment being done, should be removed or not.
#'
#' @return The original dataset \code{inputDT} to which the new variables are added and some lines are removed.
#
remLinesNoChg <- function(inputDT, remLoneChgIncur){
  # Check input parameters and certain variables in 'inputDT'.
  checkDT(inputDT, c('polNumb', 'cumPay', 'incur', 'bookDate', 'closedDate'))
  checkLogicVec(list(remLoneChgIncur))
  checkLength(list(remLoneChgIncur), 1)
  
  # Define 'ind'.
  inputDT[, ind := 1:.N, by = polNumb]
  
  # Define 'payInd' and 'incurInd'.
  inputDT[, deltRowPay := cumPay - shift(cumPay, n = 1, fill = NA, type = "lag"), by = polNumb]
  inputDT[, deltRowIncur := incur - shift(incur, n = 1, fill = NA, type = "lag"), by = polNumb]
  inputDT[ind == 1, deltRowPay := cumPay]
  inputDT[ind == 1, deltRowIncur := incur]
  inputDT[, payInd := sapply(deltRowPay, function(x){if(x == 0 | is.na(x)){0} else{1}})]
  inputDT[, incurInd := sapply(deltRowIncur, function(x){if(x == 0 | is.na(x)){0} else{1}})]
  inputDT[nLines == 1, payInd := sapply(cumPay, function(x){if(x == 0 | is.na(x)){0} else{1}})]
  inputDT[nLines == 1, incurInd := sapply(incur, function(x){if(x == 0 | is.na(x)){0} else{1}})]
  inputDT[, c('deltRowPay', 'deltRowIncur') := NULL]
  
  # Redefine 'bookDate' as 'closedDate' (if it exists) in case nothing happens, e.g. no payment or change in incurred, on the last line of that 'polNumb'.
  inputDT[(payInd == 0 & incurInd == 0 & ind == nLines & bookDate != closedDate & !is.na(closedDate)), bookDate := closedDate]
  
  # Remove the lines where nothing happens, e.g. no payment or change in incurred, when it is not the last line of that 'polNumb'.
  inputDT <- inputDT[!(payInd == 0 & incurInd == 0 & ind != nLines), ]
  
  # Remove the lines where there is a change in the incurred without a payment being done, if 'remLoneChgIncur' is TRUE (less than 0.6% of the cases).
  # Note that the last line is always kept, since it contains the last information available.
  if(remLoneChgIncur){
    inputDT <- inputDT[!(payInd == 0 & incurInd == 1 & ind != nLines), ]
  }
  
  # Since some lines may be removed, the variables 'ind', 'payInd' and 'incurInd' should be redefined.
  inputDT[, ind := 1:.N, by = polNumb]
  inputDT[, deltRowPay := cumPay - shift(cumPay, n = 1, fill = NA, type = "lag"), by = polNumb]
  inputDT[, deltRowIncur := incur - shift(incur, n = 1, fill = NA, type = "lag"), by = polNumb]
  inputDT[ind == 1, deltRowPay := cumPay]
  inputDT[ind == 1, deltRowIncur := incur]
  inputDT[, payInd := sapply(deltRowPay, function(x){if(x == 0 | is.na(x)){0} else{1}})]
  inputDT[, incurInd := sapply(deltRowIncur, function(x){if(x == 0 | is.na(x)){0} else{1}})]
  inputDT[nLines == 1, payInd := sapply(cumPay, function(x){if(x == 0 | is.na(x)){0} else{1}})]
  inputDT[nLines == 1, incurInd := sapply(incur, function(x){if(x == 0 | is.na(x)){0} else{1}})]
  inputDT[, c('deltRowPay', 'deltRowIncur') := NULL]
  
  return(inputDT)
}

## addLaggedVars
#' addLaggedVars is a function that adds the following new variables to the given dataset:
#'    \code{incurIndCum}: The number of changes of incurred loss that took place up until and including the \code{bookDate}.
#'    \code{payIndCum}: The number of payments that were done up until and including the \code{bookDate}.
#'    \code{delt0PayTime}: Number of periods from \code{bookDate} of the given line since the last recorded payment.
#'                         If the selected line correspond to line when a payment took place, then the value is by definition 0.
#'    \code{delt1PayTime}: Number of periods from \code{bookDate} of the given line since the second last recorded payment.
#'                         If the selected line corresponds to the first recorded payment, then the value is by definition NA.
#'    ... until deltMaxLagPayTime
#'    \code{delt0IncurTime}: Number of periods from \code{bookDate} of the given line since the last recorded change in incurred loss.
#'                           If the selected line correspond to line when a change in incurred loss took place, then the value is by definition 0.
#'    \code{delt1IncurTime}: Number of periods from \code{bookDate} of the given line since the second last recorded change in incurred loss.
#'                           If the selected line corresponds to the first line when a change in incurred loss took place, then the value is by definition NA.
#'    ... until deltMaxLagIncurTime
#'    \code{delt0Pay}: Value of the current payment.
#'    \code{delt1Pay}: Value of the last payment.
#'    \code{delt2Pay}: Value of the second last payment.
#'    ... until deltMaxLagPay
#'    \code{delt0Incur}: Value of the current change in incurred loss.
#'    \code{delt1Incur}: Value of the last change in incurred loss.
#'    \code{delt2Incur}: Value of the second last change in incurred loss.
#'    ... until deltMaxLagIncur
#'
#' @param inputDT Dataset used in this function.
#' @param minLag Minimal lag - default value is 1.
#' @param maxLag Maximal lag.
#' @param perLen Length of the timeframe expressed in number of days.
#' @param verbose Boolean indicating whether output should be printed or not.
#'
#' @return The original dataset \code{inputDT} to which the new variables are added.
#
addLaggedVars <- function(inputDT, minLag, maxLag, perLen, verbose){
  # Check input parameters and certain variables in 'inputDT'.
  checkDT(inputDT, c('polNumb', 'cumPay', 'incur', 'bookDate', 'closedDate', 'repDate'))
  checkNumVec(list(perLen, minLag, maxLag))
  checkLength(list(perLen, minLag, maxLag, verbose), 1)
  checkRanges(list(minLag, maxLag, perLen), list(c('>', -1), c('>', -1), c('>', 0)))
  checkWholeNumb(list(minLag, maxLag))
  checkLogicVec(list(verbose))
  
  # Define 'incurIndCum' and 'payIndCum'.
  inputDT[, incurIndCum := cumsum(incurInd), by = polNumb]
  inputDT[, payIndCum := cumsum(payInd), by = polNumb]
  
  # Define 'delt0PayTime'.
  inputDT[payInd == 1L, payDate := bookDate]
  inputDT[, payDate := na.locf(payDate, na.rm = FALSE), by = polNumb]
  inputDT[, delt0PayTime := bookDate - payDate]
  inputDT[nLines == 1L & cumPay != 0, delt0PayTime := 0]
  inputDT[nLines == 1L & cumPay == 0, delt0PayTime := bookDate - repDate]
  asNumericDT(inputDT, 'delt0PayTime')
  inputDT[, delt0PayTime := ceiling(delt0PayTime/perLen)]
  inputDT[, payDate := NULL]
  
  # Define 'delt0IncurTime'.
  inputDT[incurInd == 1L, incurDate := bookDate]
  inputDT[, incurDate := na.locf(incurDate, na.rm = FALSE), by = polNumb]
  inputDT[, delt0IncurTime  := bookDate - incurDate]
  inputDT[nLines == 1L & incur != 0, delt0IncurTime := 0]
  inputDT[nLines == 1L & incur == 0, delt0IncurTime := bookDate - repDate]
  asNumericDT(inputDT, 'delt0IncurTime')
  inputDT[, delt0IncurTime  := ceiling(delt0IncurTime /perLen)]
  inputDT[, incurDate := NULL]
  
  # Needed for correction in case the first line of "cumPay" or "incur" is 0.
  polNumbPay0 <- inputDT[(payInd == 0L) & (ind == 1), ]$polNumb
  polNumbPay1 <- inputDT[(payInd == 1L) & (ind == 1), ]$polNumb
  polNumbInc0 <- inputDT[(incurInd == 0L) & (ind == 1), ]$polNumb
  polNumbInc1 <- inputDT[(incurInd == 1L) & (ind == 1), ]$polNumb
  
  for(iLag in minLag:maxLag){
    if(verbose) print(paste0(paste0('Creating the lag ', iLag), ' variables.'))
    
    # Define correct names for the variables that will be created in this loop.
    payName <- paste0(paste0('delt', iLag), 'Pay')
    incurName <- paste0(paste0('delt', iLag), 'Incur')
    payTimeName <- paste0(paste0('delt', iLag), 'PayTime')
    incurTimeName <- paste0(paste0('delt', iLag), 'IncurTime')
    
    if(iLag == 0){#changed 250520
      
      # Define 'deltPay'.
      # inputDT[payInd == 1L, temp := cumPay - shift(cumPay, n = iLag, fill = NA, type = "lag"), by = polNumb]   #removed 230420
      inputDT[, temp := shift(cumPay, n = iLag + 1, type = "lag"), by = polNumb] #changed 200520
      inputDT[!((payInd == 0L) & (ind == nLines)), temp := NA] #changed 040520
      inputDT[ (polNumb %in% polNumbPay0) & (payInd == 1L), temp := cumPay - (shift(rep(0, length(cumPay)), n = iLag, fill = NA) + shift(cumPay, n = iLag + 1, fill = 0)), by = polNumb] #changed 200520
      inputDT[ (polNumb %in% polNumbPay1) & (payInd == 1L), temp := cumPay - shift(cumPay, n = iLag + 1, fill = NA, type = "lag"), by = polNumb] #changed 200520
      inputDT[, temp := na.locf(temp, na.rm = FALSE), by = polNumb]
      if(sum(names(inputDT) == payName) > 0) inputDT[, c(payName) := NULL]
      inputDT[ind == 1 & (payInd == 1L), temp := cumPay] #changed 250520
      setnames(inputDT, 'temp', payName)
      
      # Define 'deltIncur'.
      inputDT[, temp := shift(incur, n = iLag + 1, type = "lag"), by = polNumb] #changed 200520
      inputDT[!((incurInd == 0L) & (ind == nLines)), temp := NA] #changed 040520
      inputDT[ (polNumb %in% polNumbInc0) & (incurInd == 1L), temp := incur - (shift(rep(0, length(incur)), n = iLag, fill = NA) + shift(incur, n = iLag + 1, fill = 0)), by = polNumb] #changed 200520
      inputDT[ (polNumb %in% polNumbInc1) &(incurInd == 1L), temp := incur - shift(incur, n = iLag + 1, fill = NA, type = "lag"), by = polNumb] #changed 200520
      inputDT[, temp := na.locf(temp, na.rm = FALSE), by = polNumb]
      if(sum(names(inputDT) == incurName) > 0) inputDT[, c(incurName) := NULL]
      inputDT[ind == 1 & (incurInd == 1L), temp := incur] #changed 250520
      setnames(inputDT, 'temp', incurName)
      
    } else {#changed 250520
      
      # Define 'deltPay'.#changed 250520
      inputDT[, temp := shift(delt0Pay, n = iLag, type = "lag"), by = polNumb] #changed 250520
      setnames(inputDT, 'temp', payName) #changed 250520
      
      # Define "deltIncur" #changed 250520
      inputDT[, temp := shift(delt0Incur, n = iLag, type = "lag"), by = polNumb] #changed 250520
      setnames(inputDT, 'temp', incurName) #changed 250520
      
    } #changed 250520
    
    # Define 'transTypeTemp', a temporary variable that indicates whether a transition to a terminal state without payment took place.
    # 'transType' is not defined yet, but we need to identify 'TN' transitions when we define 'deltTimePay'.
    inputDT[ind == nLines & procStat == 1 & payInd == 0, transTypeTemp := 'TN']
    
    # Define 'deltTimePay'.
    # inputDT[ ((polNumb %in% polNumbPay0) & (payInd == 1L))  | (nLines == ind & transTypeTemp == 'TN'), diffDaysBook := bookDate - (shift(rep(0, length(bookDate)), n = (iLag - 1), fill = NA) + shift(bookDate, n = iLag, fill = 0)), by = polNumb] #removed 280420
    # inputDT[ ((polNumb %in% polNumbPay1) & (payInd == 1L))  | (nLines == ind & transTypeTemp == 'TN'), temp := bookDate - shift(bookDate, n = iLag, fill = NA, type = "lag"), by = polNumb] #removed 280420, idem as for deltTimeIncur
    # inputDT[payInd == 1L | (nLines == ind & transTypeTemp == 'TN'), diffDaysBook := bookDate - shift(bookDate, n = iLag, fill = NA, type = "lag"), by = polNumb]    #inputDT[ind == iLag, diffDaysBook := bookDate - repDate]  #changed 280420
    # asNumericDT(inputDT, 'diffDaysBook')
    # inputDT[, temp := ceiling(diffDaysBook/perLen)]
    # inputDT[, temp := na.locf(temp, na.rm = FALSE), by = polNumb]
    # if(sum(names(inputDT) == payTimeName) > 0) inputDT[, c(payTimeName) := NULL]
    # setnames(inputDT, 'temp', payTimeName)
    
    #Jolien
    inputDT[payInd == 1L | (nLines == ind & transTypeTemp == 'TN'), temp := inProcTime - shift(inProcTime, n = iLag, fill = NA, type = "lag"), by = polNumb]    #inputDT[ind == iLag, diffDaysBook := bookDate - repDate]  #changed 280420
    asNumericDT(inputDT, 'temp')
    inputDT[, temp := na.locf(temp, na.rm = FALSE), by = polNumb]
    if(sum(names(inputDT) == payTimeName) > 0) inputDT[, c(payTimeName) := NULL]
    setnames(inputDT, 'temp', payTimeName)
    
    
    # Define 'deltTimeIncur'.
    # inputDT[ ((polNumb %in% polNumbInc0) & (incurInd == 1L))| (nLines == ind & transTypeTemp == 'TN'), diffDaysBook := bookDate - (shift(rep(0, length(bookDate)), n = (iLag - 1), fill = NA) + shift(bookDate, n = iLag, fill = 0)), by = polNumb] #removed 280420, idem as for deltTimePay
    # inputDT[ ((polNumb %in% polNumbInc1) & (incurInd == 1L))| (nLines == ind & transTypeTemp == 'TN'), temp := bookDate - shift(bookDate, n = iLag, fill = NA, type = "lag"), by = polNumb] #removed 280420
    inputDT[incurInd == 1L | (nLines == ind & transTypeTemp == 'TN'), diffDaysBook := bookDate - shift(bookDate, n = iLag, fill = NA, type = "lag"), by = polNumb]#inputDT[ind == iLag, diffDaysBook := bookDate - repDate] #changed 280420
    asNumericDT(inputDT, 'diffDaysBook')
    inputDT[, temp := ceiling(diffDaysBook/perLen)]
    inputDT[, temp := na.locf(temp, na.rm = FALSE), by = polNumb]
    if(sum(names(inputDT) == incurTimeName) > 0) inputDT[, c(incurTimeName) := NULL]
    setnames(inputDT, 'temp', incurTimeName)
    
    # Remove the temporary variables 'diffDaysBook' and 'transTypeTemp'.
    inputDT[, diffDaysBook := NULL]
    inputDT[, transTypeTemp := NULL]
  }
  return(inputDT)
}

## addFirstLast
#' addFirstLast is a function that first sorts all the data based on the columns defined in \code{orderBy}.
#' In case isFirst is TRUE, a new variable \code{first} is defined which is one for each first unique row, and zero otherwise.
#' In case isLast is TRUE, a new variable \code{last} is defined which is one for each last unique row, and zero otherwise.
#'
#' @param inputDT Dataset used in this function.
#' @param orderBy Columnames ofinputDT on which this dataset will be sorted.
#' @param isFirst Boolean with default value TRUE. In case it is TRUE, a new variable \code{first} is defined which is one for each first unique row, and zero otherwise.
#' @param isLast Boolean with default value TRUE. In case it is TRUE, a new variable \code{last} is defined which is one for each last unique row, and zero otherwise.
#'
#' @return The original dataset \code{inputDT} that is sorted and to which the new variable(s) is/are added.
#
addFirstLast <- function(inputDT, orderBy, isFirst = TRUE, isLast = TRUE){
  # Check input parameters and certain variables in 'inputDT'.
  checkCharVec(list(orderBy))
  checkLogicVec(list(isFirst, isLast))
  checkLength(list(isFirst, isLast), 1)
  checkDT(inputDT, orderBy)
  
  # Sort inputDT based on the colums listed in 'orderBy'.
  setkeyv(inputDT, orderBy)
  # uDT are the unique lines of inputDT.
  uDT <- unique(inputDT)
  
  if(isFirst){
    # Define a new variable 'first' which is in the beginning zero for all observations.
    inputDT[, "first" := 0L]
    # The variable 'first' is one for the unique rows, and for the first line in case of a row that is not unique (mult="first").
    inputDT[uDT, first := 1L, mult = "first"]
  }
  if(isLast){
    # Define a new variable 'last' which is in the beginning zero for all observations.
    inputDT[, "last" := 0L]
    # The variable 'last' is one for the unique rows, and for the last line in case of a row that is not unique (mult="last").
    inputDT[uDT, last := 1L, mult = "last"]
  }
  return(inputDT)
}

## mergePayIncr
#' Removing the small payments (indicated by \code{minPayVal}) from the database.
#'
#' @param inputDT Dataset used in this function.
#' @param polNumbs The unique policy numbers in \code{inputDT} that should be checked for small payments.
#' @param minPayVal Smallest value from which on a payment is considered.
#' @param perLen Length of the timeframe expressed in number of days - default value is 30.
#' @param maxLag Maximal lag - default value is 1.
#' @param verbose Boolean indicating whether output should be printed or not - default value is FALSE.
#
mergePayIncr <- function(inputDT, polNumbs, minPayVal, perLen = 30, maxLag = 0, verbose = FALSE){ #changed 200520
  # Check input parameters and certain variables in 'inputDT'.
  checkNumVec(list(minPayVal, maxLag))
  checkCharVec(list(polNumbs))
  payName <- paste0(paste0('delt', maxLag), 'Pay')
  checkDT(inputDT, c('polNumb', 'payInd', 'incurInd', 'bookDate', 'closedDate', 'maxNumbPay', payName))
  checkLength(list(minPayVal, maxLag), 1)
  checkRanges(list(minPayVal), list(c('>', 0)))
  checkRanges(list(maxLag, perLen), list(c('>', -1), c('>', 0)))
  checkWholeNumb(list(maxLag))
  
  # Check if there are still some low values.
  inputDT[, lowPayIncr := NA]
  asNumericDT(inputDT, 'lowPayIncr')
  selRows <- as.integer(which(inputDT$polNumb %in% polNumbs & !(inputDT$payInd == 0 & inputDT$incurInd == 0 & inputDT$bookDate == inputDT$closedDate)))
  selCol <- as.integer(which(names(inputDT) == 'lowPayIncr'))
  vals <- as.numeric(abs(inputDT[polNumb %in% polNumbs & !(payInd == 0 & incurInd == 0 & bookDate == closedDate), .SD, .SDcol = which(names(inputDT) == payName)]) < minPayVal)
  set(inputDT, selRows, selCol, vals)
  
  tempAllDT <- copy(inputDT)
  firstTime <- TRUE
  goOn <- TRUE
  counter <- 1
  
  polNumbsChgd <- c()
  
  # if(verbose) print('Starting the while loop.')
  
  while(goOn){
    # if(verbose) print(paste0('Run ', counter))
    
    # Through the function addFirstLast, the data is sorted based on the 'polNumb' and 'lowPayIncr'.
    # Moreover, a new variable 'first' is defined, which is one for each first unique row, and zero otherwise.
    tempAllDT <- addFirstLast(tempAllDT, c('polNumb', 'lowPayIncr'), , F)
    setkeyv(tempAllDT, c('polNumb', 'ind'))
    
    # The 'polNumbs' with at least one 'lowPayIncr' row will be extracted (keep == 1)
    evalDT <- tempAllDT[, .(times = .N), by = c('polNumb', 'lowPayIncr')]
    tempAllDT <- merge(tempAllDT, evalDT[lowPayIncr == 1, .(polNumb, keep = 1, times)], all.x = T, by = 'polNumb')
    # The done var is created now which is 1, if the polNumb is done, if no eligible row is left (we needed the keep var to construct this var).
    donePolNumbs <- unique(tempAllDT[is.na(keep) | (first == 1 & lowPayIncr == 1 & (maxNumbPay == 1 | ind == nLines) & times == 1), polNumb])
    tempAllDT[, done := NA]
    if(length(donePolNumbs) > 0){
      tempAllDT[, done := NULL]
      evalDT2 <- data.table(polNumb = donePolNumbs, done = 1)
      tempAllDT <- merge(tempAllDT, evalDT2[, .(polNumb, done)], all.x = T, by = 'polNumb')
    }
    
    # if(firstTime){
    #   doneDT <- tempAllDT[is.na(keep), ]
    #   firstTime <- FALSE
    # } else {
    #   if(nrow(tempAllDT[is.na(keep), ]) > 0){
    #     doneDT <- rbindlist(list(doneDT, tempAllDT[is.na(keep), ]), use.names = T, fill = TRUE)
    #   }
    # }
    
    # doneDT contains all the 'polNumb's (and all of their lines) that do not have any lowPayIncr == 1 row.
    # tempAllDT just contains those polNumb (with all their lines) that have at least one lowPayIncr == 1.
    if(firstTime){
      doneDT <- tempAllDT[done == 1, ]
      firstTime <- FALSE
    } else {
      if(nrow(tempAllDT[done == 1, ]) > 0){
        doneDT <- rbindlist(list(doneDT, tempAllDT[done == 1, ]), use.names = T, fill = TRUE)
      }
    }
    
    # #let's remove the keep == NA polNumbs
    # tempAllDT <- tempAllDT[keep == 1, ]
    # tempAllDT[, keep := NULL]
    
    # Let's remove the keep == NA 'polNumbs'
    tempAllDT <- tempAllDT[is.na(done), ]
    # tempAllDT[, keep := NULL]
    # tempAllDT[, done := NULL]
    # tempAllDT[, times := NULL]
    tempAllDT[, c('times', 'keep', 'done') := NULL]
    
    # We still accept the small payment if it is the last line
    # (we do not want to loose the last line + you will actually loose information when you throw away the last line) and when there is just one payment.
    ind2BeRemoved <- which(tempAllDT$first == 1 & tempAllDT$lowPayIncr == 1 & tempAllDT$maxNumbPay != 1 & tempAllDT$ind != tempAllDT$nLines)
    n2BeRemoved <- length(ind2BeRemoved)
    if(n2BeRemoved == 0){
      goOn <- FALSE
    } else {
      # if(verbose & n2BeRemoved > 1) print(paste0(n2BeRemoved, ' rows are removed in this run.'))
      # if(verbose & n2BeRemoved == 1) print(paste0(n2BeRemoved, ' row is removed in this run.'))
      # Remove the lines with the low payment.
      polNumbsRun <- unique(tempAllDT[ind2BeRemoved, polNumb])
      polNumbsChgd <- unique(c(polNumbsChgd, polNumbsRun))
      tempAllDT <- tempAllDT[setdiff(1:nrow(tempAllDT), ind2BeRemoved), ]
      
      # Redefine the variables that are impacted by removing a line from the data base.
      tempAllDT <- createCleanIncrVars(tempAllDT, 0, maxLag, perLen, TRUE, FALSE) #changed 200520
      
      # Check if there are still some low values.
      tempAllDT[, lowPayIncr := NA]
      asNumericDT(tempAllDT, 'lowPayIncr')
      selRows <- as.integer(which(tempAllDT$polNumb %in% polNumbsRun & !(tempAllDT$payInd == 0 & tempAllDT$incurInd == 0 & tempAllDT$bookDate == tempAllDT$closedDate)))
      selCol <- as.integer(which(names(tempAllDT) == 'lowPayIncr'))
      vals <- as.numeric(abs(tempAllDT[polNumb %in% polNumbsRun & !(payInd == 0 & incurInd == 0 & bookDate == closedDate), .SD, .SDcol = which(names(tempAllDT) == payName)]) < minPayVal)
      set(tempAllDT, selRows, selCol, vals)
    }
    counter <- counter + 1
  }
  doneDT[, c('first', 'times', 'keep', 'done', 'lowPayIncr') := NULL]
  
  if(nrow(tempAllDT) == 0){
    return(list(inputDT = doneDT, polNumbs = polNumbsChgd))
  } else {
    return(list(inputDT = rbind(doneDT, tempAllDT), polNumbs = polNumbsChgd))
  }
}

## mergeFirstCumPay
#' mergeFirstCumPay is a function that removes the first line of each \code{polNumb} with more than one line,
#' in case the absolute value of \code{cumPay} on that line is smaller than \code{minPayVal}.
#' The variables \code{ind} and \code{nLines} are redefined.
#'
#' @param inputDT Dataset used in this function.
#' @param minPayVal Smallest value from which on a payment is considered.
#' @param verbose Boolean indicating whether output should be printed or not.
#'
#' @return The original dataset \code{inputDT} on which the changes explained in the desciption are applied.
#
mergeFirstCumPay <- function(inputDT, minPayVal, verbose){
  # Check input parameters and certain variables in 'inputDT'.
  checkNumVec(list(minPayVal))
  checkLogicVec(list(verbose))
  checkDT(inputDT, c('polNumb', 'cumPay', 'ind', 'nLines'))
  checkLength(list(minPayVal, verbose), 1)
  checkRanges(list(minPayVal), list(c('>', 0)))
  
  # if(verbose) print('Starting the while loop.')
  goOn <- TRUE
  counter <- 1
  
  while(goOn){
    # if(verbose) print(paste0('Run ', counter))
    
    # Define a temporary variable 'getOut', which is equal to zero.
    inputDT[, getOut := 0]
    
    # In case a policynumber with more than 1 line satisfies abs(cumPay) < minPayVal on the first line,
    # the variable 'getOut' is 1 for that line.
    #inputDT[ind == 1 & nLines > 1 & cumPay < minPayVal, getOut := 1]
    inputDT[ind == 1 & nLines > 1 & abs(cumPay) < minPayVal, getOut := 1]
    
    # Define the integer 'ngetOut' as the number of lines where the variable 'getOut' is 1.
    ngetOut <- nrow(inputDT[getOut == 1, ])
    if(ngetOut == 0){
      # In case ngetOut is 0, the while loop will stop.
      goOn <- FALSE
    } else {
      # if(verbose & ngetOut > 1) print(paste0(ngetOut, ' rows are removed in this run.'))
      # if(verbose & ngetOut == 1) print(paste0(ngetOut, ' row is removed in this run.'))
      
      # Keep only the lines where getOut is zero in the dataset.
      inputDT <- inputDT[getOut == 0, ]
      # Redefine the variables 'ind' and 'nLines'. This is necessary after the previous selection in the dataset.
      if(sum(names(inputDT) == 'ind') == 1) inputDT[, ind := NULL]
      if(sum(names(inputDT) == 'nLines') == 1) inputDT[, nLines := NULL]
      inputDT[, ind := 1:.N, by = polNumb]
      inputDT <- merge(inputDT, inputDT[, .(nLines = .N), by = polNumb], all.x = TRUE)
      counter <- counter + 1
    }
  }
  # Remove the temporary variable 'getOut'.
  inputDT[, getOut := NULL]
  
  return(inputDT)
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

addError <- function(msg, argcheck){
  if(!"ArgCheck" %in% class(argcheck)) stop("'argcheck' must be an object of class 'ArgCheck'")
  assign("n_error", get("n_error", envir = argcheck) + 1, envir = argcheck)
  assign("error_msg", c(get("error_msg", envir = argcheck), msg), envir = argcheck)
}

## transPrep
#' transPrep is a function that returns a list of datasets where each element in the list is the dataset containing the observations corresponding to a transition number between 1 and 'maxTrans',
#' focussing on the transtype defined in 'whichTrans'.
#'
#' @param inputDT Dataset used in this function.
#' @param maxTrans Maximum number of transitions.
#' @param whichTrans TransType that should be modelled - default value is 'P'.
#' @param lumbLast Boolean that indicates whether the payments after maxTrans should be lumped together - default value is TRUE.
#' @param extraVars Extra variables that should be used - default value is NULL.
#' @param addDup Boolean that indicates whether one line per period a claim spends in a given transition should be created +
#'               reajust \code{inprocTime} for the created lines - default value is TRUE.
#' @param verbose Boolean indicating whether output should be printed or not - default value is TRUE
#' @param dropPolNumb Boolean that indicates whether the policy numbers should be dropped - default value is FALSE
#'
#' @return  A list of datasets where each element in the list is the dataset containing the observations corresponding to a transition number between 1 and \code{maxTrans},
#' focussing on the transtype defined in \code{whichTrans}.
#' The variables in each dataset in the list are :'polNumb' (if dropPolNumb==FALSE), 'outcomeVars', 'transStatLast', 'deltRep', 'fastRep', 'inProcTime', 'time2Trans', 'transType', 'finYear', 'extraVars', 'delt(0:iTrans-1)Pay', 'deltIncur(0:(iTrans-1))', 'deltPayTime(0:(iTrans-2))', 'deltIncurTime(0:(iTrans-2))'
#
transPrep <- function(inputDT, maxTrans, whichTrans = 'P', lumbLast = TRUE, extraVars = NULL, addDup = TRUE, verbose = TRUE, dropPolNumb = FALSE){
  # Check input parameters and certain variables in 'inputDT'.
  checkDT(inputDT, c('bookDate', 'repDate', 'accDate', 'procStat', 'closedDate', 'inProcTimeMax', 'payInd',  'ind', 'nLines', 'maxNumbPay', 'payIndCum',  'lastPay', 'polNumb', 'deltRep', 'fastRep', 'inProcTime', 'transType', extraVars))
  checkNumVec(list(maxTrans))
  checkLogicVec(list(verbose, dropPolNumb, lumbLast))
  if(!is.null(extraVars)) checkCharVec(list(extraVars))
  checkCharVec(list(whichTrans))
  checkValues(list(whichTrans), list(c('P', 'I', 'PI')))
  checkLength(list(maxTrans, verbose, dropPolNumb, whichTrans, lumbLast), 1)
  checkRanges(list(maxTrans), list(c('>', 0)))
  
  transData <- list()
  
  for(iTrans in 1:maxTrans){
    
    if(verbose) print(paste0('Prepping transition ', iTrans))
    # Create 'selData'.
    # Subset of the original dataset based on the transition number (or transition number -1 if it's the last line and there has been no change in the payment/incur).
    if(iTrans == maxTrans & lumbLast){# To add all the remaining transitions to the last element of the list.
      if(whichTrans == 'P'){
        selData <- copy(inputDT[(payIndCum >= iTrans & payInd == 1L) | (payIndCum >= (iTrans-1) & ind == nLines & nLines >= iTrans& payInd == 0L), ])
      } else if(whichTrans == 'I'){
        selData <- copy(inputDT[(incurIndCum >= iTrans & incurInd == 1L) | (incurIndCum == (iTrans - 1) & ind == nLines & nLines >= iTrans), ])
      } else if(whichTrans == 'PI'){
        selData <- copy(inputDT[(indCum >= iTrans & (payInd == 1L | incurInd == 1L)) | (indCum == (iTrans - 1) & ind == nLines & nLines >= iTrans), ])#added
      }
    } else {
      if(whichTrans == 'P'){
        selData <- copy(inputDT[(payIndCum == iTrans & payInd == 1L) | (payIndCum == (iTrans - 1) & ind == nLines & nLines >= iTrans & payInd == 0L), ])
      } else if(whichTrans == 'I'){
        selData <- copy(inputDT[(incurIndCum == iTrans & incurInd == 1L) | (incurIndCum == (iTrans - 1) & ind == nLines & nLines >= iTrans), ])
      } else if(whichTrans == 'PI'){
        selData <- copy(inputDT[(indCum == iTrans & (payInd == 1L | incurInd == 1L)) | (indCum == (iTrans - 1) & ind == nLines & nLines >= iTrans), ])
      }
    }
    selData[, finYear := year(bookDate)]
    selData[, repYear := year(repDate)]
    
    # Define a new indicator variable 'transStatLast' indicating whether a transition happened or not.
    # If it's not the last payment or change in the incurred, it's 1. Otherwise, it's equal to the procStat.
    selData[, transStatLast := 1]
    selData[lastPay == 1, transStatLast := procStat]
    
    # Remove redundant columns.
    selData[, c('repDate', 'accDate', 'procStat', 'closedDate', 'inProcTimeMax', 'payInd',  'ind', 'nLines', 'maxNumbPay', 'payIndCum', 'lastPay') := NULL]
    
    # Define outcome variables('outcomeI' and/or 'outcomeT').
    # When interested in the payments (incurred): it equals 'cumPay' ('incur') for the first transition and 'delt1Pay' ('delt1Incur') otherwise.
    outcomeVars <- c()
    if(whichTrans %in% c('P', 'PI')){
      if(iTrans == 1){
        selData[, outComeT := cumPay]
      } else {
        # selData[, outComeT := linkRatioT]
        selData[, outComeT := delt0Pay] #changed 200520
        selData[delt0Pay != 0, linkRatio := cumPay/(cumPay-delt0Pay)] #added MAFE 060821
        selData[delt0Pay == 0, linkRatio := 1] #added MAFE 060821
        outcomeVars <- c(outcomeVars, 'linkRatio')  #added MAFE 060821
      }
      outcomeVars <- c(outcomeVars, 'outComeT')  
    }
    if (whichTrans %in% c('I', 'PI')){
      if(iTrans == 1){
        selData[, outComeI := incur]
      } else {
        selData[, outComeI := delt0Incur] #changed 200520
      }
      outcomeVars <- c(outcomeVars, 'outComeI')
    }
    
    # Define a new variable 'time2Trans'.
    # It is the smallest number of periods since the last transition we're interested in (not including the current line).
    selData[, time2Trans := 0]
    if(iTrans == 1){
      selData[, time2Trans := inProcTime]
    } else {
      if(whichTrans == 'P'){
        selData[delt0PayTime != 0, time2Trans := (delt0PayTime)]
        selData[delt0PayTime == 0, time2Trans := (delt1PayTime)]
        # #test
        # selData[time2Trans == 0 & ind == nLines,] #can have some lines
        # selData[time2Trans == 0 & ind == nLines & payInd == 1,] #should be empty
      } else if(whichTrans == 'I'){
        selData[delt0IncurTime == 0, time2Trans := (delt1IncurTime)]
      } else if(whichTrans == 'PI'){
        selData[delt0PayTime == 0 & delt0IncurTime == 0, time2Trans := pmin(delt1PayTime, delt1IncurTime)]
        selData[delt0PayTime != 0 & delt0IncurTime == 0, time2Trans := pmin(delt0PayTime, delt1IncurTime)]
        selData[delt0PayTime == 0 & delt0IncurTime != 0, time2Trans := pmin(delt1PayTime, delt0IncurTime)]
      }
    }
    
    # Define a new variable 'saveInds'; the indices of the interesting columns of 'selData'.
    saveInds <- c()
    if(iTrans > 1){
      saveInds <- c(saveInds, which(names(selData) %in% paste0(paste0('delt', 0:(iTrans - 1)), 'Pay'))) #changed 200520
      saveInds <- c(saveInds, which(names(selData) %in% paste0(paste0('delt', 0:(iTrans - 1)), 'Incur'))) #changed 200520
      saveInds <- c(saveInds, which(names(selData) %in% paste0(paste0('delt', 0:(iTrans - 1)), 'PayTime'))) #changed 300420
      saveInds <- c(saveInds, which(names(selData) %in% paste0(paste0('delt', 0:(iTrans - 1)), 'IncurTime'))) #changed 300420
      saveInds <- c(saveInds, which(names(selData) %in% paste0(paste0('cumDelt', 1:(iTrans - 1)), 'Pay'))) #changed 300420
    }
    saveInds <- sort(unique(c(saveInds, which(names(selData) %in% c('polNumb', outcomeVars, 'transStatLast', 'deltRep', 'fastRep', 'inProcTime', 'time2Trans', 'transType', 'repYear','finYear', 'repDate', 'bookDate', extraVars))))) #changed 140720
    selData <- selData[, .SD, .SDcol = saveInds]
    
    # When 'addDup' is TRUE:
    #    - Duplicate each line of claims such that there is one line per period a claims spends in a given transition number.
    #    - Modify 'inprocTime' for the created duplicate lines according to its definition.
    #    - Define a new indicator variable 'transStat', indicating whether a transition has happened on each duplicated line.
    if(addDup){
      selData[is.na(time2Trans), time2Trans := 1]#Jordy: for some lines there are no payments in a given year. We choose the bettonville approach
      selData[time2Trans == 0, time2Trans := 1]
      selData[inProcTime == 0, inProcTime := 1]
      selData <- selData[rep(1:nrow(selData), selData$time2Trans),]
      selData[, transStat := 0]
      
      if(iTrans < maxTrans){
        selData[, inStateTime := 1:.N, by = polNumb]
        if(iTrans> 1) selData[, delt1PayTime := inStateTime]
      }else{ iTrans == maxTrans
        selData[, diffPol := c(0,diff(as.numeric(as.character(polNumb))))]
        selData[, diffPay := c(0,diff(bookDate))]
        selData[, diffIndex := diffPol | diffPay]
        selData[, delt1PayTime := rowid(cumsum(diffIndex  == T))]
        selData[, diffPol := NULL]
        selData[, diffPay := NULL]
        selData[, diffIndex := NULL]
        selData[, inStateTime := delt1PayTime]
      }
      selData[, transStat := 0]
      selData[inStateTime == time2Trans, transStat := 1]
      selData[, indTime := 1:.N, by = polNumb]
      selData[, revIndTime := .N:1, by = polNumb]
      selData[revIndTime == 1, inProcTimeLast := inProcTime, by = polNumb ]
      selData[, inProcTimeLast := na.locf(inProcTimeLast, fromLast = TRUE), by = polNumb]
      selData[, inProcTime := inProcTimeLast -revIndTime + 1]
      selData[inProcTime == 0, inProcTime := 1]
      selData[, c('time2Trans', 'transStatLast', 'indTime', 'revIndTime', 'inProcTimeLast') := NULL]
    } else {
      setnames(selData, 'transStatLast', 'transStat')
      setnames(selData, 'time2Trans', 'inStateTime')
    }
    if(dropPolNumb) selData[, c('polNumb') := NULL]
    selData[, inStateTimeMax := max(selData$inStateTime)]
    removeEmptyLevelsDT(selData)
    transData[[iTrans]] <- selData
  }
  # Making sure that when 'transType' equals 'TN', 'outComeT' doesn't continue to have the previous value.
  # It should namely be equal to 0 in this case.
  llply(transData, function(xx) xx[transType == 'TN', outComeT := 0]) #changed 070520
  llply(transData, function(xx) xx[transStat == 0, outComeT := 0]) #changed 070520
  llply(transData, removeEmptyLevelsDT)
  return(transData)
}

## addLevelDT
#' addLevelDT is a function that adds a level called \code{newLevel} to the variable \code{catVar} in the dataset \code{inputDT}.
#'
#' @param inputDT Dataset used in this function.
#' @param catVar Name of the variable to which a new level should be added.
#' @param newLevel Name of the new level that should be added to the variable \code{catMerge}.
#'
#' @return  This function doesn't return anything.
#
addLevelDT <- function(inputDT, catVar, newLevel){
  # Check input parameters and certain variables in 'inputDT'.
  checkCharVec(list(catVar, newLevel))
  checkLength(list(catVar, newLevel), 1)
  checkDT(inputDT, catVar)
  
  indCatVar <- which(names(inputDT) == catVar)
  selFact <- inputDT[, .SD, .SDcols = indCatVar]
  levels(selFact[[1]]) <- unique(c(levels(selFact[[1]]), newLevel))
  set(inputDT, NULL, as.integer(indCatVar), selFact)
}

## varPrep
#' varPrep is a function that does the following for each dataset in \code{inputDT}:
#'  - In case the number of rows is larger than \code{nMaxNoMod}, bin (using catmerge) the variables 'inStateTime', 'inProcTime' and
#'    all the variables with variables with 'payTime' or 'incurTime' in their name.
#'  - In case the number of rows of the dataset is larger than \code{nMinMod}, the levels with frequency smaller than \code{nMinLev} are removed
#'    for each factor variable in \code{modVars}.
#'  - A new variable 'transMN' is defined. It equals 'transType'; and in case of no transition (e.g. 'transStat'==0) it is 'N'.
#'  - When \code{noIncur} is TRUE, the lines where 'transType'=='I' are removed and the lines where 'transType'=='PI' arerenamed
#'    to 'P' in 'transType' and 'transMN' columns.
#'  - When \code{binning} is TRUE and the number of rows of the dataset is larger than \code{nMaxNoMod},
#     the payment variables are grouped using the MN model.
#' @param inputDT List of datasets used in this function (output of transPrep).
#' @param nMinMod Minimum number of observations to model with covariates.
#' @param nMaxNoMod The number of observations that needs to be exceeded before any model is fit to the data.
#' @param nMaxLevInState Maximum number of levels for the inStateTime variable (maximum time an observation can stay in a state).
#' @param nMaxLevInStateLast Maximum number of levels for the inStateTime variable (maximum time an observation can stay in a state) for the last transition.
#' @param nMaxLevInProc Maximum number of levels for the inProcTime variable (maximum time an observation can stay in the time process).
#' @param nMaxLevInProcLast Maximum number of levels for the inProcTime variable (maximum time an observation can stay in the time process) for the last transition.
#' @param nMinTimeLev Number of values for each value of inStateTime.
#' @param nMinLev Number of values for each level of a covariate (min number of observations each  level  a factor  variable should have, if a level has less than that, it is removed).
#' @param modVars Additional variables for the time process modelling  - default value is NULL.
#' @param modVarsType Type of the additional variables for the time process modelling - default value is NULL.
#' @param nGroupsFin Maximal number of groups for a numeric variable that gets binned. This is the final binning, where the results of the binning
#'                   performed for each transition separately, are aggregated over the different transitions - default value is 15.
#' @param nGroupsTr Maximal number of groups for a numeric variable that gets binned. This is done for each transition separately - default value is 10.
#' @param noIncur Boolean indicating whether the incurred should be removed from the dataset - default value is TRUE.
#' @param binning Boolean indicating whether the payment variables should be binned or not - default value is FALSE.
#' @param verbose Boolean indicating whether output should be printed or not - default value is TRUE.
#'
#' @return A list that contains the following items:
#'         - A list of datasets (for each transition) on which everything in the description is applied.
#'         - A list of the split points for each of the grouped payment variables in those datasets (for each transition).
#
varPrep <- function(inputDT, nMinMod, nMaxLevdeltRep, nMaxNoMod, nMaxLevInState, nMaxLevInStateLast, nMaxLevInProc, nMaxLevInProcLast, nMinTimeLev, nMinLev, modVars = NULL, modVarsType = NULL, nGroupsFin = 15, nGroupsTr = 10, noIncur = TRUE, binning = FALSE, verbose = TRUE){#changed 280420
  # Check input parameters and certain variables in 'inputDT'.
  if(!is.list(inputDT)) stop("The 'inputDT' argument needs to be a list of data.tables objects.")
  if(is.null(modVars) + is.null(modVarsType) == 1) stop("Both 'modVars' and 'modVarsType' arguments needs to be NULL or both need to be different from NULL.")
  if(!(is.null(modVars) & is.null(modVarsType))){
    checkCharVec(list(modVars, modVarsType))
    checkEqualLength(list(modVars, modVarsType))
    #checkValues(list(modVarsType), list(c('fac', 'num')))
    #llply(inputDT, function(xx) checkDT(xx, c('polNumb', 'inStateTime', 'transType', 'inProcTime', 'transStat', modVars)))
    numInd <- which(modVarsType == 'num')
    facInd <- which(modVarsType == 'fac')
  } else {
    llply(inputDT, function(xx) checkDT(xx, c('polNumb', 'inStateTime', 'transType', 'inProcTime', 'transStat')))
    numInd <- c()
    facInd <- c()
  }
  
  checkWholeNumb(list(nMinMod, nMinTimeLev, nMinLev, nMaxLevInStateLast, nMaxLevInProcLast)) #changed 280420
  checkWholeNumb(list(nMaxNoMod, nMaxLevInState, nMaxLevInProc, nGroupsFin, nGroupsTr))
  checkLogicVec(list(verbose))
  checkLength(list(nMinMod, nMinTimeLev, nMinLev, nGroupsFin, nGroupsTr), 1)
  checkLength(list(nMaxNoMod, nMaxLevInState, nMaxLevInProc, verbose), 1)
  
  outputDT <- list()
  finSplits <- list()
  
  for(iList in 1:length(inputDT)){
    
    if(verbose) print(paste0('Prepping transition ', iList))
    # Select relevant variables for the modelling.
    if(!isNumericDT(inputDT[[iList]], 'inStateTime')) asNumericDT(inputDT[[iList]], c('inStateTime'))
    if(!isNumericDT(inputDT[[iList]], 'inProcTime')) asNumericDT(inputDT[[iList]], c('inProcTime'))
    if(length(facInd) > 0) asFactorDT(inputDT[[iList]], modVars[facInd])
    if(length(numInd) > 0) asNumericDT(inputDT[[iList]], modVars[numInd])
    
    dataFitTrans <- copy(inputDT[[iList]])
    
    # This gives 'deltVars': the names of the variables of 'dataFitTrans' with 'delt' in their name.
    deltVars <- names(dataFitTrans)[grep('delt', names(dataFitTrans))]
    deltVars <- c(deltVars, names(dataFitTrans)[grep('Delt', names(dataFitTrans))]) #changed 300420
    
    # This gives 'extravars2Merge': the names of the variables of 'dataFitTrans' with 'payTime' or 'incurTime' in their name.
    extraVars2Merge <- names(dataFitTrans)[grep('PayTime', names(dataFitTrans))]
    extraVars2Merge <- c(extraVars2Merge, names(dataFitTrans)[grep('IncurTime', names(dataFitTrans))])
    if(sum(names(dataFitTrans) == 'deltRep')) extraVars2Merge <- c(extraVars2Merge, 'deltRep')
    
    # This gives 'payNames': the names of the variables of 'dataFitTrans' with 'pay' but without 'time' or 'grouped' in their name.
    payNames <- deltVars[grep('Pay', deltVars)]
    payNames <- payNames[!grepl('Time', payNames)]
    payNames <- payNames[!grepl('Grouped', payNames)]
    
    # This gives 'deltNoPayNames': the names of the variables of 'dataFitTrans' without 'delt' in their name.
    deltNoPayNames <- deltVars[!(deltVars %in% payNames)]
    
    if(nrow(dataFitTrans) > nMaxNoMod){
      # Bin (using catmerge) the different variables in the various groups obtained above.
      if(iList != length(inputDT)){#changed 280420
        catMerge(dataFitTrans, 'inStateTime', nMinTimeLev, nMinMod, nMaxLevInState)
        catMerge(dataFitTrans, 'inProcTime', nMinTimeLev, nMinMod, nMaxLevInProc)
      } else {#changed 280420
        catMerge(dataFitTrans, 'inStateTime', nMinTimeLev, nMinMod, nMaxLevInStateLast)#changed 280420
        catMerge(dataFitTrans, 'inProcTime', nMinTimeLev, nMinMod, nMaxLevInProcLast)#changed 280420
      }#changed 280420
      
      if(length(extraVars2Merge) != 0){
        for(iVarPT in extraVars2Merge){
          if(iVarPT == 'deltRep'){
            catMerge(dataFitTrans, iVarPT, nMinTimeLev, nMinMod, nMaxLevdeltRep)
          }else{ 
            catMerge(dataFitTrans, iVarPT, nMinTimeLev, nMinMod, nMaxLevInState)  
          }
          
        }
      }
      extraVars2MergeTr <- c()
      if(length(extraVars2Merge)){
        extraVars2MergeTr <- paste0(extraVars2Merge, 'Trans')
      }
      if(iList>1){
        dataFitTrans <- dataFitTrans[, .SD, .SDcol = c(modVars, deltVars[!(deltVars %in% extraVars2Merge)], extraVars2MergeTr, 'polNumb', 'inStateTimeTrans', 'inProcTimeTrans', 'transType', 'transStat', 'outComeT', 'repYear','finYear', 'bookDate', 'inStateTimeMax', 'linkRatio')] #changed 140720
      }else{
        dataFitTrans <- dataFitTrans[, .SD, .SDcol = c(modVars, deltVars[!(deltVars %in% extraVars2Merge)], extraVars2MergeTr, 'polNumb', 'inStateTimeTrans', 'inProcTimeTrans', 'transType', 'transStat', 'outComeT', 'repYear','finYear', 'bookDate', 'inStateTimeMax')] #changed 140720
      }
    } else {
      setnames(dataFitTrans, 'inStateTime', 'inStateTimeTrans')
      setnames(dataFitTrans, 'inProcTime', 'inProcTimeTrans')
      setnames(dataFitTrans, extraVars2Merge, paste0(extraVars2Merge, 'Trans'))
    }
    
    if(nrow(dataFitTrans) > nMinMod){
      # For each factor variable in 'modVars', remove levels with frequency smaller than 'nMinLev'.
      if(!is.null(modVars)){
        for(iVar in 1:length(modVars)){
          if(modVarsType[iVar] == 'fac'){
            # Construct a frequency table of all levels of modVars[iVar] and define 'lev2Remove'.
            # This are the levels  with frequency smaller than 'nMinLev'.
            freqTabVar <- table(dataFitTrans[, .SD, .SDcol = modVars[iVar]])
            lev2Remove <- names(freqTabVar)[which(freqTabVar <= nMinLev)]
            if(length(lev2Remove) > 0){
              # Replace the levels of 'lev2Remove' by 'Missing', which is a value that is allowed after applying addLevelDT.
              addLevelDT(dataFitTrans, modVars[iVar], 'Missing')
              dataFitTrans[get(modVars[iVar]) %in% lev2Remove, modVars[iVar] := 'Missing']
              # In case all levels of the variable have a frequency smaller than 'nMinLev' or only one level has a frequency that is large enough,
              # this variable is removed.
              if(length(freqTabVar) - length(lev2Remove) <= 1){
                dataFitTrans[, modVars[iVar] := NULL]
              }
              # Remove empty levels from all the factor columns of 'dataFitTrans'.
              removeEmptyLevelsDT(dataFitTrans)
            }
          }
        }
      }
    }
    asFactorDT(dataFitTrans, 'inStateTimeTrans')
    asFactorDT(dataFitTrans, c('inProcTimeTrans'))
    
    if(nrow(dataFitTrans) > nMaxNoMod) asFactorDT(dataFitTrans, paste0(extraVars2Merge, 'Trans'))
    
    # Define new variable 'transMN'.
    # It equals 'transType'; and in case of no transition (e.g. 'transStat'==0) it is 'N'.
    dataFitTrans[transStat == 0, transMN := 'N']
    dataFitTrans[transStat == 1, transMN := transType]
    
    if(noIncur){
      # Remove the lines where 'transType'=='I' and rename 'transType'=='PI' to 'P' in 'transType' and transMN columns.
      var2Rem <- names(dataFitTrans)[grep('ncur', names(dataFitTrans))]
      if(length(var2Rem)){
        dataFitTrans[, (var2Rem) := NULL][]
      }
      dataFitTrans <- dataFitTrans[transType != 'I', ]
      dataFitTrans[transType == 'PI', transType := 'P']
      dataFitTrans[transMN == 'PI', transMN := 'P']
    }
    
    # varName <- 'delt1Pay'
    # nGroupsFin <- 20
    # nGroups <- 20
    # sampleSizeBin <- 50000
    #
    # library(tpSuite)
    # isNumericDT(dataFitTrans, varName)
    
    # dataDT <- dataFitTrans
    # dataDT[, delt1PayGrouped := NULL]
    
    if(nrow(dataFitTrans) > nMaxNoMod & binning){#changed
      # Group the payment variables, using the MN model.
      finSplits[[iList]] <- c()
      if(length(payNames)){
        if(verbose) print('>>> Binning phase')
        splits <- list()
        for(iSpl in 1:length(payNames)){
          set.seed(2021)
          splits[[iSpl]] <- unlist(createGroupedVar(dataFitTrans, payNames[iSpl], nGroupsFin, nGroupsTr, nMinLev)) #Jolien
          # set.seed(2021) #TODO
          # if(iList>1) splits[[iSpl]] <- unlist(createGroupedVarPay(dataFitTrans, payNames[iSpl], nGroupsFin, nGroupsTr, nMinLev)) #Jolien
        }
        # names(splits) <- payNames
        finSplits[[iList]] <- splits
      }
      payNamesGr <- c()
      if(length(payNames)){
        payNamesGr <- paste0(payNames, 'Grouped')
      }
      # dataFitTrans <- dataFitTrans[, .SD, .SDcol = which(!(names(dataFitTrans) %in% payNames))]
    }
    dataFitTrans[, last := NULL] #changed 290420
    outputDT[[iList]] <- copy(dataFitTrans)
  }
  return(list(data = outputDT, split = finSplits))
}

## catMerge
#' catMerge is a function that returns the input dataset where the variables in \code{varNameMerge} are  binned and
#' a new column varNameMerge+'Trans' is added.
#'
#' @param inputDT Dataset used in this function.
#' @param varNameMerge List of variable names that should be merged.
#' @param nMinLev Minimum number of observations for each level of the variables in \code{varNameMerge} - default value is 20.
#' @param nObsMin Minimum number of observations in the input dataset that are needed before the merging happens  - default value is 1000.
#' @param nMaxLevTime Maximum number of levels for the variables in \code{varNameMerge} - default value is NULL.
#'
#' @return The input dataset where the variables in \code{varNameMerge} are  binned and a new column varNameMerge+'Trans' is added.
#
catMerge <- function(inputDT, varNameMerge, nMinLev = 20, nObsMin = 1000, nMaxLevTime = NULL){
  # Check input parameters and certain variables in 'inputDT'.
  checkCharVec(list(varNameMerge))
  checkNumOrIntVec(list(nMinLev, nObsMin))
  checkWholeNumb(list(nMinLev, nObsMin))
  checkLength(list(nMinLev, nObsMin), 1)
  if(!is.null(nMaxLevTime)){
    checkWholeNumb(list(nMaxLevTime))
    checkLength(list(nMaxLevTime), 1)
  }
  checkDT(inputDT, varNameMerge)
  
  for(iVar in 1:length(varNameMerge)){
    # For each variable that has to be binned:
    if(nrow(inputDT) >= nObsMin){
      # Check on which values of the covariate the binning should be done.
      # This is done by creating a table of the covariate and then finding the index
      # on the table where the number of observations is bigger than 'nMinLev'.
      selCol <- as.integer(which(names(inputDT) == varNameMerge[iVar]))
      tabvarMerge <- table(inputDT[, .SD, .SDcol = selCol][[1]])
      # if(sum(tabvarMerge >= nMinLev) == 0) stop("Please lower the value for the 'nMinLev' argument") #removed 050520
      
      # Warning message when the value for the 'nMinLev' argument is too low.
      if(sum(tabvarMerge >= nMinLev) == 0){ #changed 050520
        warning("The value for the 'nMinLev' argument is too low. The mode is taken as a value for the 'nMinLev' argument.") #changed 050520
        nMinLev <- max(tabvarMerge) #changed 050520
      } #changed 050520
      indUpper <- length(tabvarMerge)
      while(tabvarMerge[indUpper] < nMinLev){
        indUpper <- indUpper - 1
      }
      # Make sure that number of levels is smaller than 'nMaxLevTime'.
      if(!is.null(nMaxLevTime)) indUpper <- min(nMaxLevTime, indUpper)
      indLower <- 1
      while(tabvarMerge[indLower] < nMinLev){
        indLower <- indLower + 1
      }
      
      # Remove existing transvariables of the selected variables.
      while(sum(names(inputDT) == paste0(varNameMerge[iVar], 'Trans')) > 0){
        inputDT[,  paste0(varNameMerge[iVar], 'Trans') := NULL]
      }
      # Create transvariables for the selected variable.
      inputDT[,  paste0(varNameMerge[iVar], 'Trans') := get(varNameMerge[iVar])]
      selColTrans <- as.integer(which(names(inputDT) == paste0(varNameMerge[iVar], 'Trans')))
      
      # Bin the variables, based on 'nMinLev' and 'nMaxLevTime'.
      catsOrder <- as.numeric(names(tabvarMerge))
      if(indUpper < length(tabvarMerge)) set(inputDT, as.integer(which(inputDT[, .SD, .SDcol = selCol][[1]] >= catsOrder[indUpper])), selColTrans, catsOrder[indUpper])
      if(indLower > 1) set(inputDT, as.integer(which(inputDT[, .SD, .SDcol = selCol][[1]] <= catsOrder[indLower])), selColTrans, catsOrder[indLower])
    }
  }
}

## catMerge
#' catMerge is a function that returns the input dataset where the variables in \code{varNameMerge} are  binned and
#' a new column varNameMerge+'Trans' is added.
#'
#' @param inputDT Dataset used in this function.
#' @param varNameMerge List of variable names that should be merged.
#' @param nMinLev Minimum number of observations for each level of the variables in \code{varNameMerge} - default value is 20.
#' @param nObsMin Minimum number of observations in the input dataset that are needed before the merging happens  - default value is 1000.
#' @param nMaxLevTime Maximum number of levels for the variables in \code{varNameMerge} - default value is NULL.
#'
#' @return The input dataset where the variables in \code{varNameMerge} are  binned and a new column varNameMerge+'Trans' is added.
#
catMergePay <- function(inputDT, varNameMerge, nMinLev = 20, nObsMin = 1000, nMaxLevTime = NULL){
  # Check input parameters and certain variables in 'inputDT'.
  checkCharVec(list(varNameMerge))
  checkNumOrIntVec(list(nMinLev, nObsMin))
  checkWholeNumb(list(nMinLev, nObsMin))
  checkLength(list(nMinLev, nObsMin), 1)
  if(!is.null(nMaxLevTime)){
    checkWholeNumb(list(nMaxLevTime))
    checkLength(list(nMaxLevTime), 1)
  }
  checkDT(inputDT, varNameMerge)
  
  for(iVar in 1:length(varNameMerge)){
    # For each variable that has to be binned:
    if(nrow(inputDT) >= nObsMin){
      # Check on which values of the covariate the binning should be done.
      # This is done by creating a table of the covariate and then finding the index
      # on the table where the number of observations is bigger than 'nMinLev'.
      selCol <- as.integer(which(names(inputDT) == varNameMerge[iVar]))
      tabvarMerge <- table(inputDT[, .SD, .SDcol = selCol][[1]])
      # if(sum(tabvarMerge >= nMinLev) == 0) stop("Please lower the value for the 'nMinLev' argument") #removed 050520
      
      # Warning message when the value for the 'nMinLev' argument is too low.
      if(sum(tabvarMerge >= nMinLev) == 0){ #changed 050520
        warning("The value for the 'nMinLev' argument is too low. The mode is taken as a value for the 'nMinLev' argument.") #changed 050520
        nMinLev <- max(tabvarMerge) #changed 050520
      } #changed 050520
      # indUpper <- length(tabvarMerge)
      # while(tabvarMerge[indUpper] < nMinLev){
      #   indUpper <- indUpper - 1
      # }
      # # Make sure that number of levels is smaller than 'nMaxLevTime'.
      # if(!is.null(nMaxLevTime)) indUpper <- min(nMaxLevTime, indUpper)
      # indLower <- 1
      # while(tabvarMerge[indLower] < nMinLev){
      #   indLower <- indLower + 1
      # }
      probInd = c()
      for(tabInd in 1:length(tabvarMerge)){
        if(as.numeric(as.character(tabvarMerge[tabInd])) < nMinLev) probInd = c(probInd, tabInd)
      }
      if(length(probInd)==1){
        if(probInd > 10){
          indLower = 1
          indUpper = probInd
        }else{
          indLower = probInd
          indUpper = length(tabvarMerge)
        }
      }else{
        if( min(probInd) > 10){
          indLower = 1
          indUpper = min(probInd)
        }else{
          if(max(probInd) < 10){
            indLower = max(probInd)
            indUpper = length(tabvarMerge)
          }else{
            indLower = min(probInd)
            indUpper = max(probInd)
          }
        }
      }
      # Remove existing transvariables of the selected variables.
      while(sum(names(inputDT) == paste0(varNameMerge[iVar], 'GrPay')) > 0){
        inputDT[,  paste0(varNameMerge[iVar], 'GrPay') := NULL]
      }
      # Create transvariables for the selected variable.
      inputDT[,  paste0(varNameMerge[iVar], 'GrPay') := get(varNameMerge[iVar])]
      selColTrans <- as.integer(which(names(inputDT) == paste0(varNameMerge[iVar], 'GrPay')))
      
      # Bin the variables, based on 'nMinLev' and 'nMaxLevTime'.
      catsOrder <- as.numeric(names(tabvarMerge))
      if(indUpper < length(tabvarMerge)) set(inputDT, as.integer(which(as.numeric(as.character(inputDT[, .SD, .SDcol = selCol][[1]])) >= catsOrder[indUpper])), selColTrans, catsOrder[indUpper])
      if(indLower > 1) set(inputDT, as.integer(which(as.numeric(as.character(inputDT[, .SD, .SDcol = selCol][[1]])) <= catsOrder[indLower])), selColTrans, catsOrder[indLower])
    }
  }
}
## inflCalc
#' inflCalc is a function that returns inflations based on an M-estimation with Tukey's biweight for each transition number.
#'
#' @param transDataPrep List of datasets used in this function, together with the split points for
#'                      each of the grouped payment variables in those datasets(output of varPrep).
#' @param nSamps Number of samples to take from the data - default value is 50000.
#' @param plotIt Boolean indicating whether a plot should be made of the different estimated inflation rates or not - default value is FALSE.
#' @param verbose Boolean indicating whether output should be printed or not - default value is TRUE.
#'
#' @return The inflations based on an M-estimation with Tukey's biweight for each transition number.
#
inflCalc <- function(transDataPrep, nSamps = 50000, plotIt = FALSE, verbose = TRUE){
  # Check input parameters.
  if(sum(names(transDataPrep) %in% c('data', 'split'))  != 2) stop("The 'transDataPrep' argument needs to be an output object of the 'varPrep' function.")
  checkLength(list(plotIt, verbose, nSamps), 1)
  checkLogicVec(list(plotIt, verbose))
  checkWholeNumb(list(nSamps))
  
  inflRate <- rep(NA, length(transDataPrep[[1]]))
  
  for(iTrans in 1:length(transDataPrep[[1]])){
    # For each transition number.
    
    if(verbose) print(paste0('Computing the inflation of transition ', iTrans))
    if(verbose) print('>>> Prepping the data')
    
    # Selection of the last lines containing a payment.
    # Through the function addFirstLast, the data transDataPrep[[1]][[iTrans]] is sorted based on the 'polNumb'.
    # Moreover, a new variable 'last' is defined, which is one for each last unique row, and zero otherwise.
    fitData <- addFirstLast(transDataPrep[[1]][[iTrans]], 'polNumb', F, T)
    fitData <- fitData[last == 1 & !(transMN %in% c('I', 'TN', 'N')), ][outComeT > 0, ][, last := NULL]
    removeEmptyLevelsDT(fitData)
    fitData <- copy(fitData[outComeT != 'Inf', ])
    
    if(verbose) print('>>> Fitting the basic Gamma model')
    
    # Fit a basic (Gamma) model, containing the financial year as a factor, and other influencial variables.
    if(iTrans == 1){
      inflForm <- as.formula('outComeT ~ finYear + inStateTimeTrans  + deltRepTrans')
    } else {
      inflForm <- as.formula('outComeT ~ finYear + inProcTimeTrans')
    }
    asFactorDT(fitData, 'finYear')
    if(nrow(fitData) > nSamps){
      inds <- sample(1:nrow(fitData), nSamps, replace = FALSE)
    } else {
      inds <- 1:nrow(fitData)
    }
    inflMod <- gam(inflForm, data = fitData[inds, ], family = Gamma(link = log))
    #inflMod <- glmrob(formula = inflForm,data = fitData[inds, ],control = glmrobMqle.control(maxit =500,tcc=0.4), family = Gamma(link = log))
    
    
    if(verbose) print('>>> Extracting the annual inflation rate, based on a robust GLM model')
    # The covariate estimates from the basic model are collected and an estimate of the annual inflation rate is extracted from it using a robust GLM model.
    finYearInd <- grepl('finYear', names(coef(inflMod)))
    inflFinYear <- c(as.numeric(coef(inflMod)[1]), as.numeric(coef(inflMod)[1] + as.numeric(coef(inflMod)[finYearInd])))
    tabFinYear <- table(fitData[inds, .(finYear)])
    tabFinYear <- tabFinYear[tabFinYear != 0]
    nClaimsYear <- as.numeric(tabFinYear)
    finYears <- as.numeric(names(tabFinYear))
    
    X <- finYears
    fitRobust3 <- rlm(inflFinYear ~ finYears, method = 'MM', weights = nClaimsYear)
    
    if(plotIt){
      fit.lin.regr <- lm(inflFinYear ~ finYears, weights = nClaimsYear)
      fitRobust1 <- rlm(inflFinYear ~ finYears, psi = psi.bisquare, weights = nClaimsYear)
      fitRobust2 <- rlm(inflFinYear ~ finYears, weights = nClaimsYear)
      
      plot(X, inflFinYear)
      lines(X, fit.lin.regr$coeff[1] + as.numeric(X)*fit.lin.regr$coeff[2])
      lines(X, fitRobust1$coeff[1] + X*fitRobust1$coeff[2], col = 3)
      lines(X, fitRobust2$coeff[1] + X*fitRobust2$coeff[2], col = 2)
      lines(X, fitRobust3$coeff[1] + X*fitRobust3$coeff[2], col = 4)
      legend(1, 3.5, col = c(1:4), lty = c(1,1,1,1), legend = c("Linear Regression", "Huber Psi", "Tukey Psi", "MM Method"))
    }
    
    #The MM method is chosen to compute the annual inflation rates.
    estMeanPay <- exp(fitRobust3$coeff[1] + X*fitRobust3$coeff[2])
    ratio <- rep(NA, length(estMeanPay) - 1)
    for(i in 2:length(estMeanPay)){
      ratio[i-1] <- (estMeanPay[i] - estMeanPay[i-1])/estMeanPay[i-1]
    }
    inflRate[iTrans] <- pmax(mean(ratio)*100, 0)
    
  }
  return(inflRate)
}

## addActualsPart1
#' addActualsPart1 is a function that defines the inflation rates for each transition number - financial year combination.
#' Next, 'mergedData' is defined, which is the data containing the last line for each claim for each transition number, ordered by \code{polNumb}.
#' In 'mergedData', all payment variables are actualized using the inflation rates explained before.
#'
#' @param transDataPrep List of datasets used in this function, together with the split points for
#'                      each of the grouped payment variables in those datasets(output of varPrep).
#' @param inflRates Vector of inflation rates, one for each transition.
#' @param minLag Minimal lag.
#' @param maxLag Maximal lag.
#' @param currentYear Value indicating the current year - default value is 2019.
#' @param verbose Boolean indicating whether output should be printed or not - default value is TRUE.
#'
#' @return 'mergedData' as described in this function description.
#
addActualsPart1 <- function(transDataPrep, inflRates, minLag, maxLag, currentYear = 2019, verbose = TRUE){
  
  # For each transition number, define 'fitData' as the dataset containing the lines where a transition happens.
  if(verbose) print('>>> Transforming the data')
  
  # Through the function addFirstLast, the data transDataPrep[[1]][[1]] is sorted based on the 'polNumb'.
  # Moreover, a new variable 'last' is defined, which is one for each last unique row, and zero otherwise.
  fitData <- addFirstLast(transDataPrep[[1]][[1]], 'polNumb', F, T)
  fitData <- fitData[last == 1, ][, last := NULL]
  fitData[, ind := 1]
  # Define 'tempData' as the data containing the last line for each claim for transition number 1.
  tempData <- fitData
  
  nTrans <- length(transDataPrep[[1]])
  
  for(iTrans in 2:nTrans){
    # Through the function addFirstLast, the data transDataPrep[[1]][[iTrans]] is sorted based on the 'polNumb'.
    # Moreover, a new variable 'last' is defined, which is one for each last unique row, and zero otherwise.
    fitData <- addFirstLast(transDataPrep[[1]][[iTrans]], 'polNumb', F, T)
    if(iTrans != nTrans){
      # Only the last row is selected for each polNumb
      fitData <- fitData[last == 1, ][, last := NULL]
      fitData[, ind := iTrans]
    } else {
      # For each polNumb, the rows are selected where a payment was made, or when it is the last line in the DB
      fitData <- fitData[last == 1 | transMN != 'N', ][, last := NULL]
      fitData[, ind := iTrans:(iTrans + .N - 1), by = polNumb]
    }
    # Define 'tempData' as the data containing the last line for each claim for each transition number.
    tempData <- rbind(tempData, fitData, fill = T)
  }
  
  # Define 'mergedData' as a copy of 'tempData' ordered by polNumb.
  mergedData <- copy(tempData[order(polNumb, ind),])
  
  if(verbose) print('>>> Introducing the inflation')
  inflRates <- 1 + (inflRates/100)
  # finYears <- sort(unique(mergedData$finYear)) #removed 13052020
  # finYearsRates <- inflRates[iTrans]^(currentYear - finYears) #removed 13052020
  setnames(mergedData, 'outComeT', 'rawOutComeT')
  mergedData$outComeT <- NA
  asNumericDT(mergedData, 'outComeT')
  for(iTrans in 1:nTrans){
    # Define inflation rates for each financial year in each transition.
    # This rate is defined as the inflation rate for the transition number to the power (currentYear - finYear).
    finYears <- sort(unique(mergedData[ind == iTrans, finYear]))
    finYearsRates <- inflRates[iTrans]^(currentYear - finYears)
    for(iYear in 1:length(finYears)){
      # Actualize the payments in 'mergedData' using the inflation rates per transition number - financial year combination.
      if(iTrans != nTrans){
        mergedData[finYear == finYears[iYear] & ind == iTrans, outComeT := rawOutComeT*(finYearsRates[iYear])]
      } else {
        mergedData[finYear == finYears[iYear] & ind >= iTrans, outComeT := rawOutComeT*(finYearsRates[iYear])]
      }
    }
  }
  
  # Define the lagged payment variables in 'mergedData' using the actualized 'outComeT'.
  if(verbose) print('>>> Creating lag variables')
  # The next lines are extracted from the 'addLaggedVars' function.
  mergedData <- merge(mergedData, mergedData[, .(nLines = .N), by = polNumb], all.x = TRUE)
  mergedData[transType %in% c('I', 'TN'), outComeT := 0]
  mergedData[, cumPay := cumsum(outComeT), by = polNumb]
  
  mergedData[, payInd := 1]
  mergedData[is.na(outComeT), payInd := 0]
  # mergedData[transType %in% c('TN', 'I'), payInd := 0]
  
  # Still define 'incurInd' and possibly the above definition is not right.
  polNumbPay0 <- mergedData[(payInd == 0L) & (ind == 1) ,]$polNumb
  polNumbPay1 <- mergedData[(payInd == 1L) & (ind == 1) ,]$polNumb
  # polNumbInc0 <- inputDT[(incurInd == 0L) & (ind == 1) ,]$polNumb
  # polNumbInc1 <- inputDT[(incurInd == 1L) & (ind == 1) ,]$polNumb
  mergedData[, outComeT := round(outComeT, digits = 2)]
  
  for(iLag in minLag:maxLag){
    
    if(verbose) print(paste0(paste0('>>> >>> Creating the lag ', iLag), ' variables.'))
    
    # Define correct names for the variables that will be created in this loop.
    payName <- paste0(paste0('delt', iLag), 'Pay')
    # cumPayName <- paste0(paste0('cumDelt', iLag), 'Pay') #removed 250520
    # incurName <- paste0(paste0('delt', iLag), 'Incur')
    
    # Define 'deltPay'.
    # mergedData[payInd == 1L, temp := cumPay - shift(cumPay, n = iLag + 1, fill = NA, type = "lag"), by = polNumb] #removed 250520
    # mergedData[ (polNumb %in% polNumbPay0) & (payInd == 1L), temp := cumPay - (shift(rep(0, length(cumPay)), n = iLag, fill = NA) + shift(cumPay, n = iLag + 1, fill = 0)), by = polNumb] #removed 250520
    # mergedData[ (polNumb %in% polNumbPay1) &(payInd == 1L), temp := cumPay - shift(cumPay, n = iLag + 1, fill = NA, type = "lag"), by = polNumb] #removed 250520
    # mergedData[, temp := na.locf(temp, na.rm = FALSE), by = polNumb] #removed 250520
    # mergedData[, temp2 := cumPay - temp] #removed 250520
    # if(sum(names(mergedData) == payName) > 0) mergedData[, c(payName) := NULL] #removed 250520
    # if(sum(names(mergedData) == cumPayName) > 0) mergedData[, c(cumPayName) := NULL] #removed 250520
    # mergedData[, temp := round(temp, digits = 2)] #removed 250520
    # mergedData[, temp2 := round(temp2, digits = 2)] #removed 250520
    # setnames(mergedData, 'temp', payName) #removed 250520
    # setnames(mergedData, 'temp2', cumPayName) #removed 250520
    
    mergedData[, paste0(paste0('delt', iLag), 'Pay') := NULL]
    
    if(iLag == 0){
      
      mergedData[, temp := shift(cumPay, n = iLag + 1, type = "lag"), by = polNumb]
      mergedData[!((payInd == 0L) & (ind == nLines)), temp := NA]
      mergedData[ (polNumb %in% polNumbPay0) & (payInd == 1L), temp := cumPay - (shift(rep(0, length(cumPay)), n = iLag, fill = NA) + shift(cumPay, n = iLag + 1, fill = 0)), by = polNumb]
      mergedData[ (polNumb %in% polNumbPay1) & (payInd == 1L), temp := cumPay - shift(cumPay, n = iLag + 1, fill = NA, type = "lag"), by = polNumb]
      mergedData[, temp := na.locf(temp, na.rm = FALSE), by = polNumb]
      if(sum(names(mergedData) == payName) > 0) mergedData[, c(payName) := NULL]
      mergedData[ind == 1 & (payInd == 1L), temp := cumPay]
      mergedData[, temp := round(temp, digits = 2)]
      setnames(mergedData, 'temp', payName)
      
    } else {
      
      cumPayName <- paste0(paste0('cumDelt', iLag), 'Pay')
      if(sum(names(mergedData) == cumPayName) > 0) mergedData[, c(cumPayName) := NULL]
      mergedData[, temp := shift(delt0Pay, n = iLag, type = "lag"), by = polNumb]
      setnames(mergedData, 'temp', payName)
      
      mergedData[, temp2 := shift(cumPay, n = iLag, type = "lag"), by = polNumb]
      mergedData[, temp2 := round(temp2, digits = 2)]
      setnames(mergedData, 'temp2', cumPayName)
      
      # if(iLag == 1){ #changed 250520
      #   mergedData[, cumDelt1Pay := round(cumPay - delt0Pay, digits = 2)] #changed 250520
      #   mergedData[ind == 1, cumDelt1Pay := NA] #changed 250520
      # } else if(iLag == 2){ #changed 250520
      #   mergedData[, cumDelt2Pay := round(cumPay - delt0Pay - delt1Pay, digits = 2)] #changed 250520
      #   mergedData[ind %in% c(1, 2), cumDelt2Pay := NA] #changed 250520
      # } else if(iLag == 3){ #changed 250520
      #   mergedData[, cumDelt3Pay := round(cumPay - delt0Pay - delt1Pay - delt2Pay, digits = 2)] #changed 250520
      #   mergedData[ind %in% c(1, 2, 3), cumDelt3Pay := NA] #changed 250520
      # } else { #changed 250520
      #   stop("The variable 'cumDelt4Pay' (and higher lags) is not implemented yet in the 'addActualsPart1' function.") #changed 250520
      # } #changed 250520
      
    }
    
    # # Define 'deltIncur'
    # inputDT[ (polNumb %in% polNumbInc0) & (incurInd == 1L), temp := incur - (shift(rep(0,length(incur)),n=(iLag-1),fill=NA) + shift(incur,n=iLag,fill=0) ), by = polNumb]
    # inputDT[ (polNumb %in% polNumbInc1) &(incurInd == 1L),temp := incur - shift(incur, n = iLag, fill = NA, type = "lag"), by = polNumb]
    # inputDT[, temp := na.locf(temp, na.rm = FALSE), by = polNumb]
    # if(sum(names(inputDT) == incurName) > 0) inputDT[, c(incurName) := NULL]
    # setnames(inputDT, 'temp', incurName)
  }
  return(mergedData)
}

## addActualsPart2
#' addActualsPart2 is a function that merges the relevant columns of \code{transDataPrep} and \code{mergedData} for each transition number.
#'
#' @param transDataPrep List of datasets used in this function, together with the split points for
#'                      each of the grouped payment variables in those datasets(output of varPrep).
#' @param mergedData Dataset used in this function (output of addActualsPart1).
#' @param verbose Boolean indicating whether output should be printed or not - default value is TRUE.
#'
#' @return A list of datasets (one for each transition number) as a result of merging the relevant columns of \code{transDataPrep} and \code{mergedData}.

addActualsPart2 <- function(transDataPrep, mergedData, verbose = TRUE){
  # Check input parameters and certain variables in 'mergedData'.
  checkDT(mergedData, 'polNumb')
  if(sum(names(transDataPrep) %in% c('data', 'split')) != 2) stop("The argument 'transDataPrep' should be an output object of the function 'varPrep'.")
  checkLength(list(verbose), 1)
  checkLogicVec(list(verbose))
  
  # This gives 'deltNames': names of variables of 'mergedData' with 'delt' in their name, but no 'Grouped', 'Time' and 'Rep'. 'delt0Pay' is not contained in 'deltNames'
  # This gives 'deltGrouped': names of variables of 'mergedData' with 'Grouped' and 'delt' in their name.
  deltNames <- names(mergedData)[grep('delt', names(mergedData))]
  deltGrouped <- deltNames[grepl('Grouped', deltNames)]
  deltNames <- deltNames[!grepl('Grouped', deltNames) & !grepl('Time', deltNames) & !grepl('Rep', deltNames)]
  deltNames <- deltNames[deltNames != 'delt0Pay'] #changed 200520
  
  # This gives 'cumulNames': names of variables of 'mergedData' with 'cum' in their name, but 'cumPay' is not contained in 'cumulNames'.
  cumulNames <- names(mergedData)[grep('cum', names(mergedData))]
  cumulNames <- cumulNames[cumulNames != 'cumPay']
  
  # Select the relevant column in 'mergedData', saved in 'mergedDataRed'. This are the names in 'cumulNames' and 'deltNames', combined with 'polNumb', 'outComeT' and 'ind'.
  mergedDataRed <- copy(mergedData[, .SD, .SDcol = which(names(mergedData) %in% c(cumulNames, deltNames, 'polNumb', 'outComeT', 'ind'))])
  
  transDataPrepNew <- list()
  nTrans <- length(transDataPrep[[1]])
  
  for(iTrans in 1:nTrans){
    if(verbose) print(paste0('>>> Fixing transition ', iTrans))
    # Define 'mergedDataRedRed', part of 'mergedDataRed' concerning transition number 'iTrans'.
    if(iTrans != nTrans){
      mergedDataRedRed <- copy(mergedDataRed[ind == iTrans, ])
    } else {
      mergedDataRedRed <- copy(mergedDataRed[ind >= iTrans, ])
    }
    selVars <- names(mergedDataRedRed)[laply(mergedDataRedRed, function(xx) sum(is.na(xx))) != nrow(mergedDataRedRed)]
    if(iTrans != nTrans) selVars <- selVars[selVars != 'ind']
    mergedDataRedRed <- copy(mergedDataRedRed[, .SD, .SDcol = which(names(mergedDataRedRed) %in% selVars)])
    var2Drop <- names(mergedDataRedRed)[names(mergedDataRedRed) != 'polNumb']
    
    tempMat <- as.matrix(aaply(var2Drop, 1, function(xx) !grepl(xx, names(transDataPrep[[1]][[iTrans]]))))
    keepCols <- aaply(tempMat, which(dim(tempMat) == length(transDataPrep[[1]][[iTrans]])), all)
    transDataPrepRed <- copy(transDataPrep[[1]][[iTrans]][, .SD, .SDcol = keepCols])
    # Merge 'transDataPrepRed' and 'mergedDataRedRed' in 'transDataPrepNew'.
    if(iTrans != nTrans){
      transDataPrepNew[[iTrans]] <- unique(merge(transDataPrepRed, mergedDataRedRed, all.x = TRUE))
    } else {
      transDataPrepRed[, transStat_temp := transStat]
      transDataPrepRed[last == 1 & transMN != 'P', transStat_temp := 0]
      transDataPrepRed[, ind := nTrans + cumsum(transStat_temp), by = polNumb]
      transDataPrepRed[transStat_temp == 1, ind := ind - 1]
      transDataPrepRed[, polNumb_temp := paste(polNumb, ind, sep = '_')]
      mergedDataRedRed[, polNumb_temp := paste(polNumb, ind, sep = '_')]
      mergedDataRedRed[, polNumb := NULL]
      mergedDataRedRed[, ind := NULL]
      transDataPrepRed[, transStat_temp := NULL]
      setkey(transDataPrepRed, 'polNumb_temp')
      setkey(mergedDataRedRed, 'polNumb_temp')
      transDataPrepNew[[iTrans]] <- unique(merge(transDataPrepRed, mergedDataRedRed, all.x = TRUE))[, polNumb_temp := NULL]
      transDataPrepNew[[iTrans]] <- transDataPrepNew[[iTrans]][order(polNumb, ind, bookDate), ]
    }
    suppressWarnings(transDataPrepNew[[iTrans]][, c(deltGrouped, 'last') := NULL])
  }
  return(transDataPrepNew)
}

## binPayVars
#' binPayVars is a function that defines the payment variables that should be binned.
#' Finally, it returns a list that contains the following items:
#'     - A list of datasets, containing the unique lines for each transition.
#'     - A list of the split points for each of the payment variables (for each transition).
#'
#' @param inputDT List of datasets used in this function (output of addActualsPart2).
#' @param nMaxNoMod Maximum number of observations to model without covariates.
#' @param nGroupsFin Maximal number of groups for a numeric variable that gets binned. This is the final binning, where the results of the binning
#'                   performed for each transition separately, are aggregated over the different transitions - default value is 15.
#' @param nGroupsTr Maximal number of groups for a numeric variable that gets binned. This is done for each transition separately - default value is 10.
#' @param noDelt0PayTime Boolean indicating whether delt0PayTime should not be used in the binning or not - default value is FALSE.
#' @param noDelt0Pay Boolean indicating whether delt0Pay should not be used in the binning or not - default value is FALSE.
#' @param noDeltPayVars Boolean indicating whether the payment variables (variables with 'Pay' but without 'delt', 'Time' or 'Grouped' in their name)
#'                      should not be used in the binning or should - default value is FALSE.
#' @param verbose Boolean indicating whether output should be printed or not - default value is TRUE.
#'
#' @return A list that contains the following items:
#'         - A list of datasets, containing the unique lines for each transition.
#'         - A list of the split points for each of the payment variables (for each transition).
#
binPayVars <- function(inputDT, nMaxNoMod, nGroupsFin, nGroupsTr, noDelt0PayTime = FALSE, noDelt0Pay = FALSE, noDeltPayVars = FALSE, verbose = TRUE){#changed 250520
  # Check input parameters.
  if(!is.list(inputDT)) stop("The 'inputDT' argument needs to be a list of data.tables objects.")
  
  checkWholeNumb(list(nMaxNoMod, nGroupsFin, nGroupsTr))
  checkLogicVec(list(verbose, noDeltPayVars))#changed 300420
  checkLength(list(nMaxNoMod, verbose, nGroupsFin, nGroupsTr, noDeltPayVars), 1)#changed 300420
  
  outputDT <- list()
  finSplits <- list()
  
  for(iList in 1:length(inputDT)){ #changed 180520
    
    if(verbose) print(paste0('Prepping transition ', iList))
    
    dataFitTrans <- copy(inputDT[[iList]]) #changed 180520
    
    # This gives 'deltVars': the names of the variables of 'dataFitTrans' with 'delt' in their name.
    deltVars <- names(dataFitTrans)[grep('delt', names(dataFitTrans))]
    
    # This gives 'payNames': the names of the variables of 'dataFitTrans' with 'delt' and 'Pay' but without 'Time' or 'Grouped' in their name.
    payNames <- deltVars[grep('Pay', deltVars)]
    payNames <- payNames[!grepl('Time', payNames)]
    payNames <- payNames[!grepl('Grouped', payNames)]
    
    # This gives 'cumPayName': the names of the variables of 'dataFitTrans' with 'cumDelt' in their name.
    cumPayName <- names(dataFitTrans)[grep('cumDelt', names(dataFitTrans))]
    
    if(noDeltPayVars){#changed 300420
      # payNames <- payNames[!(payNames %in% 'delt')] #removed 040520
      # The names in 'payNames' containing 'delt' are removed.
      payNames <- payNames[!grepl('delt', payNames)] #changed 040520
      for(iLag in 0:length(payNames)){ #changed 200520
        # The delt-Pay variables are removed from 'dataFitTrans'.
        if(sum(names(dataFitTrans) == paste0(paste0('delt', iLag), 'Pay')) != 0){ #changed 200520
          dataFitTrans[, paste0(paste0('delt', iLag), 'Pay') := NULL] #changed 300420
        } #changed 200520
      } #changed 300420
    }
    
    # 'payNames' combines the names is 'payNames' originally and 'cumPayName'. #removed 250520
    payNames <- unique(c(cumPayName, payNames)) #removed 250520
    
    if(noDelt0PayTime){
      # 'delt0PayTime' is removed from 'dataFitTrans'.
      if(sum(names(dataFitTrans) == 'delt0PayTime') != 0){ #changed 200520
        dataFitTrans <- dataFitTrans[, .SD, .SDcol = which(!grepl('delt0PayTime', names(dataFitTrans)))]
      } #changed 200520
    }
    
    if(noDelt0Pay){#changed 250520
      # 'delt0Pay' is removed from 'dataFitTrans'.#changed 250520
      if(sum(names(dataFitTrans) == 'delt0Pay') != 0){ #changed 250520
        dataFitTrans <- dataFitTrans[, .SD, .SDcol = which(!grepl('delt0Pay', names(dataFitTrans)))]#changed 250520
        payNames <- payNames[payNames != 'delt0Pay'] #changed 250520
      } #changed 250520
    }
    
    # 'payNames' combines the names is 'payNames' originally and 'cumPayName'. #changed 250520
    payNames <- unique(c(cumPayName, payNames)) #changed 250520
    
    if(nrow(dataFitTrans) > nMaxNoMod){
      # For each variable name in 'payName', find splits using the 'createGroupedVars' function.
      finSplits[[iList]] <- c()
      if(length(payNames)){
        if(verbose) print('>>> Binning phase')
        
        splits <- list()
        for(iSpl in 1:length(payNames)){
          print(payNames[iSpl])
          splits[[iSpl]] <- unlist(createGroupedVar(dataFitTrans, payNames[iSpl], nGroupsFin, nGroupsTr))
        }
        finSplits[[iList]] <- splits
      }
      #dataFitTrans <- dataFitTrans[, .SD, .SDcol = which(!(names(dataFitTrans) %in% payNames))]
    }
    outputDT[[iList]] <- copy(unique(dataFitTrans))
  }
  return(list(data = outputDT, split = finSplits))
}

## actuaPrep
#' actuaPrep is a function that calculates the inflations (in case \code{inflRates} is NULL) and actualizes
#' the data afterwards. In case \code{noBinning} is FALSE, the payment variables are binned as well.
#'
#' @param transDataPrep List of datasets used in this function, together with the split points for
#'                      each of the grouped payment variables in those datasets(output of varPrep).
#' @param maxLag Maximal lag.
#' @param noBinning Boolean indicating whether the payment variables should be binned or not (TRUE = no binning, FALSE = binning) - default value is FALSE.
#' @param inflRates Inflations for each transition separately. In case this is NULL, the inflations are
#'                  computed via 'inflCalc' - default value is NULL.
#' @param noDelt0PayTime Boolean indicating whether delt0PayTime should not be used in the binning or not - default value is TRUE.
#' @param noDelt0Pay Boolean indicating whether delt0Pay should not be used in the binning or not - default value is TRUE.
#' @param noDeltPayVars Boolean indicating whether the payment variables should not be used in the binning or not - default value is FALSE.
#' @param currentYear Value indicating the current year - default value is 2019.
#' @param nGroupsFin Maximal number of groups for a numeric variable that gets binned. This is the final binning, where the results of the binning
#'                   performed for each transition separately, are aggregated over the different transitions - default value is 15.
#' @param nGroupsTr Maximal number of groups for a numeric variable that gets binned. This is done for each transition separately - default value is 10.
#' @param minLag Minimal lag - default value is 1.
#' @param nSamps Number of samples to take from the data - default value is 50000.
#' @param nMaxNoMod Maximum number of observations to model without covariates.
#' @param verbose Boolean indicating whether output should be printed or not - default value is TRUE.
#'
#' @return A list that contains the following items:
#'         - A list of datasets (for each transition) where the data is actualized and binned (in case \code{noBinning} is FALSE).
#'         - A list of inflations (for each transition) that is used to actualize the data.
#
actuaPrep <- function(transDataPrep, maxLag, noBinning = FALSE, inflRates = NULL, noDelt0PayTime = TRUE, noDelt0Pay = TRUE, noDeltPayVars = FALSE, currentYear = 2019, nGroupsFin = 15, nGroupsTr = 10, minLag = 0, nSamps = 50000, nMaxNoMod = 50, verbose = TRUE){ #changed 250520
  
  # In case the argument 'inflRates' is NULL, the inflation is calculated for each transition seperately.
  # Otherwise, it is checked that the argument 'inflRates' has an inflation for each transition seperately.
  if(is.null(inflRates)){
    if(verbose) print('Computing the inflation for each transition separately')
    inflRates <- inflCalc(transDataPrep, nSamps, FALSE, verbose)
  } else {
    if(length(inflRates) != length(transDataPrep[[1]])) stop("The argument 'inflRates' needs to be of the same length as the number of transitions, stored in the data element of the 'transDataPrep' object.")
    checkNumVec(list(inflRates))
  }
  
  # No checks of the arguments are added here, since they will be checked in each function separately.
  
  # Actualizing the payment variables.
  if(verbose) print('Actualizing the payment variables')
  mergedData <- addActualsPart1(transDataPrep, inflRates, minLag, maxLag, currentYear, verbose)
  transDataPrep <- addActualsPart2(transDataPrep, mergedData, verbose) #changed 13052020
  
  # Binning the payment variables when the argument 'noBinning' is FALSE.
  if(!noBinning){
    if(verbose) print('Binning the payment variables')
    transDataPrep <- binPayVars(transDataPrep, nMaxNoMod, nGroupsFin, nGroupsTr, noDelt0PayTime, noDelt0Pay, noDeltPayVars, verbose) #changed 300420
  }
  
  # Remove the variable 'last' in the data when it is still present.
  for(iTrans in 1:length(transDataPrep$data)){ #changed 290420
    if(sum(names(transDataPrep$data[[iTrans]]) == 'last') == 1){ #changed 290420
      transDataPrep$data[[iTrans]][, last := NULL] #changed 290420
    } #changed 290420
  } #changed 290420
  
  return(list(data = transDataPrep, infl = inflRates))
}

ppPlotSplicing <- function(testDT, samps){
  
  if(is.data.table(testDT)){
    sampDT <- list()
    sampDT[[1]] <- testDT
  } else {
    sampDT <- testDT
  }
  
  obsY <- sampDT[[listNr]]$outComeT
  
  allSamps <- unlist(samps)
  predY <- allSamps[sample(1:length(allSamps), 100000)]
  predY <- allSamps
  
  ppOutput <- function(cdfObj, obsVal){
    cdfVals <- rep(NA, length(obsVal))
    for(iVal in 1:length(obsVal)){
      cdfVals[iVal] <- environment(cdfObj)$y[which.min(environment(cdfObj)$x < obsVal[iVal])]
    }
    return(cdfVals)
  }
  
  cdfObs <- ecdf(obsY)
  cdfPred <- ecdf(predY)
  obsZ <- ppOutput(cdfObs, quantile(obsY, seq(0.01, 0.99, 0.01)))
  predZ <- ppOutput(cdfPred, quantile(obsY, seq(0.01, 0.99, 0.01)))
  plot(obsZ, predZ, type = 'l')
  lines(c(0, 1000000), c(0, 1000000), col = 4)
  
}

qqPlotSplicing <- function(testDT, samps){
  
  if(is.data.table(testDT)){
    sampDT <- list()
    sampDT[[1]] <- testDT
  } else {
    sampDT <- testDT
  }
  
  quantsTh <- seq(0.01, 0.99, 0.01)
  valObs <- quantile(sampDT[[listNr]][, outComeT], quantsTh)
  quantsEmpMN <- rep(NA, length(quantsTh))
  
  for(iQ in 1:length(valObs)){
    quantsEmpMN[iQ] <- sum(laply(samps, function(xx) sum(xx < valObs[iQ])))/(length(samps)*nSamps)
  }
  
  plot(quantsTh, quantsEmpMN, type = 'l', ylim = c(0, 1), xlim = c(0, 1))
  lines(c(0, 1), c(0, 1), col = 2)
  
}

createIBNRDT <- function(microDT, vectorOfYearlyIBNRCounts,initPay=0.0,refYear = 2013 ,perLen = 30,verboseDevelopment=F){
  
  firstTransitionTrainDT <- unique(microDT,by='polNumb')
  listOfYearlyDT <- list()
  numberOfIBNRYears <- length(vectorOfYearlyIBNRCounts)
  
  maxPolNumb <- max(as.integer(firstTransitionTrainDT$polNumb))
  if(verboseDevelopment) print(paste0('maxPolNumb is: ', maxPolNumb))
  totalNumberOfIBNR <- sum(vectorOfYearlyIBNRCounts)
  if(verboseDevelopment) print(paste0('total number of IBNR is: ', totalNumberOfIBNR))
  IBNRPolNumbs <- sample(seq(maxPolNumb+1,maxPolNumb+3*totalNumberOfIBNR),totalNumberOfIBNR)
  if(verboseDevelopment) print(paste0('number of createdPolnumbs is: ', length(IBNRPolNumbs)))
  remainingPolNumbs <- IBNRPolNumbs
  for(accidentYearIndex in 1:numberOfIBNRYears){
    if(vectorOfYearlyIBNRCounts[accidentYearIndex]>0){
      currentYear <- names(vectorOfYearlyIBNRCounts)[accidentYearIndex]
      currentYearIBNRCounts <- vectorOfYearlyIBNRCounts[accidentYearIndex]
      if(verboseDevelopment)print(paste0('currentYearIBNR count: ',currentYearIBNRCounts))
      currentYearPolNumbs <- as.factor(remainingPolNumbs[1:currentYearIBNRCounts])
      remainingPolNumbs <- remainingPolNumbs[min(currentYearIBNRCounts+1,length(remainingPolNumbs)): length(remainingPolNumbs)]
      currentYearTrainDT <- firstTransitionTrainDT[finYear == currentYear,]
      currentYearIBNRDT <- as.data.table(data.frame(polNumb=currentYearPolNumbs))
      currentYearIBNRDT$fastRep <- "0"
      currentYearIBNRDT$deltRepTrans <- as.factor((refYear - (as.numeric(as.character(currentYear))))*(365/perLen))# assume occurence on 1/1 of acc Year
      currentYearIBNRDT$inStateTimeTrans <-1
      currentYearIBNRDT$inStateTime <-1
      currentYearIBNRDT$inProcTimeTrans <- 1
      currentYearIBNRDT$inProcTime <- 1
      currentYearIBNRDT$transType <- "P"
      currentYearIBNRDT$transStat <- 0
      currentYearIBNRDT$finYear <- refYear
      currentYearIBNRDT$bookDate <- as.Date.character(paste0("01/01/",refYear),"%d/%m/%Y")
      currentYearIBNRDT$inStateTimeMax <- 158
      currentYearIBNRDT$transMN <- "N"
      currentYearIBNRDT$outComeT <-initPay
      listOfYearlyDT[[accidentYearIndex]] <- currentYearIBNRDT
    }
  }
  return(listOfYearlyDT)
}

asNumericDT = function (inputDT, colNamesToBeTransformed = NULL){
  if (is.null(colNamesToBeTransformed)) 
    stop("Please provide the column names to the \"colNamesToBeTransformed\" argument of which the values of the whole columns need to be transformed to a numeric value.")
  checkCharVec(list(colNamesToBeTransformed))
  checkDT(inputDT, colNamesToBeTransformed)
  indexColumns <- sort(match(colNamesToBeTransformed, names(inputDT)))
  for (iCol in indexColumns) {
    set(inputDT, NULL, as.integer(iCol), as.numeric(as.character(inputDT[, 
                                                                         .SD, .SDcols = iCol][[1]])))
  }
}