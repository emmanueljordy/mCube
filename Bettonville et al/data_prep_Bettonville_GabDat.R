library(actuar)
library(uniftest)
library(EnvStats)
library(data.table)
library(parallel)
library(foreach)
library(doParallel)
library(plyr)
library(MASS)
library(ChainLadder)
library(mgcv)
library(gamlss)
library(gamlss.dist)
library(gamlss.mx) 
library("reshape2")
library(DT)
library(latex2exp)
library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)


db <- copy(data)

#To apply the multi-state model, each entry in the data must correspond to the information of one development year. This piece of code transforms lines into columns.

payments <- db %>% 
  dplyr::select(ClNr,
                starts_with("Pay")) %>% 
  gather(-ClNr,
         key= "variable",
         value= "Paiement")

open <- db %>% 
  dplyr::select(ClNr,
                starts_with("Open")) %>% 
  gather(-ClNr,
         key= "variable",
         value= "Ouvert")

payments["variable"] <- substring(payments$variable,4,5)# keep only the index of the payment i.E. Pay00 => 00
open["variable"] <- substring(open$variable,5,6)# Keep only the index of the open indicator i.e. Open00 => 00

db <- db %>% inner_join(payments, by = c("ClNr" = "ClNr"))
db <- db %>% left_join(open, by = c("ClNr" = "ClNr",  "variable" = "variable"))

db <- db %>% dplyr::select(-starts_with("Pay"), -starts_with("Open"))


#Creation of the needed variables Development year and Settlement year. Reopenings are removed, the claim is considered as remained open till the true settlement. The part of the claims that is not yet known is removed from the data. 

#Create development year => start from devYear = 1
db["DevYear"] <- as.numeric(db$variable) + 1

#Delete half of the rectangle that is not yet known
db <- db %>% dplyr::filter(RY + DevYear - 1  < 2006)#2013 because DevYear starts at 1 => this represents the part that is known to us

###Ajouter l'année de clôture + Enlever les réouvertures

#setyear <- db %>% dplyr::filter(Ouvert == 1) %>% dplyr::group_by(ClNr) %>% dplyr::summarise(max = max(DevYear))
#db <- db %>% left_join(setyear,by = c("ClNr" = "ClNr"))
#db$max <- db$max - db$RepDel# to take into account that some claims have a reporting delay and max counts the lines for year not reported as open

#db["SetYear"] <- ifelse(is.na(db$max) &  db$RepDel == 0, db$AY+db$RepDel, ifelse(db$AY + db$max  >= 2013, 0, db$AY + db$max))
# there is a mistake in the logic for set year as it those not compute it correctly for claims that have a reporting delay and have only one payment in a year before they are closed 
db["SetYear"] <- ifelse(db$SetYear >= 2006| is.na(db$SetYear), 0,db$SetYear )
db["RepYear"] <- db$AY + db$RepDel
db["Payment"] <- db$Paiement
db <- db %>% 
  dplyr::select(-Ouvert, -variable, -Paiement) %>%  
  dplyr::filter(RY + DevYear -1 <= SetYear | SetYear == 0)

MaxObs = max(db$RY)
MaxObs

db[is.na(db$Payment),9] <- 0
# State: State occupied by the claim at the end of the current considered year.
long <- db %>% filter(RY != SetYear) %>% filter(RY != MaxObs) # claims that have at least 2 years observed
short <- db %>% filter(RY == SetYear | RY == MaxObs)# claims that have only 1 year observed

for(clN in unique(long$ClNr)){# for each claim in long, count the number of payments 
  subClN <- long %>% filter(ClNr == clN) 
  if(subClN$Payment[1] == 0){subClN$NbPayment[1] = 0}
  else{subClN$NbPayment[1] = 1}
  for(i in 2:nrow(subClN)){
    if(subClN$Payment[i] == 0){
      subClN$NbPayment[i] = subClN$NbPayment[i-1]
    }
    else{
      subClN$NbPayment[i] = subClN$NbPayment[i-1] + 1 
    }
  }
  if(clN == long$ClNr[1]){ 
    final <- subClN
  }
  else{ 
    final <- rbind(final,subClN)
  }
}

short$NbPayment <- ifelse(short$Payment !=0,1,0) 

db <- rbind(short,final); db <- db[order(db$ClNr),] 

db$State <- ifelse(db$RY+db$DevYear-1 == db$SetYear, 
                   ifelse(db$Payment == 0,
                          'Closed0', 
                          'Closed1'), # closure with or without payment 
                   ifelse(db$RepYear >= db$DevYear, 
                          db$NbPayment,  
                          db$NbPayment))# state occupied at the end of the current year 


#Previous state: The state occupied during the preceding development year.

db$previous_state <- c(-1,db$State[-length(db$State)])
db$previous_state <- ifelse(db$DevYear == 1,-1,db$previous_state)#state occupied at end of previous year, -1 if no prev state

long <- db %>% filter(RY != SetYear) %>% filter(RY != MaxObs) 
short <- db %>% filter(RY == SetYear | RY == MaxObs)

for(clN in unique(long$ClNr)){ 
  subClN <- long %>% filter(ClNr == clN) 
  subClN$Time[1] = 1 
  for(i in 2:nrow(subClN)){
    if(subClN$State[i] == subClN$previous_state[i]){
      subClN$Time[i] = subClN$Time[i-1] +1 
    }
    else{
      subClN$Time[i] = 1
    }
  }
  if(clN == long$ClNr[1]){
    final <- subClN
  }
  else{
    final <- rbind(final,subClN)
  }
}
short$Time <- 1
db <- rbind(short,final) ; db <- db[order(db$ClNr),]


# Previous time: The time spent in the previous state.
db$previous_time <- c(1,db$Time[-length(db$Time)])
db$previous_time <- ifelse(db$DevYear == 1,0,db$previous_time)# because dev year starts at 1

# Transition: A binary variable equals to 1 if the claim has made a transition during the year, 0 otherwise. (If there is a payment, there is a transition.)

db$Trans <- ifelse(db$State == db$previous_state,0,1)

# Transition to closure: A binary variable equals to 1 if the transition of the claim is to a closure state, 0 otherwise. (This variable must be equal to 0 if **Trans** equals 0.)

db$TransClosed <- ifelse(db$Trans == 1 & db$State %in% c('Closed1','Closed0'),1,0)

# Transition to closure with payment: A binary variable equals to 1 if a payment is done during the year of settlement of the claim, 0 otherwise. (This variable must be equal to 0 if **TransClosed** equals 0.)

db$TransClosed1 <- ifelse(db$TransClosed == 1 & db$State == 'Closed1',1,0)

# Transition to RBNP: A binary variable equals to 1 if the transition of the claim is from IBNR to RBNP, 0 otherwise. (This variable must be equal to 0 if **Trans** equals 0 or if the previous state is different from IBNR.)
#state = Nb of payment up to this point except if it's closed or not reported

db$TransRBNP <- ifelse(db$Trans == 1 & db$State == 0,1,0)

# Cumulative payments: For each claim : $C_i = \sum\limits_{k=1}^j P_k$ with $P_k$ the payment done during the $k^{th}$ development year.

long <- db %>% filter(RY != SetYear) %>% filter(RY != MaxObs)
short <- db %>% filter(RY == SetYear | RY == MaxObs)

for(clN in unique(long$ClNr)){
  subClN <- long %>% filter(ClNr == clN)
  subClN$cumP[1] <- subClN$Payment[1]
  for(i in 2:nrow(subClN)){
    subClN$cumP[i] <- subClN$cumP[i-1] + subClN$Payment[i]
  }
  if(clN == long$ClNr[1]){
    final <- subClN
  }
  else{
    final <- rbind(final,subClN)
  }
}

short$cumP <- short$Payment
db <- rbind(short,final) ; db <- db[order(db$ClNr),]


#Link ratios: For each claim the link ration is : $\Lambda_j = \frac{C_{j+1}}{C_j}$. When no payment is due for the year or if it is the first payment, no link ratio should exists. We put this link ratio to 1 for the corresponding lines.

db$previous_cumP <- c(0,db$cumP[-length(db$cumP)])
db$previous_cumP <- ifelse(db$DevYear == 1,0,db$previous_cumP)
db$LinkRatio <- ifelse(db$previous_cumP == 0,1,db$cumP/db$previous_cumP)

# Removing lines after the last payment if the claim is still open

db$previous_NbPayment <- c(0,db$NbPayment[-length(db$NbPayment)])
db$previous_NbPayment <- ifelse(db$DevYear == 1,0,db$previous_NbPayment)

db_entire <- db

temp <- db[db$SetYear == 0,]# set year is 0 if the claim is still open in 2012 i.e at the moment of evaluation

for(clN in unique(temp$ClNr)){
  subClN <- temp %>% filter(ClNr == clN)
  if(subClN[subClN$DevYear == max(subClN$DevYear),'Payment'] == 0){
    temp2 <- subClN[-which(subClN$previous_NbPayment == max(subClN$previous_NbPayment)),]
  }
  else{
    temp2 <- subClN
  }
  if(clN == temp$ClNr[1]){
    final <- temp2
  }
  else{
    final <- rbind(final,temp2)
  }
}

db <- rbind(db[db$SetYear != 0,],final) ; db <- db[order(db$ClNr), ]



