require(hirem)
require(tidyverse)
library(tidyr)


reserving_data <- copy(data)
# Creating the interaction effect
reserving_data$monthDev12 <- as.character(reserving_data$rep.month)
reserving_data$monthDev12[reserving_data$dev.year > 3] <- 'dev.year > 3'
reserving_data$devYearMonth <- factor(paste(reserving_data$dev.year, reserving_data$monthDev12, sep = '-'))
# Observed and prediction data set
observed_data   <- reserving_data %>% filter(calendar.year <= 12)
prediction_data <- reserving_data %>% filter(calendar.year > 13 & settlement.year >13)


# Calculating the weights
reported_claims <- observed_data %>%
  dplyr::filter(dev.year == 1) %>%
  group_by(rep.year) %>% 
  dplyr::summarise(count = n()) %>%
  pull(count)

denominator <- tail(rev(cumsum(reported_claims)), -1)
numerator <- head(cumsum(rev(reported_claims)), -1)
weight <- c(10^(-6), numerator / denominator)

names(weight) <- paste0('dev.year',1:12)
weight

# Hierarchical GLM

# Model specificiations hierarchical GLM
model_glm  <- hirem(reserving_data) %>%
  split_data(observed = function(df) df %>% filter(calendar.year <= 12, open == 1)) %>%
  layer_glm(name = 'settlement', 'family' = binomial(link = cloglog)) %>%
  layer_glm(name = 'payment', 'family' = binomial(link = logit)) %>%
  layer_glm(name = 'size', 'family' = Gamma(link = 'log'), 
            filter = function(x){x$payment == 1 & x$size >0})

# Formulae hierarchical GLM layers - model calibration
formula_settle_glm <- "settlement ~  dev.year.fact"
formula_pay_glm    <- "payment ~ settlement  + devYearMonth"
formula_size_glm   <- "size ~ devYearMonth + settlement"

# to check the length of weight: dat_mod <- (reserving_data %>% filter(calendar.year <= 12, open == 1))
# table(dat_mod$dev.year)
# Fitting the hierarchical GLM
model_glm <- fit(model_glm,
                 weights = weight,
                 weight.var = 'dev.year',
                 balance.var = 'dev.year',
                 settlement = formula_settle_glm,
                 payment = formula_pay_glm,
                 size = formula_size_glm)

# Hierarchical GBM
# Results of hierarchical model calibration
gbm_param_settle <- list('n.trees' = 225, 'interaction.depth' = 1, 'shrinkage' = 0.05)
gbm_param_pay    <- list('n.trees' = 125, 'interaction.depth' = 3, 'shrinkage' = 0.05)
gbm_param_size   <- list('n.trees' = 700, 'interaction.depth' = 1, 'shrinkage' = 0.05)

# Model specifications
model_gbm <- hirem(reserving_data) %>%
  split_data(observed = function(df) df %>% filter(calendar.year <= 12, open == 1)) %>%
  layer_gbm('settlement', distribution = 'bernoulli', bag.fraction = 0.75, n.minobsinnode = 100,
            n.trees = gbm_param_settle$n.trees, interaction.depth = gbm_param_settle$interaction.depth,
            shrinkage = gbm_param_settle$shrinkage, select_trees = 'last') %>%
  layer_gbm('payment', distribution = 'bernoulli', bag.fraction = 0.75, n.minobsinnode = 100,            
            n.trees = gbm_param_pay$n.trees, interaction.depth = gbm_param_pay$interaction.depth,
            shrinkage = gbm_param_pay$shrinkage, select_trees = 'last') %>%
  layer_gbm('size', distribution = 'gamma', bag.fraction = 0.75, n.minobsinnode = 100,
            n.trees = gbm_param_size$n.trees, interaction.depth = gbm_param_size$interaction.depth,
            shrinkage = gbm_param_size$shrinkage, select_trees = 'last',
            filter = function(data){data$payment == 1 & data$size>0})

# Covariates
covariates_gbm <- c( 'dev.year.fact', 'rep.year.fact','rep.month', 'rep.delay', 'calendar.year', 
                     "occ.year.fact", "occ.month" )

# Fitting the hierarchical GBM
model_gbm <- fit(model_gbm,
                 weights = weight,
                 weight.var = 'dev.year',
                 balance.var = 'dev.year',
                 settlement = paste0('settlement ~ 1 + ', paste0(covariates_gbm, collapse = ' + ')),
                 payment = paste0('payment ~ 1 + ', paste0(c(covariates_gbm, 'settlement'), collapse = ' + ')),
                 size = paste0('size ~ 1 + ', paste0(c(covariates_gbm,'settlement'), collapse = ' + ')))

# Predicting the future development of claims in the hierarchical reserving models
# Update function
update <- function(data) {
  data$dev.year <- data$dev.year + 1
  data$dev.year.fact <- factor(data$dev.year, levels = 1:12)
  
  data$calendar.year <- data$calendar.year + 1
  
  data$monthDev12[data$dev.year > 3] <- 'dev.year > 3'
  data$devYearMonth <- factor(paste(data$dev.year, data$monthDev12, sep = '-'))
  
  data
}

model_glm <- register_updater(model_glm, update)
model_gbm <- register_updater(model_gbm, update)


simul_glm <- simulate(model_glm,
                      nsim = 100,
                      filter = function(data){dplyr::filter(data, dev.year <= 12, settlement == 0)},# train on the open claims that happened before dev.year =9
                      data = model_glm$data_observed %>% dplyr::filter(calendar.year == 12),
                      balance.correction = TRUE)

simul_gbm <- simulate(model_gbm,
                      nsim = 100,
                      filter = function(data){dplyr::filter(data, dev.year <= 12, settlement == 0)},
                      data = model_gbm$data_observed %>% dplyr::filter(calendar.year == 12),
                      balance.correction = TRUE)

# Chain-ladder model

# Incremental run-off triangles
triangle_open    <- construct_triangle(data = observed_data %>% filter(open == 1), group.var1 = 'rep.year', 
                                       group.var2 = 'dev.year', value = 'open', cumulative = FALSE)
triangle_payment <- construct_triangle(data = observed_data, group.var1 = 'rep.year',
                                       group.var2 = 'dev.year', value = 'payment', cumulative = FALSE)
triangle_size    <- construct_triangle(data = observed_data, group.var1 = 'rep.year',
                                       group.var2 = 'dev.year', value = 'size', cumulative = FALSE)

# Number of open claims in the year following the evaluation date
settle.evalyear <- observed_data %>% 
  filter(open == 1, calendar.year == 12)%>%
  group_by(rep.year, dev.year) %>%
  summarise(settlement = sum(settlement))

# The number of open claims in the year after the evaluation date
#triangle_open[row(triangle_open) + col(triangle_open) == 12] <- 
# (triangle_open[row(triangle_open) + col(triangle_open) == 10] - rev(settle.evalyear$settlement))[1:8]

# Chain ladder predictions
cl_open <- chainLadder_open(triangle_open)
cl_pay  <- chainLadder(triangle_payment, is_cumulatif = FALSE)
cl_size <- chainLadder(triangle_size, is_cumulatif = FALSE)

# Evaluating the predictive performance of the chain-ladder model and the hierarchical GLM and GBM

nsim <- 100


#we compare the prediction for the total number of payment sizes in the prediction set.

# Predictions
obs_size_total <- prediction_data %>% filter(calendar.year != 12) %>% summarise(Total = sum(size)) %>% pull(Total)
cl_size_total  <- sum(cl_size)
glm_size_total <- simul_glm %>% filter(calendar.year != 12) %>% summarise(Total = sum(size)/nsim) %>% pull(Total)
gbm_size_total <- simul_gbm %>% filter(calendar.year != 12) %>% summarise(Total = sum(size)/nsim) %>% pull(Total)

# Print Results
c('Actual' = obs_size_total, 'Chain-Ladder' = cl_size_total, 'Hierarchical GLM' = glm_size_total, 'Hierarchical GBM' = gbm_size_total)

simul_glm_indiv <- simul_glm%>% filter(calendar.year != 12)%>% group_by(polNumb,simulation)%>% summarise(Total = sum(size))%>% group_by(polNumb) %>%  summarise(Total = sum(Total)/nsim)

simul_gbm_indiv <- simul_gbm%>% filter(calendar.year != 12)%>% group_by(polNumb,simulation)%>% summarise(Total = sum(size))%>% group_by(polNumb) %>%  summarise(Total = sum(Total)/nsim)

true_reserves_crevecoeur <- prediction_data%>% filter(settlement.year > 12| is.na(settlement.year))%>% group_by(polNumb)  %>% summarise(Total = sum(size))




simul_gbm_indiv_BE <- simul_gbm%>% filter(calendar.year != 12)%>% group_by(polNumb,simulation)%>% summarise(Total = sum(size))
simul_gbm_indiv_BE <- (reshape2::dcast(simul_gbm_indiv_BE , ... ~ simulation, value.var = "Total"))

simul_glm_indiv_BE <- simul_glm%>% filter(calendar.year != 12)%>% group_by(polNumb,simulation)%>% summarise(Total = sum(size))
simul_glm_indiv_BE <- (reshape2::dcast(simul_glm_indiv_BE , ... ~ simulation, value.var = "Total"))



