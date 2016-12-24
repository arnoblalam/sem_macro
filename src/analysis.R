# name:         sem_macro_main.R
# description:  source file for analysis of counterparty risk in FX swaps
# author:       Arnob L. Alam <aa2288a@american.edu>
#
# This is the main analysis file for the analysis of counterparty risk premia
# during financial crisis in the FX market

source("src/load_data.R")

library(strucchange)
library(rugarch)

# model_1 <- lm(cipv ~ ., data = main_data)
# e <- resid(model_1)

main_data_c <- main_data[complete.cases(main_data),]

spec <- ugarchspec(
  variance.model = list(
    model = 'eGARCH',
    garchOrder = c(1, 1)),
  mean.model = list(
    armaOrder = c(0, 0),
    include.mean = TRUE,
    external.regressors = as.matrix(main_data_c[,-1])
    ))

fit <- ugarchfit(spec, main_data_c[,1])
