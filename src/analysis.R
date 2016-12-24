# name:         sem_macro_main.R
# description:  source file for analysis of counterparty risk in FX swaps
# author:       Arnob L. Alam <aa2288a@american.edu>
#
# This is the main analysis file for the analysis of counterparty risk premia
# during financial crisis in the FX market

source("src/load_data.R")

library(strucchange)
library(rugarch)

main_data <- merge(cipv = swap_implied_rates - usd_libor,
                   na_ig_cdx,
                   eur_ig_cdx = european_fin_cds,
                   broad_usd = (eurodollar - usd_ois),
                   broad_eur = (euribor - eur_ois),
                   ecb_operations)
main_data$ecb_operations <- na.fill(main_data$ecb_operations, list(0, 0, 0))

main_data_c <- main_data[complete.cases(main_data),]
pre_2010 <- window(main_data_c, end = "2009-12-31")


model_1 <- lm(cipv ~ ., data = main_data_c)
e <- resid(model_1)

spec <- ugarchspec(
  variance.model = list(
    model = 'eGARCH',
    garchOrder = c(1, 1)),
  mean.model = list(
    armaOrder = c(0, 0),
    include.mean = TRUE,
    external.regressors = as.matrix(main_data_c[,-1:-3])
    ))

fit <- ugarchfit(spec, main_data_c[,1])
