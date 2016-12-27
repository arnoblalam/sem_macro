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

# Spllit the data into several groups
pre_2007 <- window(main_data_c, end = "2007-08-08")
turmoil <- window(main_data_c, start = "2007-08-09", end = "2008-09-12")
crisis <- window(main_data_c, start = "2008-09-13", end = "2009-02-28")
post_crisis <- window(main_data_c, start = "2009-03-01")


model_1 <- lm(cipv ~ ., data = main_data_c)
e <- resid(model_1)

fit_model <- function(x) {
  spec <- ugarchspec(
    variance.model = list(
      model = 'eGARCH',
      garchOrder = c(1, 1),
      external.regressors = as.matrix(x[,6])),
    mean.model = list(
      armaOrder = c(0, 0),
      include.mean = TRUE,
      external.regressors = as.matrix(x[,-1])
    ))
  ugarchfit(spec, x[,1], solver = "hybrid")
}

fits <- lapply(list(main_data_c, turmoil, crisis, post_crisis), fit_model)

pre_2007 <- window(main_data_c, end = "2007-08-08")
turmoil <- window(main_data_c, start = "2007-08-09", end = "2008-09-12")
crisis <- window(main_data_c, start = "2008-09-13", end = "2009-02-28")
post_crisis <- window(main_data_c, start = "2009-03-01")

ybar <- rbind(fitted(fits[[2]]), fitted(fits[[3]]), fitted(fits[[4]]))
plot(merge(as.zoo(main_data_c$cipv), as.zoo(ybar)), plot.type="single", col=c("red", "blue"))
grid()
