# name:         sem_macro_main.R
# description:  source file for analysis of counterparty risk in FX swaps
# author:       Arnob L. Alam <aa2288a@american.edu>
#
# This is the main analysis file for the analysis of counterparty risk premia
# during financial crisis in the FX market


library(strucchange)
library(rugarch)

model_1 <- lm(cipv ~ ., data = main_data)
plot(gefp(cipv ~ ., data = window(main_data)))
