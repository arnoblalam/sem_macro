# name:         sem_macro_main.R
# description:  source file for analysis of counterparty risk in FX swaps
# author:       Arnob L. Alam <aa2288a@american.edu>
#
# This is the main file for the analysis of counterparty risk premia
# during financial crisis in the FX market

# Load some requireed libraries
library("foreign", lib.loc="~/sem_macro/packrat/lib-R")
library(readxl)
library(dplyr)
library(ggplot2)
library(reshape2)
library(zoo)
library(forecast)
library(urca)

source("src/helper.R")

fwd_points <- read.zoo(as.data.frame(remove_last_row(read_excel("data/Forward rates.xlsx", 
                        col_types = c("date", "numeric"), 
                        skip = 5))))

spot_rates <- read.zoo(as.data.frame(remove_last_row(read_excel("data/Spot Rates.xlsx", 
                        col_types = c("date", "numeric"), 
                        skip = 5))))

usd_libor <- read.zoo(as.data.frame(remove_last_row(read_excel("data/LIBOR.xlsx",
                        col_types = c("date", "numeric"),
                        skip = 4))))

eur_libor <- read.zoo(as.data.frame(remove_last_row(read_excel("data/EUR LIBOR.xlsx",
                        col_types = c("date", "numeric"),
                        skip = 4))))

eur_libor <- eur_libor/100

usd_libor <- usd_libor/100

forward_rates <- spot_rates + fwd_points/10000

swap_implied_rates <- ((forward_rates/spot_rates) * (1 + eur_libor)^0.25)^4 - 1

plot(merge(swap_implied_rates*100, usd_libor*100), 
     plot.type = "single",
     col = c("red", "blue"),
     main = "3 month FX swap implied rates and
     3 month USD Libor rates",
     sub = "Bloomberg News, author's own calculations",
     ylab = "Percentage")
grid()
legend("topright", 
       legend = c("FX swap implied rate", "USD Libor 3M"), 
       col = c("red", "blue"),
       lty = c(1, 1))

plot(window(merge(swap_implied_rates*100, usd_libor*100), 
            start = "2006-09-01", 
            end = "2008-09-01"), 
     plot.type = "single",
     col = c("red", "blue"),
     main = "3 month FX swap implied rates and
     3 month USD Libor rates (Sep '06 - Sep '08)",
     sub = "Bloomberg News, author's own calculations",
     ylab = "Percentage")
grid()
legend("topright", 
       legend = c("FX swap implied rate", "USD Libor 3M"), 
       col = c("red", "blue"),
       lty = c(1, 1))
bofa_cds <- read_excel("~/sem_macro/data/BOFA CDS.xlsx", 
                       col_types = c("date", "numeric"), 
                       skip = 5)

# plot(window(swap_implied_rates*100, end="2008-01-01"))
# plot(window(forward_rates/spot_rates, end="2008-01-01"))
# plot(window(fwd_points, end="2008-01-01")) 
# 
# plot(window(usd_libor*100, start="2007-07-01", end="2008-02-01"), ylim=c(2.8, 5.8))
# plot(window(eur_libor*100, start="2007-07-01", end="2008-02-01"), ylim=c(3, 5))
# 
# plot(window(swap_implied_rates, start="2007-01-01", end="2008-01-01"),
#      plot.type = "single",
#      col = c("red", "blue"))
# 
# 
# 
