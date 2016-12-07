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

exchange_rate_data <- merge(fwd_points, spot_rates, usd_libor, eur_libor)


exchange_rate_data$forward_rates <- exchange_rate_data$spot_rate + exchange_rate_data$fwd_points/10000

exchange_rate_data$swap_implied_forward_rates <- exchange_rate_data$spot_rate * ((1+exchange_rate_data$usd_libor/100)^.25)/((1+exchange_rate_data$eur_libor/100)^.25)

plot(exchange_rate_data[,c("swap_implied_forward_rates", "forward_rates", 
                           "spot_rates")], plot.type = "single", 
     col=c("red", "blue", "green"))

plot(window((exchange_rate_data$forward_rates - exchange_rate_data$swap_implied_forward_rates)*10000, end="2016-12-31"),
     ylab="Basis Points")
