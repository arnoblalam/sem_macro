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
library(xlsx)

source("src/helper.R")

# Get the data

# Exchange rates (forward and spot)
fwd_points <- read_data("data/Forward rates.xlsx")

spot_rates <- read_data("data/Spot Rates.xlsx")

# Libor and Overnight swaps (USD and Euro)
usd_libor <- read_data("data/LIBOR.xlsx", skip = 4)

eur_libor <- read_data("data/EUR LIBOR.xlsx", skip = 4)

euribor <- read_data("data/Euribor 3 month.xlsx", skip = 4, 
                     col_types = c("date", "numeric", "numeric"))

eurodollar <- read_data("data/Eurodollar.xlsx", skip = 4, col_types = c("date", "numeric", "numeric"))

eur_ois <- read_data("data/Euro OIS.xlsx")

usd_ois <- read_data("data/USD OIS.xlsx")

# Ted Spread (requires a different funcion)
ted_spread <- read.zoo(read.xlsx2("data/TEDRATE.xls", 
                                  sheetIndex = 1, 
                                  startRow = 11, 
                                  colClasses = c("Date", "numeric")))

# CDS rates
bofa_cds <- read_data("data/BOFA CDS.xlsx")

cinc_cds <- read_data("data/CINC CDS.xlsx")

jpm_cds <- read_data("data/JMPCC CDS.xlsx")

rabobank_cds <- read_data("data/Rabobank CDS.xlsx")

deutsche_cds <- read_data("data/Deutsche Bank CDS.xlsx")

# Data cleaning

# Remove zero values from TED rate
ted_spread <- ted_spread[ted_spread != 0]

# Divide rates by 100

ted_spread <- ted_spread/100

eur_libor <- eur_libor/100

usd_libor <- usd_libor/100

eur_ois <- eur_ois/100

# usd_ois <- usd_ois/100

# For Euribor we have ask and last, let's just take the last
euribor <- euribor[,1]/100

# Same thing with the Eurodollar
eurodollar <- eurodollar[,1]/100

# Divide forward points by 10000
forward_rates <- spot_rates + fwd_points/10000

swap_implied_rates <- ((forward_rates/spot_rates) * (1 + eur_libor)^0.25)^4 - 1

# CDS averages
eur_cds <- (rabobank_cds + deutsche_cds)/2

us_cds <- (jpm_cds + cinc_cds + bofa_cds)/3

cds_libor <- us_cds - eur_cds
