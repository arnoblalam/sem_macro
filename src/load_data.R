# name:         sem_macro_main.R
# description:  source file for analysis of counterparty risk in FX swaps
# author:       Arnob L. Alam <aa2288a@american.edu>
#
# This is the file for loading data  for the analysis of counterparty risk premia
# during financial crisis in the FX market

# Load some requireed libraries
library(foreign)
library(readxl)
library(zoo)
library(xlsx)
library(strucchange)
library(rugarch)

source("src/helper.R")

# Get the data

# European Dollar auctions
ecb_operations <- read.csv("data/ECB opartions.csv", stringsAsFactors = FALSE)
ecb_operations <- ecb_operations[ecb_operations$t_operation_currency == "USD",]
ecb_operations <- ecb_operations[order(ecb_operations$t_duration, 
                                       decreasing = TRUE),]
ecb_operations <- ecb_operations[!duplicated(as.POSIXct(ecb_operations$t_allot_dt)),]
ecb_operations <- zoo(rep(1, length.out=length(ecb_operations)), as.POSIXct(ecb_operations$t_allot_dt))

# Exchange rates (forward and spot)
fwd_points <- read_data("data/Forward rates.xlsx")

spot_rates <- read_data("data/Spot Rates.xlsx")

# Libor and Overnight swaps (USD and Euro)
usd_libor <- read_data("data/LIBOR.xlsx", skip = 4)

eur_libor <- read_data("data/EUR LIBOR.xlsx", skip = 4)

euribor <- read_data(
  "data/Euribor 3 month.xlsx",
  skip = 4,
  col_types = c("date", "numeric", "numeric")
)

eurodollar <- read_data(
  "data/Eurodollar.xlsx",
  skip = 4,
  col_types = c("date", "numeric", "numeric")
)

eur_ois <- read_data("data/Euro OIS.xlsx")

usd_ois <- read_data("data/USD OIS.xlsx")

# Ted Spread (requires a different funcion)
ted_spread <- read.zoo(read.xlsx2(
  "data/TEDRATE.xls",
  sheetIndex = 1,
  startRow = 11,
  colClasses = c("Date", "numeric")
))

# CDS rates
# Major US financials (Bank of America, Citibank and JP Morgan)
bofa_cds <- read_data("data/BOFA CDS.xlsx")

cinc_cds <- read_data("data/CINC CDS.xlsx")

jpm_cds <- read_data("data/JMPCC CDS.xlsx")

# Major European banks
rabobank_cds <- read_data("data/Rabobank CDS.xlsx")

deutsche_cds <- read_data("data/Deutsche Bank CDS.xlsx")

#Investment grade American financials
agmc_cds <- read_data("data/AGMC CDS.xlsx")

aig_cds <- read_data("data/AIG CDS.xlsx")

allstate_cds <- read_data("data/Allstate CDS.xlsx")

amex_cds <- read_data("data/AMEX CDS.xlsx")

berkshire_cds <- read_data("data/Berkshire CDS.xlsx")

cap_one_cds <- read_data("data/Capital One CDS.xlsx")

chubb_cds <- read_data("data/Chubb CDS.xlsx")

erp_cds <- read_data("data/ERP Financial CDS.xlsx")

hartford_cds <- read_data("data/Hartford CDS.xlsx")

# host_cds <- read_data("data/Host Hotels CDS.xlsx")

international_lease_cds <-
  read_data("data/International Lease Financing Corp CDS.xlsx")

lincoln_cds <- read_data("data/Lincoln CDS.xlsx")

lowes_cds <- read_data("data/Lowes CDS.xlsx")

marsh_cds <- read_data("data/Marsh CDS.xlsx")

metlife_cds <- read_data("data/METLIFE CDS.xlsx")

nr_cds <- read_data("data/National Rural CDS.xlsx")

prudential_cds <- read_data("data/Prudential CDS.xlsx")

simon_cds <- read_data("data/Simon CDS.xlsx")

weyhauser_cds <- read_data("data/Weyerhaeuser CDS.xlsx")

# European Financial CDS index
# european_fin_cds <- read_data("data/Euro 5Y.xlsx")
european_fin_cds <- read.zoo(as.data.frame(read_excel(
  "data/european_five_year_cds.xlsx"
)))

# Data cleaning

# Remove zero values from TED rate
ted_spread <- ted_spread[ted_spread != 0]

# Divide rates by 100

ted_spread <- ted_spread / 100

eur_libor <- eur_libor / 100

usd_libor <- usd_libor / 100

eur_ois <- eur_ois / 100

usd_ois <- usd_ois / 100

# For Euribor we have ask and last, let's just take the last
euribor <- euribor[, 1] / 100

# Same thing with the Eurodollar
eurodollar <- eurodollar[, 1] / 100

# Divide things quoted in basis points by 10000
agmc_cds <- agmc_cds / 10000
aig_cds <- aig_cds / 10000
allstate_cds <- allstate_cds / 10000
amex_cds <- amex_cds / 10000
berkshire_cds <- berkshire_cds / 10000
cap_one_cds <- cap_one_cds / 10000
chubb_cds <- chubb_cds / 10000
erp_cds <- erp_cds / 10000
hartford_cds <- hartford_cds / 10000
international_lease_cds <- international_lease_cds / 10000
lincoln_cds <- lincoln_cds / 10000
lowes_cds <- lowes_cds / 10000
marsh_cds <- marsh_cds / 10000
metlife_cds <- metlife_cds / 10000
nr_cds <- nr_cds / 10000
prudential_cds <- prudential_cds / 10000
simon_cds <- simon_cds / 10000
weyhauser_cds <- weyhauser_cds / 10000

bofa_cds <- bofa_cds / 10000
cinc_cds <- cinc_cds / 10000
jpm_cds <- jpm_cds / 10000

deutsche_cds <- deutsche_cds / 10000
rabobank_cds <- rabobank_cds / 10000

european_fin_cds <- european_fin_cds / 10000

fwd_points <- fwd_points / 10000

# Calculations

# Forward rate (divide basis points by 10000)
forward_rates <- spot_rates + fwd_points

# Swap Implied Rates
swap_implied_rates <-
  ((forward_rates / spot_rates) * (1 + eur_libor) ^ 0.25) ^ 4 - 1

# CDS averages (sample of Libor panel banks)
eur_cds <- (rabobank_cds + deutsche_cds) / 2

us_cds <- (jpm_cds + cinc_cds + bofa_cds) / 3

# Difference between European and American Libor panel banks
cds_libor <- us_cds - eur_cds

# CDS averages (North American Invenstment Grade financials)
na_ig_cds <-
  merge(
    agmc_cds,
    aig_cds,
    allstate_cds,
    amex_cds,
    berkshire_cds,
    cap_one_cds,
    chubb_cds,
    erp_cds,
    hartford_cds,
    international_lease_cds,
    lincoln_cds,
    lowes_cds,
    marsh_cds,
    metlife_cds,
    nr_cds,
    prudential_cds,
    simon_cds
  )

na_ig_cdx <- zoo(rowMeans(na_ig_cds, na.rm = TRUE), time(na_ig_cds))

main_data <- merge(cipv = swap_implied_rates - usd_libor,
                   na_ig_cdx,
                   eur_ig_cdx = european_fin_cds,
                   broad_euro = euribor - eur_ois,
                   broad_us = eurodollar - usd_ois)
# main_data <- merge(main_data, ecb_operations, fill = 0)
