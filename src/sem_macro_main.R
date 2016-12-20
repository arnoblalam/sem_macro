# name:         sem_macro_main.R
# description:  source file for analysis of counterparty risk in FX swaps
# author:       Arnob L. Alam <aa2288a@american.edu>
#
# This is the main file for the analysis of counterparty risk premia
# during financial crisis in the FX market

# Load some requireed libraries
library("foreign", lib.loc = "~/sem_macro/packrat/lib-R")
library(readxl)
library(dplyr)
library(ggplot2)
library(reshape2)
library(zoo)
library(forecast)
library(urca)
library(xlsx)
library(xtable)
library(strucchange)
library(SBRect)
library(PerformanceAnalytics)
library(rugarch)

source("src/helper.R")

# Get the data

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
  "~/sem_macro/data/european_five_year_cds.xlsx"
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
  cbind(
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

auction_dates <- as.POSIXct(c("2008-10-15", "2008-10-14", "2008-10-10",
                              "2008-10-09", "2008-10-07", "2008-10-06",
                              "2008-10-03", "2008-10-02", "2008-10-01",
                              "2008-09-30", "2008-09-29", "2008-09-26",
                              "2008-09-25", "2008-09-24", "2008-09-23",
                              "2008-09-22", "2008-09-19", "2008-09-18",
                              "2007-12-17", "2008-01-14", "2008-03-25",
                              "2008-04-22", "2008-05-06", "2008-05-20",
                              "2008-06-03", "2008-06-17", "2007-08-01",
                              "2008-07-15", "2008-07-29", "2008-08-13",
                              "2008-08-26", "2008-09-10", "2008-09-23"),
                            tz = "UTC")
dollar_auction <- zoo(0, time(spot_rates))
indices <- which(index(dollar_auction) %in% auction_dates)
dollar_auction[indices] <- 1

main_data <- merge(y = swap_implied_rates,
                   dollar_auction = dollar_auction,
                   cds_europe = european_fin_cds,
                   cds_america = na_ig_cdx,
                   broad_dollar_ois = eurodollar - usd_ois,
                   borad_euro_ois = euribor - eur_ois,
                   lois_euro = eur_libor - eur_ois,
                   lois_usd = usd_libor - usd_ois)

# Plot

numCols <- ncol(main_data) - 2
colors = rainbow(numCols)
lintypes = 1:numCols

plot(
  main_data[, c(-1,-2)],
  screens = c(1, 2, 2, 1, 1, 2),
  col = colors,
  lty = lintypes,
  main = "Key Explanatory Variables",
  sub = "Source: Bloomberg, author's own calculations",
  ylab = c("European variables", "American variables"),
  lwd = 2
)

legend("topright",
       c("European CDS Index", "American CDS Index", "Broad - OIS (dollar)",
         "Broad - OIS (euro)", "Libor - OIS (euro)", "Libor - OIS (dollar)"),
       lty = lintypes,
       col = colors,
       lwd =2 )

model_1 <- lm(y ~ ., data = window(main_data,
                                   start = "2007-08-01",
                                   end = "2008-09-12"))

model_2 <- lm(y ~ ., data = window(main_data,
                                   start = "2008-09-15",
                                   end = "2009-01-30"))

model_3 <- lm(y ~ ., data = window(main_data,
                                   start = "2009-02-01",
                                   end = "2012-09-30"))

model_4 <- lm(y ~ ., data = window(main_data,
                                   start = "2008-10-01"))

summary(model_1)
summary(model_2)
summary(model_3)
summary(model_4)

model_5 <- lm(y ~ ., data = as.data.frame(main_data))

ocus1 <- efp(model_1, data = as.data.frame(main_data), type="OLS-MOSUM")
ocus2 <- efp(model_2, data = as.data.frame(main_data), type="OLS-MOSUM")
ocus3 <- efp(model_3, data = as.data.frame(main_data), type="OLS-MOSUM")
ocus4 <- efp(model_4, data = as.data.frame(main_data), type="OLS-MOSUM")

plot(ocus1)
plot(ocus2)
plot(ocus3)
plot(ocus4)

# eGARCH
e <- resid(model_5)

spec <- ugarchspec(variance.model = list(
  model  = "eGARCH"))
fit <- ugarchfit(spec = spec, data = e)
show(fit)
