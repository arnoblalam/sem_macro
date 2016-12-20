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
