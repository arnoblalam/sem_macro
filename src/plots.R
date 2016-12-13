# set the color scheme for graphs
cols <- c("red", "blue")

# Plot the swap implied interest rates and the actual (Libor) rate
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
       col = cols,
       lty = c(1, 1))

# Plot the swap implied and actual (Libor) rate for a smaller time period
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

# Plot the CDS rates for the Euro area and US banks
plot(merge(eur_cds, us_cds), plot.type="single", col=cols,
     ylab ="Average CDSD spread (basis points)",
     main = "Average CDS spreads for 2 European and 3 US financial institutions
     that contribute to Libor")
legend("topright", legend=c("European CDS Average", "US CDS Average"), col=c("blue", "red"), lty=1)
