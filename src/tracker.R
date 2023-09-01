genbaltrack <- subset(read.csv('/Users/rohanathreya/Projects/genbaltrack.csv'),
                      select = c(-ELAPSED, -RECWGT, -TOTWGT))
dates <- seq(as.Date("2022-01-01"), Sys.Date(), "days")
tracker <- matrix(0, nrow = length(dates), ncol = 3)
colnames(tracker) <- c("DPCT", "RPCT", "MARGIN")

for (i in 1:length(dates)) {
  dailygb <- subset(genbaltrack, END <= dates[i])
  for (j in 1:nrow(dailygb)) {
    dailygb[j,"ELAPSED"] <- as.integer(dates[i] - as.Date(dailygb[j,"END"]))
    dailygb[j,"RECWGT"] <- 100 * ((0.9361) ^ dailygb[j,"ELAPSED"])
    dailygb[j,"TOTWGT"] <- dailygb[j,"RECWGT"] * dailygb[j, "WEIGHT"] * 
                          dailygb[j, "SAMPWEIGHT"] * dailygb[j, "POPWEIGHT"] 
  }
  dailygb21 <- subset(dailygb, ELAPSED <= 21)
  tracker[i,"DPCT"] <- sum(dailygb21$DEMOCRATIC*dailygb21$TOTWGT)/sum(dailygb21$TOTWGT)
  tracker[i,"RPCT"] <- sum(dailygb21$REPUBLICAN*dailygb21$TOTWGT)/sum(dailygb21$TOTWGT)
  tracker[i,"MARGIN"] <- tracker[i,"DPCT"] - tracker[i,"RPCT"]
}

write.csv(tracker, '/Users/rohanathreya/Projects/AthreyaForecast/tracker.csv')

x <- dates
y1 <- tracker[, "DPCT"]
y2 <- tracker[, "RPCT"]

plot(x,y1,type="l",col="blue", ylim = c(30,60))
lines(x,y2,col="red")
