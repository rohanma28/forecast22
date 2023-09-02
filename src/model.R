library("Hmisc")
library("SuppDists")

polls <- read.csv('data/raw/polls.csv')
bias <- read.csv('data/raw/bias.csv')
weights <- read.csv('data/raw/weights.csv')

seats <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "FL", "GA",
           "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "MD",
           "MO", "NV", "NH", "NY", "NC", "ND", "OH", "OK","OKS", 
           "OR", "PA", "SC", "SD", "UT", "VT", "WA", "WI")

rownames(weights) <- weights[,1]
polls["POLLSTER"][polls["POLLSTER"] == "Fabrizio Lee & Associates"] <- "Fabrizio, Lee & Associates"
polls[is.na(polls)] <- 0
polls[,"END"] <- as.Date(c(polls[,"END"]), "%m/%d/%y")
midtermdate <- as.Date("2022-11-08")
polls$ELAPSED <- as.integer(midtermdate - polls$END)
polls$RECWGT <- 100 * ((0.9361) ^ polls$ELAPSED)
polls$BIAS <- 0
for (i in 1:nrow(polls)) {
  if (polls[i,"STATE"] == "OKS") {
    value <- bias[polls[i,"POLLSTER"], "OK"]
  }
  else {
    value <- bias[polls[i,"POLLSTER"], polls[i,"STATE"]]
  }
  if (is.na(value)) {
    polls[i, "BIAS"] <- 0
  } 
  else {
    polls[i, "BIAS"] <- value
  }
}
polls$WEIGHT <- 0
for (i in 1:nrow(polls)) {
  value <- weights[polls[i,"POLLSTER"], 2]
  if (is.na(value)) {
    polls[i, "WEIGHT"] <- 1
  } 
  else {
    polls[i, "WEIGHT"] <- value
  }
}
polls$SAMPWEIGHT <- 0
for (i in 1:nrow(polls)) {
  if (is.na(polls[i,"SAMPLE"])) {
    polls[i,"SAMPWEIGHT"] <- 0
  } else {
    polls[i,"SAMPWEIGHT"] <- log(polls[i,"SAMPLE"])/4
  }
}
polls$POPWEIGHT <- 0
for (i in 1:nrow(polls)) {
  if (polls[i,"POPULATION"] == "a") {
    polls[i, "POPWEIGHT"] <- 1
  } else if (polls[i,"POPULATION"] == "v") {
    polls[i, "POPWEIGHT"] <- 1.33
  } else if (polls[i,"POPULATION"] == "rv") {
    polls[i, "POPWEIGHT"] <- 1.67
  } else if (polls[i,"POPULATION"] == "lv") {
    polls[i, "POPWEIGHT"] <- 2
  }
}
polls$TOTWGT <- polls$RECWGT * polls$WEIGHT * polls$SAMPWEIGHT * polls$POPWEIGHT
polls$DEMOCRATIC <- (polls$DEM1PCT + polls$DEM2PCT + polls$DEM3PCT) - (polls$BIAS)/4
polls$DEMOCRATIC <- ifelse (polls$DEMOCRATIC > 100, 100, polls$DEMOCRATIC)
polls$DEMOCRATIC <- ifelse (polls$DEMOCRATIC < 0, 0, polls$DEMOCRATIC)
polls$REPUBLICAN <- (polls$REP1PCT + polls$REP2PCT + polls$REP3PCT) + (polls$BIAS)/4
polls$REPUBLICAN <- ifelse (polls$REPUBLICAN > 100, 100, polls$REPUBLICAN)
polls$REPUBLICAN <- ifelse (polls$REPUBLICAN < 0, 0, polls$REPUBLICAN)

genbal <- read.csv('data/raw/genbal.csv')
genbalbias <- read.csv('data/raw/genbalbias.csv')
gbweights <- read.csv('data/raw/gbweights.csv')

rownames(genbalbias) <- c(genbalbias[,1])
rownames(gbweights) <- gbweights[,1]
genbal[,"END"] <- as.Date(c(genbal[,"END"]), "%m/%d/%y")
genbal$ELAPSED <- as.integer(midtermdate - genbal$END)
genbal$RECWGT <- 100 * ((0.9361) ^ genbal$ELAPSED)
genbal$BIAS <- 0
for (i in 1:nrow(genbal)) {
  value <- genbalbias[genbal[i,"POLLSTER"], 2]
  if (is.na(value)) {
    pollvec <- as.numeric(bias[genbal[i,"POLLSTER"],])
    if (is.na(pollvec[1])) {
      genbal[i, "BIAS"] <- 0
    }
    else {
      genbal[i, "BIAS"] <- mean(pollvec)
    }
  } 
  else {
    genbal[i, "BIAS"] <- value
  }
}
genbal$WEIGHT <- 0
for (i in 1:nrow(genbal)) {
  value <- gbweights[genbal[i,"POLLSTER"], 2]
  if (is.na(value)) {
    genbal[i, "WEIGHT"] <- 1
  } 
  else {
    genbal[i, "WEIGHT"] <- value
  }
}
genbal$SAMPWEIGHT <- 0
for (i in 1:nrow(genbal)) {
  if (is.na(genbal[i,"SAMPLE"])) {
    genbal[i,"SAMPWEIGHT"] <- 0
  } else {
    genbal[i,"SAMPWEIGHT"] <- log(genbal[i,"SAMPLE"])/4
  }
}
genbal$POPWEIGHT <- 0
for (i in 1:nrow(genbal)) {
  if (genbal[i,"POPULATION"] == "a") {
    genbal[i, "POPWEIGHT"] <- 1
  } else if (genbal[i,"POPULATION"] == "v") {
    genbal[i, "POPWEIGHT"] <- 1.33
  } else if (genbal[i,"POPULATION"] == "rv") {
    genbal[i, "POPWEIGHT"] <- 1.67
  } else if (genbal[i,"POPULATION"] == "lv") {
    genbal[i, "POPWEIGHT"] <- 2
  }
}
genbal$TOTWGT <- genbal$RECWGT * genbal$WEIGHT * genbal$SAMPWEIGHT * genbal$POPWEIGHT
genbal$DEMOCRATIC <- genbal$DEMPCT - (genbal$BIAS)/4
genbal$REPUBLICAN <- genbal$REPPCT + (genbal$BIAS)/4
genbalrec <- subset(genbal, ELAPSED <= 21)
DPCTGB <- sum(genbalrec$DEMOCRATIC*genbalrec$TOTWGT)/sum(genbalrec$TOTWGT)
RPCTGB <- sum(genbalrec$REPUBLICAN*genbalrec$TOTWGT)/sum(genbalrec$TOTWGT)
marginGB <- DPCTGB - RPCTGB

write.csv(genbal, 'data/raw/genbaltrack.csv')

correlation <- read.csv('data/raw/correlation.csv')
rownames(correlation) <- correlation[,1]
correlation <- correlation[,-1]
means <- rowMeans(correlation)
stdevs <- apply(correlation,1,sd)
corr <- matrix(0, nrow = 35, ncol = 35)
rownames(corr) <- seats
colnames(corr) <- seats
for (i in 1:nrow(correlation)) {
  for (j in 1:nrow(correlation)) {
    corr[i,j] <- 0.25 * sum((((correlation[i,1:5]-means[i])/stdevs[i])*((correlation[j,1:5]-means[j])/stdevs[j])))
    if (corr[i,j] < 0) {
      corr[i,j] = 0
    }
  }
}

partisanlean <- read.csv('data/raw/partisanlean.csv')
rownames(partisanlean) <- partisanlean[,1]
pvi <- matrix(0, nrow = 35, ncol = 35)
rownames(pvi) <- seats
colnames(pvi) <- seats
for (i in 1:nrow(partisanlean)) {
  for (j in 1:nrow(partisanlean)) {
    pvi[i,j] <- partisanlean[i,2] - partisanlean[j,2]
  }
}

pollavg22 <- matrix(0, nrow = 35, ncol = 8)

rownames(pollavg22) <- seats
colnames(pollavg22) <- c("DPCT", "RPCT", "MARGIN", "DSD", "RSD", "NPOLLS", "NPOLLSREC", "TOTWGT")
for (i in 1:nrow(pollavg22)) {
  incpolls <- subset(polls, STATE == seats[i] )
  increcpolls <- subset(incpolls, TOTWGT > 1)
  if (nrow(increcpolls) > 0) {
    DPCT <- sum(incpolls$DEMOCRATIC*incpolls$TOTWGT)/sum(incpolls$TOTWGT)
    pollavg22[i,1] <- DPCT
    RPCT <- sum(incpolls$REPUBLICAN*incpolls$TOTWGT)/sum(incpolls$TOTWGT)
    pollavg22[i,2] <- RPCT
  } else {
    DPCT <- DPCTGB + 0.5 * partisanlean[i,"PVI"]
    pollavg22[i,1] <- DPCT 
    RPCT <- RPCTGB -  0.5 * partisanlean[i,"PVI"] 
    pollavg22[i,2] <- RPCT
  }
  pollavg22[i,3] <- DPCT - RPCT
  pollavg22[i,6] <- nrow(incpolls)
  pollavg22[i,7] <- nrow(increcpolls)
  if (nrow(increcpolls) > 1) {
    pollavg22[i,4] <- sqrt(wtd.var((incpolls)[,"DEMOCRATIC"],
                                   (incpolls)[,"TOTWGT"]))
    pollavg22[i,5] <- sqrt(wtd.var((incpolls)[,"REPUBLICAN"],
                                   (incpolls)[,"TOTWGT"])) 
  } else {
    pollavg22[i,4] <- sqrt(wtd.var((genbal)[,"DEMOCRATIC"], 
                                   (genbal)[,"TOTWGT"]))
    pollavg22[i,5] <- sqrt(wtd.var((genbal)[,"REPUBLICAN"], 
                                   (genbal)[,"TOTWGT"]))
  }
  pollavg22[i,8] <- sum(increcpolls$TOTWGT)
}

final <- matrix(0, nrow = length(seats), ncol = 4)
rownames(final) <- seats
colnames(final) <- c("DPCT", "RPCT", "DSD", "RSD")
for (i in 1:length(seats)) {
  final[i,1] <- pollavg22[i,"DPCT"]
  final[i,2] <- pollavg22[i,"RPCT"]
  final[i,3] <- pollavg22[i,"DSD"]
  final[i,4] <- pollavg22[i,"RSD"]
}

n <- 100000
d_matrix <- matrix(0, nrow = length(seats), ncol = n)
r_matrix <- matrix(0, nrow = length(seats), ncol = n)
results_matrix <- matrix(0, nrow = length(seats), ncol = n)
Dist <- list(gamma = 0, delta = 0.5, xi = 0.01, lambda = 1, type = "SN")
for(i in 1:length(seats)){
  dist_multiplier <- rJohnson(n, Dist)
  d_matrix[i,] <- final[i,1] + dist_multiplier*final[i,3]
  r_matrix[i,] <- final[i,2] - dist_multiplier*final[i,4]
  results_matrix <- d_matrix - r_matrix
}

dist_multiplier <- rJohnson(n, Dist)
genbalmargins <- genbal[,"DEMOCRATIC"] - genbal[,"REPUBLICAN"]
genbalsd <- sqrt(wtd.var(genbalmargins, genbal[,"TOTWGT"]))
gbspread <- marginGB + dist_multiplier*genbalsd

corradj <- matrix(0, nrow = 35, ncol = 35)
rownames(corradj) <- seats
colnames(corradj) <- seats
for (i in 1:nrow(pvi)) {
  for (j in 1:nrow(pvi)) {
    corradj[i,j] <- (pollavg22[j,"MARGIN"] + pvi[i,j]) * corr[i,j]
    if (is.na(pollavg22[j,"MARGIN"])) {
      corradj[i,j] <- 0
    }
  }
}

corrfund <- apply(corradj, 1, sum) / apply(corr,1,sum)

overperf <- read.csv('data/raw/overperf.csv')

rownames(overperf) <- overperf$STATE

for(i in 1:length(seats)){
  if (seats[i] != "AK") {
    if (is.na(overperf[seats[i],"OVERPERFORMANCE"])) {
      overperformance <- 0
    }
    else {
      overperformance <- overperf[seats[i],"OVERPERFORMANCE"]
    }
    statesimilarity <- corrfund[seats[i]]
    fundprop <- 0.8 * exp(-(1/2000) * pollavg22[i,8]) + 0.2
    results_matrix[i,] <- (((gbspread + partisanlean[i,"PVI"] + overperformance + 0.5*statesimilarity)/1.5) * fundprop) + 
      (results_matrix[i,] * (1 - fundprop)) 
  }
}
margins <- rowMeans(results_matrix)

final <- cbind(final, "DPROB" = 0, "RPROB" = 0, "RATING" = 0)
d_wins <- ifelse(results_matrix > 0, 1, 0)
d_prob <- apply(d_wins, 1, sum)/n
final[,"DPROB"] <- d_prob
r_wins <- ifelse(results_matrix < 0, 1, 0)
r_prob <- apply(r_wins, 1, sum)/n
final[,"RPROB"] <- r_prob
for (i in 1:nrow(final))
  if (d_prob[i] >= 0.95) {
    final[i,"RATING"] <- "Safe D"
  } else if (d_prob[i] >= 0.75 & d_prob[i] < 0.95) {
    final[i,"RATING"] <- "Likely D"
  } else if (d_prob[i] >= 0.6 & d_prob[i] < 0.75) {
    final[i,"RATING"] <- "Lean D"
  } else if (d_prob[i] > 0.4 & d_prob[i] < 0.6) {
    final[i,"RATING"] <- "Tossup"
  } else if (d_prob[i] > 0.25 & d_prob[i] <= 0.4) {
    final[i,"RATING"] <- "Lean R"
  } else if (d_prob[i] > 0.05 & d_prob[i] <= 0.25) {
    final[i,"RATING"] <- "Likely R"
  } else if (d_prob[i] <= 0.05) {
    final[i,"RATING"] <- "Safe R"
  }

probs <- subset(final, select = c("DPCT", "RPCT", "DPROB", "RPROB", "RATING"))
probs <- cbind(probs, "MARGIN" = margins)
other <- (100 - (as.numeric(probs[,"DPCT"]) + as.numeric(probs[,"RPCT"])))/2
probs[,"DPCT"] <- 50 + 0.5*(as.numeric(probs[,"MARGIN"])) - 0.5*other
probs[,"RPCT"] <- 50 - 0.5 *(as.numeric(probs[,"MARGIN"])) - 0.5*other

dsenseats <- apply(d_wins, 2, sum) + 36
rsenseats <- apply(r_wins, 2, sum) + 29
results <- data.frame(dsenseats, rsenseats)
results$dwin <- ifelse(results$dsenseats >= 50, 1, 0)
results$rwin <- ifelse(results$dsenseats < 50, 1, 0)
wholesenprob <- apply(results, 2, sum)/n

probs
wholesenprob

probs <- cbind(probs, "STATE" = seats)

probs <- cbind(probs, "COLOR" = "")
for (i in 1:nrow(probs)) {
  if (probs[i,"RATING"] == "Safe R") {
    probs[i,"COLOR"] <- 1
  } else if (probs[i,"RATING"] == "Likely R") {
    probs[i,"COLOR"] <- 2
  } else if (probs[i,"RATING"] == "Lean R") {
    probs[i,"COLOR"] <- 3
  } else if (probs[i,"RATING"] == "Tossup") {
    probs[i,"COLOR"] <- 4
  } else if (probs[i,"RATING"] == "Lean D") {
    probs[i,"COLOR"] <- 5
  } else if (probs[i,"RATING"] == "Likely D") {
    probs[i,"COLOR"] <- 6
  } else if (probs[i,"RATING"] == "Safe D") {
    probs[i,"COLOR"] <- 7
  } 
}

for (i in 1:nrow(probs)) {
  probs[i,"DPROB"] <- round(as.numeric(probs[i,"DPROB"]), digits = 4)
  probs[i,"RPROB"] <- round(as.numeric(probs[i,"RPROB"]), digits = 4)
  probs[i,"MARGIN"] <- round(as.numeric(probs[i,"MARGIN"]), digits = 2)
}

probs <- cbind(probs, "MARGINLABEL" = "")
for (i in 1:nrow(probs)) {
  if (probs[i,"MARGIN"] < 1) {
    repmargin <- substring(probs[i,"MARGIN"],2)
    probs[i,"MARGINLABEL"] <- paste("R+", repmargin, sep = "")
  } else if (probs[i,"MARGIN"] >= 1) {
    demmargin <- probs[i,"MARGIN"]
    probs[i,"MARGINLABEL"] <- paste("D+", demmargin, sep = "")
  } 
}

hist <- data.frame(table(results$dsenseats))
hist$Prop <- hist$Freq/1000
hist <- hist[,-2]

write.csv(hist, 'data/processed/hist.csv')
write.csv(probs, 'data/processed/probs.csv')
write.csv(wholesenprob, 'data/processed/wholesenprob.csv')
