polls <- read.csv('/Users/rohanathreya/Projects/raw-polls.csv')
subpolls <- subset(polls, year >= 2016 & type_simple != 'Pres-P' & 
                     type_simple != 'House-G' & location != 'US')

correlation <- read.csv('/Users/rohanathreya/Projects/Correlation.csv')

state <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI",
           "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI",
           "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC",
           "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT",
           "VT", "VA", "WA", "WV", "WI", "WY")

corr <- matrix(0, nrow = 50, ncol = 50)
rownames(corr) <- state
colnames(corr) <- state

for (i in 1:50) {
  for (j in 1:50) {
    if (i != j) {
      corrvec <- c(
        1-(abs(correlation[i,2] - correlation[j,2])/correlation[51,2]),
        1-(abs(correlation[i,3] - correlation[j,3])/correlation[51,3]),
        1-(abs(correlation[i,4] - correlation[j,4])/correlation[51,4]),
        1-(abs(correlation[i,5] - correlation[j,5])/correlation[51,5]),
        1-(abs(correlation[i,6] - correlation[j,6])/correlation[51,6]))
      corr[i,j] <- mean(corrvec)
    }
    else {
      NA
    }
    }
  }

pollster <- c(unique(subpolls[,"pollster"]))

statepoll <- matrix(0, nrow = length(unique(subpolls[,"pollster"])), ncol = 50)
rownames(statepoll) <- pollster
colnames(statepoll) <- state

for (i in 1:length(pollster)) {
  for (j in 1:length(state)) {
    name <- pollster[i]
    area <- state[j]
    pollbystate <- subset(subpolls, pollster == name & 
                            location == area)
    statepoll[i,j] <- mean(c(pollbystate[,"bias"]))
  }
}

others <- matrix(0, nrow = length(unique(subpolls[,"pollster"])), ncol = 50)
rownames(others) <- pollster
colnames(others) <- state

for (i in 1:length(pollster)) {
  for (j in 1:length(state)) {
    psn <- matrix(0, nrow = 50, ncol = 3, byrow = FALSE)
    psn[,1] <- c(statepoll[i,])
    psn[,2] <- c(corr[,j])
    subpsn <- na.omit(psn)
    subpsn[,3] <- subpsn[,1] * subpsn[,2]
    others[i,j] <- sum(subpsn[,3]) / sum(subpsn[,2])
  }
}

bias <- matrix(0, nrow = length(unique(subpolls[,"pollster"])), ncol = 50)
rownames(bias) <- pollster
colnames(bias) <- state

for (i in 1:length(pollster)) {
  for (j in 1:length(state)) {
    if (!is.na(statepoll[i,j]) & !is.na(others[i,j])) {
      bias[i,j] <- (statepoll[i,j] + others[i,j])/2
    }
    else if (is.na(statepoll[i,j]) & !is.na(others[i,j])) {
      bias[i,j] <- others[i,j]
    }
    else if (!is.na(statepoll[i,j]) & is.na(others[i,j])) {
      bias[i,j] <- statepoll[i,j]
    }
    else {
      NA
    }
  }
}

weights <- matrix(0, nrow = length(unique(subpolls[,"pollster"])), ncol = 1)
rownames(weights) <- pollster
colnames(weights) <- c("Weight")

for (i in 1:length(pollster)) {
    company <- pollster[i]
    psd <- subset(subpolls, pollster == company & !is.na(bias))
    sd <- sd(psd[,"bias"])
    if (is.na(sd)) {
      weights[i,1] <- 1
    }
    else if (sd < 3) {
      weights[i,1] <- 2
    }
    else if (sd >= 3 & sd < 4) {
      weights[i,1] <- 1.67
    }
    else if (sd >= 4 & sd < 5) {
      weights[i,1] <- 1.33
    }
    else {
      weights[i,1] <- 1
    }
}

write.csv(weights,'/Users/rohanathreya/Projects/weights.csv')

genbalpolls <- subset(polls, year >= 2016 & location == 'US')

genbalpollsters <- c(unique(genbalpolls[,"pollster"]))

uspoll <- matrix(0, nrow = length(unique(genbalpolls[,"pollster"])), ncol = 1)
rownames(uspoll) <- genbalpollsters

for (i in 1:length(genbalpollsters)) {
    name <- genbalpollsters[i]
    pollnation <- subset(genbalpolls, pollster == name)
    uspoll[i,1] <- mean(c(pollnation[,"bias"]))
  }

gbweights <- matrix(0, nrow = length(genbalpollsters), ncol = 1)
rownames(gbweights) <- genbalpollsters
colnames(gbweights) <- c("Weight")

for (i in 1:length(genbalpollsters)) {
  company <- genbalpollsters[i]
  gpsd <- subset(genbalpolls, pollster == company & !is.na(bias))
  gsd <- sd(gpsd[,"bias"])
  if (is.na(gsd)) {
    gbweights[i,1] <- 1
  }
  else if (sd < 3) {
    gbweights[i,1] <- 2
  }
  else if (sd >= 3 & sd < 4) {
    gbweights[i,1] <- 1.67
  }
  else if (sd >= 4 & sd < 5) {
    gbweights[i,1] <- 1.33
  }
  else {
    gbweights[i,1] <- 1
  }
}

write.csv(gbweights,'/Users/rohanathreya/Projects/gbweights.csv')

write.csv(uspoll, '/Users/rohanathreya/Projects/genbalbias.csv')