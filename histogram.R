## create histogram of 30-day death rates from heart attack
outcome[,11] <- as.numeric(outcome[,11])
hist(outcome[,11])