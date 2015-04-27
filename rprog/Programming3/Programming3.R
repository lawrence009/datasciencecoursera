#
hospital <- read.csv('hospital-data.csv')

#first 10 column headers are the same as hospital.csv
outcome <- read.csv('outcome-of-care-measures.csv', colClasses = 'character')

outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])

#which hospital is measured for care of outcome
measured <- hospital$Provider.Number %in% outcome$Provider.Number
