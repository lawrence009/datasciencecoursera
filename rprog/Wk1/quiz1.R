data <- read.csv("./hw1_data.csv")

#11
names(data)

#12
data[1:2,]

#13
nrow(data)

#14
data[152:153,]

#15
data$Ozone[47]

#16
length(which(is.na(data$Ozone)))

#17
mean(data$Ozone, na.rm = T)

#18
mean(data$Solar.R[data$Ozone > 31 & data$Temp > 90], na.rm = T)

#19
mean(data$Temp[data$Month == 6], na.rm = T)

#20
max(data$Ozone[data$Month == 5], na.rm = T)

