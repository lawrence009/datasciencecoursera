# R script to analyze the tidy data set prepared with HAR_ETS.R

# comment out if you don't want to reload the data
source('HAR_ETS.R')

# We only want measurements on the mean and standard deviation
# select mean and std
features.mean  <- grep('-mean', names(df), fixed=T)
features.std <- grep('-std', names(df), fixed=T)

df2 <- df[, c(2, features.mean, features.std)]

activity.mean <- aggregate(df2[, 2:80], list(activity=df$activity), mean)

write.table(activity.mean, file="activity.means.txt", row.name=F)