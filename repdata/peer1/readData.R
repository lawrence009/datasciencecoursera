#read the data and explore the data

df <- read.csv('activity.csv')

print(paste('range:', range(df$steps, na.rm = T)))

str(df)