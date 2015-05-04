#test cases
source('rankall.R')

df.expected <- read.csv('testcaseRankall_pneumonia_worst.csv', colClasses = 'character')[, 2:3]
df.result <- rankall("pneumonia", "worst")
print(identical(df.expected, df.result))
