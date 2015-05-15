library(data.table)

filename <- 'repdata-data-StormData.csv.bz2'

sample <- read.csv(filename, nrows = 100)

colClasses <- vapply(sample, class, 'character')

#large data set; skip read if already exists in the environment
if (!exists('dtbl')) {
    dtbl <- as.data.table(read.csv(filename, colClasses = 'character'))
}

