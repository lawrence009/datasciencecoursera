library(data.table)

filename <- 'repdata-data-StormData.csv.bz2'

if (!exists('dt')) {
    dt <- read.csv(filename, nrows = 100)

    colClasses <- vapply(dt, class, 'character')

    dt <- as.data.table(read.csv(filename, colClasses = 'character'))
}
