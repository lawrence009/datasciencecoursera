library(data.table)

filename <- 'repdata-data-StormData.csv.bz2'

sample <- read.csv(filename, nrows = 100)

colClasses <- vapply(sample, class, 'character')

#large data set; skip read if already exists in the environment
if (!exists('dtl')) {
    dtl <- as.data.table(read.csv(filename, na.strings = '', colClasses = 'character'))
}



sort(dtl[, unique(EVTYPE)])


dtl[grepl('Summary', EVTYPE) & grepl('wind', REMARKS), REMARKS]

dtl[grepl('Summary', EVTYPE) & !grepl('wind', REMARKS), REMARKS]

dtl[1:10, as.Date(BGN_DATE, '%m/%d/%Y')]

dtl[1:10, as.IDate(BGN_DATE, '%m/%d/%Y')]
