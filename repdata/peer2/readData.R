library(data.table)

url <- 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2'
filename <- 'repdata-data-StormData.csv.bz2'

# check if the data file is available
# if (!file.exists(filename)) {
#
#     download.file(url = url, destfile = filename)
#
#     if (!require(R.utils)) {
#         install.packages('R.utils')
#         library(R.utils)  # a library for unzipping bz2 files
#     }
#     bunzip2(filename = filename,
#             destname = 'repdata-data-StormData.csv')  # get the csv file for the task
# }



sample1 <- read.csv(filename, na.strings = '', colClasses = 'character', nrows = 100)
sample2 <- read.csv(filename, na.strings = '', nrows = 100)
sample3 <- read.csv(filename, na.strings = '', nrows = 100, skip = 100000, header = F)
colnames(sample3) <- colnames(sample1)

colClasses2 <- vapply(sample2, class, 'character')
colClasses3 <- vapply(sample3, class, 'character')

ch2 <- grep('(logical|integer|factor)', colClasses2)
colClasses2[ch2] <- 'character'

as.data.frame(cbind(colClasses2, colClasses3))

head(read.csv(filename, na.strings = '', colClasses = colClasses2, nrows = 100))


head(sample1)

#large data set; skip read if already exists in the environment
if (!exists('dtl')) {
    dtl <- as.data.table(read.csv(filename, na.strings = '', colClasses = colClasses2))

    #dtl[1:10, as.Date(BGN_DATE, '%m/%d/%Y')]

    #dtl[1:10, as.IDate(BGN_DATE, '%m/%d/%Y')]

    #dtl[, c('FATALITIES','INJURIES','PROPDMG'):=list(as.numeric(FATALITIES), as.numeric(INJURIES), as.numeric(PROPDMG))]

    dtl[, ':='(STATE__    =as.integer(STATE__),
               BGN_DATE   =as.IDate(BGN_DATE, '%m/%d/%Y'),
               EVTYPE     =tolower(EVTYPE),
               REFNUM     =as.integer(REFNUM))]

    events <- dtl[, c('REFNUM', 'EVTYPE', 'REMARKS'), with=F]

    rmrks <-dtl[, c('REFNUM', 'REMARKS'), with=F]

    dtl <- dtl[, c(37, 2, 6:8, 22:28), with=F]
}

evtype <- sort(unique(dtl$EVTYPE))


#dtl[FATALITIES & 0 & INJURIES == 0 & PROPDMG == 0]


#REFNUM 605943
# http://napavalleyregister.com/news/local/napa-flood-damage-estimate-drops-to-million/article_bc2b3e27-1a7e-5886-ada4-6da209b4a5db.html
dtl[REFNUM == 605943, ':='(PROPDMG=13.75,
                           PROPDMGEXP='M',
                           CROPDMG=(114.2 - 13.75),
                           CROPDMGEXP='M')]



#F : F-Scale ; Fujita Scale
