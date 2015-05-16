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
}


#dtl[1:10, as.Date(BGN_DATE, '%m/%d/%Y')]

#dtl[1:10, as.IDate(BGN_DATE, '%m/%d/%Y')]

#dtl[, c('FATALITIES','INJURIES','PROPDMG'):=list(as.numeric(FATALITIES), as.numeric(INJURIES), as.numeric(PROPDMG))]

dtl[, ':='(STATE__    =as.integer(STATE__),
           BGN_DATE   =as.IDate(BGN_DATE, '%m/%d/%Y'),
           REFNUM     =as.integer(REFNUM))]

#dtl[FATALITIES & 0 & INJURIES == 0 & PROPDMG == 0]

#dtl[, EVTYPE:=toupper(EVTYPE)]

events <- dtl[, c('REFNUM', 'EVTYPE', 'REMARKS'), with=F]

#F : F-Scale ; Fujita Scale

# #VOLCANIC ASH
# dtl[grepl('VO', EVTYPE), EVTYPE:='VOLCANIC ASH']
#
# #WATERSPOUT
#

#Summary
#HUNDERSTORM WIND
events[grepl('Summary', EVTYPE) & grepl('[Tt]hunderstorm.*[Ww]ind', REMARKS), EVTYPE:='THUNDERSTORM WIND']

#HAIL
events[grepl('Summary', EVTYPE) & grepl('[Hh]ail', REMARKS), EVTYPE:='HAIL']

#FLASH FLOOD
events[grepl('Summary', EVTYPE) & grepl('[Ff]lash flood', REMARKS), EVTYPE:='FLASH FLOOD']

#LIGHTNING
events[grepl('Summary', EVTYPE) & grepl('[Ll]ightning', REMARKS), EVTYPE:='LIGHTNING']

#blizzards
events[grepl('Summary', EVTYPE) & grepl('[Bb]lizzard', REMARKS), EVTYPE:='BLIZZARD']

events[grepl('Summary', EVTYPE), REMARKS]


sort(events[, unique(EVTYPE)])
