library(data.table)

filename <- 'repdata-data-StormData.csv.bz2'

sample <- read.csv(filename, nrows = 100)

colClasses <- vapply(sample, class, 'character')

#large data set; skip read if already exists in the environment
if (!exists('dtl')) {
    dtl <- as.data.table(read.csv(filename, na.strings = '', colClasses = 'character'))
}


dtl[1:10, as.Date(BGN_DATE, '%m/%d/%Y')]

dtl[1:10, as.IDate(BGN_DATE, '%m/%d/%Y')]


sort(dtl[, unique(EVTYPE)])

dtl[, EVTYPE:=toupper(EVTYPE)]

#VOLCANIC ASH
dtl[grepl('VO', EVTYPE), EVTYPE:='VOLCANIC ASH']

#WATERSPOUT

#HUNDERSTORM WIND
dtl[grepl('Summary', EVTYPE) & grepl('[Tt]hunderstorm.*[Ww]ind', REMARKS), EVTYPE:='THUNDERSTORM WIND']

#HAIL
dtl[grepl('Summary', EVTYPE) & grepl('[Hh]ail', REMARKS), EVTYPE:='HAIL']

#FLASH FLOOD
dtl[grepl('Summary', EVTYPE) & grepl('[Ff]lash flood', REMARKS), EVTYPE:='FLASH FLOOD']

#LIGHTNING
dtl[grepl('Summary', EVTYPE) & grepl('[Ll]ightning', REMARKS), EVTYPE:='LIGHTNING']

#blizzards
dtl[grepl('Summary', EVTYPE) & grepl('[Bb]lizzard', REMARKS), EVTYPE:='BLIZZARD']

dtl[grepl('Summary', EVTYPE), REMARKS]




#dtl[, MAG:=as.integer(MAG)]
