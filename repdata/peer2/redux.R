source('readData.R')

write.csv(sort(dtl[, unique(EVTYPE)]),
          file = 'foo.csv')

## to save time, make a copy of events and avoid overwriting the origial copy
events[, EVTYPE:=dtl$EVTYPE] #reset the EVTYPE value for dev/test



# #VOLCANIC ASH
# events[grepl('vo', EVTYPE), EVTYPE:='VOLCANIC ASH']
#
# #WATERSPOUT
#

# #Summary
evtype.summary <- grep('^summary', events$EVTYPE)

#thunderstorm wind
events[grepl('^summary', EVTYPE) & grepl('[Tt]hunderstorm.*[Ww]ind', REMARKS), EVTYPE:='Thunderstorm Wind']

#hail
events[grepl('^summary', EVTYPE) & grepl('[Hh]ail', REMARKS), EVTYPE:='Hail']

#flashflood
events[grepl('^summary', EVTYPE) & grepl('[Ff]lash flood', REMARKS), EVTYPE:='Flash Flood']

#lightning
events[grepl('^summary', EVTYPE) & grepl('[Ll]ightning', REMARKS), EVTYPE:='Lightning']

#blizzards
events[grepl('^summary', EVTYPE) & grepl('[Bb]lizzard', REMARKS), EVTYPE:='Blizzard']

print(events[grepl('^summary', EVTYPE), REMARKS]) # should be 0 row





evmap <- read.csv('EventMapping.csv',
                  na.strings = '',
                  colClasses = 'character')[2:3]

evmap.NA <- evmap$EVTYPE[is.na(evmap$EventName)]

evmap.ok <- evmap$EVTYPE[!is.na(evmap$EventName)]



dmg.notmapped <- dtl[which(EVTYPE %in% evmap.NA),
                    .(fatalities=sum(FATALITIES),
                      injuries=sum(INJURIES),
                      proddmg=sum(PropDmg),
                      cropdmg=sum(CropDmg))]


dmg.mapped <- dtl[which(EVTYPE %in% evmap.ok),
                  .(fatalities=sum(FATALITIES),
                    injuries=sum(INJURIES),
                    proddmg=sum(PropDmg),
                    cropdmg=sum(CropDmg))]


dmg.summary <- as.matrix(rbind(dmg.mapped, dmg.notmapped))
rownames(dmg.summary) <- c('mapped', 'not mapped')

print(dmg.summary)


dtx <- merge(dtl, evmap, by = 'EVTYPE', all.x = T)

evtable <- read.csv('EventTable.csv',
                    na.strings = '')

dtx <- merge(dtx, evtable, by = 'EventName', all.x = T)


hist((as.numeric(dtx$EventName)))

hist((as.numeric(dtx$Element1)))



regions <- read.csv('BureauOfEconomicAnalysisRegions.csv')[c(3, 1)]
colnames(regions)[1] <- 'STATE'

dtx <- merge(dtx, regions, by = 'STATE', all.x = T)

summary(dtx)

table(dtx[(FATALITIES + INJURIES + PropDmg + CropDmg) > 0, c('Region', 'Element1'), with=F]) -> dmg.freq
table(dtx[, c('Region', 'Element1'), with=F]) -> element.freq

