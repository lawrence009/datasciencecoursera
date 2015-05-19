source('readData.R')

events[, EVTYPE:=dtl$EVTYPE]

# #VOLCANIC ASH
# dtl[grepl('VO', EVTYPE), EVTYPE:='VOLCANIC ASH']
#
# #WATERSPOUT
#

# #Summary
#thunderstorm wind
events[grepl('summary', EVTYPE) & grepl('[Tt]hunderstorm.*[Ww]ind', REMARKS), EVTYPE:='thunderstorm Wind']

#hail
events[grepl('summary', EVTYPE) & grepl('[Hh]ail', REMARKS), EVTYPE:='hail']

#flashflood
events[grepl('summary', EVTYPE) & grepl('[Ff]lash flood', REMARKS), EVTYPE:='flash Flood']

#lightning
events[grepl('summary', EVTYPE) & grepl('[Ll]ightning', REMARKS), EVTYPE:='lightning']

#blizzards
events[grepl('summary', EVTYPE) & grepl('[Bb]lizzard', REMARKS), EVTYPE:='blizzard']

events[grepl('summary', EVTYPE), REMARKS]



# sort(events[, unique(EVTYPE)])


evmap <- read.csv('EventMapping.csv',
                  na.strings = '',
                  colClasses = 'character')[2:3]

evmap.NA <- evmap$EVTYPE[is.na(evmap$EventName)]

evmap.ok <- evmap$EVTYPE[!is.na(evmap$EventName)]



dmg.notmapped <- dtl[which(EVTYPE %in% evmap.NA),
                    .(fatalities=sum(FATALITIES, na.rm = T),
                      injuries=sum(INJURIES, na.rm = T),
                      proddmg=sum(PropDmg, na.rm = T),
                      cropdmg=sum(CropDmg, na.rm = T))]


dmg.mapped <- dtl[which(EVTYPE %in% evmap.ok),
                  .(fatalities=sum(FATALITIES, na.rm = T),
                    injuries=sum(INJURIES, na.rm = T),
                    proddmg=sum(PropDmg, na.rm = T),
                    cropdmg=sum(CropDmg, na.rm = T))]


dmg.summary <- as.matrix(rbind(dmg.mapped, dmg.notmapped))
rownames(dmg.summary) <- c('mapped', 'not mapped')

print(dmg.summary)


dtx <- merge(dtl, evmap, by = 'EVTYPE', all.x = T)
