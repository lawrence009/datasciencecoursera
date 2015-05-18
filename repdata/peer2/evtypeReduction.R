source('readData.R')


#F : F-Scale ; Fujita Scale

# #VOLCANIC ASH
# dtl[grepl('VO', EVTYPE), EVTYPE:='VOLCANIC ASH']
#
# #WATERSPOUT
#

# #Summary
# #HUNDERSTORM WIND
# events[grepl('summary', EVTYPE) & grepl('[Tt]hunderstorm.*[Ww]ind', REMARKS), EVTYPE:='THUNDERSTORM WIND']
#
# #HAIL
# events[grepl('summary', EVTYPE) & grepl('[Hh]ail', REMARKS), EVTYPE:='HAIL']
#
# #FLASH FLOOD
# events[grepl('summary', EVTYPE) & grepl('[Ff]lash flood', REMARKS), EVTYPE:='FLASH FLOOD']
#
# #LIGHTNING
# events[grepl('summary', EVTYPE) & grepl('[Ll]ightning', REMARKS), EVTYPE:='LIGHTNING']
#
# #blizzards
# events[grepl('summary', EVTYPE) & grepl('[Bb]lizzard', REMARKS), EVTYPE:='BLIZZARD']
#
# events[grepl('summary', EVTYPE), REMARKS]



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

