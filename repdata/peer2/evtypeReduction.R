source('readData.R')


#F : F-Scale ; Fujita Scale

# #VOLCANIC ASH
# dtl[grepl('VO', EVTYPE), EVTYPE:='VOLCANIC ASH']
#
# #WATERSPOUT
#

#Summary
#HUNDERSTORM WIND
events[grepl('summary', EVTYPE) & grepl('[Tt]hunderstorm.*[Ww]ind', REMARKS), EVTYPE:='THUNDERSTORM WIND']

#HAIL
events[grepl('summary', EVTYPE) & grepl('[Hh]ail', REMARKS), EVTYPE:='HAIL']

#FLASH FLOOD
events[grepl('summary', EVTYPE) & grepl('[Ff]lash flood', REMARKS), EVTYPE:='FLASH FLOOD']

#LIGHTNING
events[grepl('summary', EVTYPE) & grepl('[Ll]ightning', REMARKS), EVTYPE:='LIGHTNING']

#blizzards
events[grepl('summary', EVTYPE) & grepl('[Bb]lizzard', REMARKS), EVTYPE:='BLIZZARD']

events[grepl('summary', EVTYPE), REMARKS]



sort(events[, unique(EVTYPE)])


evmap <- read.csv('eventmapping.csv')[2:3]
