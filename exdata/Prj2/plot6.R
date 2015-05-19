# 6. Compare emissions from motor vehicle sources in Baltimore City with
# emissions from motor vehicle sources in Los Angeles County, California
# (fips == "06037"). Which city has seen greater changes over time in motor
# vehicle emissions?

library(data.table)

library(data.table)
## Load and process data
if (!exists('NEI')) {
    NEI <- as.data.table(readRDS("summarySCC_PM25.rds"))
    NEI$Pollutant <- NULL
}

if (!exists('SCC')) {
    SCC <- readRDS("Source_Classification_Code.rds")
}


#Baltimore County; BC = 24510
#Los Angeles County; LA = 06037
pm25bcla <- NEI[fips %in% c('24510', '06037')]
pm25bcla[fips == '24510', fips:='Baltimore']
pm25bcla[fips == '06037', fips:='Los Angeles']

pm25bcla <- merge(pm25bcla, SCC, by = 'SCC')

pm25bcla[, year:=as.factor(year)]

motov <- grepl('(Motor|Veh)', pm25bcla$Short.Name)



png('plot6.png', width = 640, height = 480)

library(ggplot2)

g <- qplot(year, 
           Emissions,
           data = pm25bcla[motov & Emissions > 0, ],
           geom = 'jitter',
           facets = .~fips,
           color = fips,
           log  = 'y',
           main = paste0('PM25 Emissions from Motor Vehicle Sources\n',
                         'Baltimore vs. Los Angeles County between 1999-2008'),
           ylab = 'Emmission in Tons')


print(g)

dev.off()