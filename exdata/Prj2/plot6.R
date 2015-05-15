# 6. Compare emissions from motor vehicle sources in Baltimore City with
# emissions from motor vehicle sources in Los Angeles County, California
# (fips == "06037"). Which city has seen greater changes over time in motor
# vehicle emissions?

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

pm25bcla <- merge(pm25bcla, SCC, by = 'SCC') 

motov <- grepl('(Motor|Veh)', pm25bcla$Short.Name)

pm25bcla[motov, .(sum=sum(Emissions)), keyby = c('fips', 'year')]
