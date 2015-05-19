# 5. How have emissions from motor vehicle sources changed from 1999–2008 in
# Baltimore City?

library(data.table)

## Load and process data
if (!exists('NEI')) {
    NEI <- as.data.table(readRDS("summarySCC_PM25.rds"))
    NEI$Pollutant <- NULL
}

if (!exists('SCC')) {
    SCC <- readRDS("Source_Classification_Code.rds")
}



pm25Baltimore <- NEI[fips == '24510' & Emissions > 0]

pm25Baltimore <- merge(pm25Baltimore, SCC, by = 'SCC') 

#motor vehicle sources
motov <- grepl('(Motor|Veh)', pm25Baltimore$Short.Name)

pm25Baltimore[, year:=as.factor(year)]



png('plot5.png', width = 480, height = 480)

library(ggplot2)

g <- qplot(year, 
           Emissions,
           data = pm25Baltimore[motov, ],
           geom = 'boxplot',
           fill = year,
           log  = 'y',
           main = 'PM25 Emissions from Motor Vehicle Sources\nBaltimore County between 1999-2008',
           ylab = 'Emmission in Tons')

print(g)

dev.off()