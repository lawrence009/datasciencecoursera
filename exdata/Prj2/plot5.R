# 5. How have emissions from motor vehicle sources changed from 1999–2008 in
# Baltimore City?

## Load and process data

url <- 'https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip'
filename <- 'exdata-data-NEI_data.zip'

#check if the data file is available
if (!file.exists(filename)) {
    download.file(url = url, destfile = filename)
    unzip(filename)
}



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