# 2. Have total emissions from PM2.5 decreased in the Baltimore City, Maryland
# (fips == "24510") from 1999 to 2008?

library(data.table)

## Load and process data
if (!exists('NEI')) {
    NEI <- readRDS("summarySCC_PM25.rds")
    NEI$type <- as.factor(NEI$type)
}
if (!exists('SCC')) {
    SCC <- readRDS("Source_Classification_Code.rds")
}


pm25Baltimore <- subset(NEI, fips == 24510, c(Emissions, year))

pm25Baltimore.year.total <- tapply(pm25Baltimore$Emissions, 
                                   pm25Baltimore$year,
                                   sum)

png('plot2.png', width = 480, height = 480)

barplot(pm25Baltimore.year.total,
        main = 'Baltimore City, Maryland\nPM25 Emissions between 1999 and 2008',
        ylab = 'Total Emission in Tons')

dev.off()