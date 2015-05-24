# 1.Have total emissions from PM2.5 decreased in the United States from 1999
# to 2008?

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
    NEI <- readRDS("summarySCC_PM25.rds")
    NEI$type <- as.factor(NEI$type)
}
if (!exists('SCC')) {
    SCC <- readRDS("Source_Classification_Code.rds")
}


pm25.year.total <- tapply(NEI$Emissions, NEI$year, sum)


png('plot1.png', width = 480, height = 480)

barplot(pm25.year.total / 1000000,
        main = expression('PM'[2.5]*'Emissions in the United States between 1999 and 2008'),
        ylab = 'Total Emission in Million Tons')

dev.off()