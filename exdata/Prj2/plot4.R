# 4. Across the United States, how have emissions from coal combustion-related
# sources changed from 1999-2008?

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
}

if (!exists('SCC')) {
    SCC <- readRDS("Source_Classification_Code.rds")
}


coal.sources <- with(SCC, as.character(SCC[grepl('comb.*coal',
                                                 Short.Name,
                                                 ignore.case = T)]))

pm25coal <- subset(NEI,
                   SCC %in% coal.sources,
                   c(Emissions, type, year))

pm25coal[, year:=as.factor(year)]



png('plot4.png', width = 480, height = 480)

library(ggplot2)

g <- qplot(year, Emissions,
           data = pm25coal,
           fill = year,
           geom = c('boxplot'),
           facets = . ~ type,
           log = 'y',
           main = 'PM25 Emissions from Coal Sources\nUnited States between 1999-2008',
           ylab = 'Emmission in Tons')

print(g)

dev.off()