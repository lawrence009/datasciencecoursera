# 3. Of the four types of sources indicated by the type (point, nonpoint,
# onroad, nonroad) variable, which of these four sources have seen decreases in
# emissions from 1999-2008 for Baltimore City? Which have seen increases in
# emissions from 1999-2008?


## Load and process data

url <- 'https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip'
filename <- 'exdata-data-NEI_data.zip'

#check if the data file is available
if (!file.exists(filename)) {
    download.file(url = url, destfile = filename)
    unzip(filename)
}



library(data.table)

url <- 'https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip'
filename <- 'exdata-data-NEI_data.zip'

#check if the data file is available
if (!file.exists(filename)) {
    download.file(url = url, destfile = filename)
    unzip(filename)
}



library(data.table)

if (!exists('NEI')) {
    NEI <- readRDS("summarySCC_PM25.rds")
    NEI$type <- as.factor(NEI$type)
}
if (!exists('SCC')) {
    SCC <- readRDS("Source_Classification_Code.rds")
}


pm25Baltimore <- subset(NEI,fips == 24510, c(Emissions, type, year))

pm25Baltimore[, year:=as.factor(year)]


png('plot3.png', width = 640, height = 480)


library(ggplot2)

g <- qplot(year, Emissions,
           data = pm25Baltimore[Emissions > 0, ],
           geom = c('boxplot'),
           facets = . ~ type,
           log = 'y',
           main = 'Baltimore City\nPM25 Emissions by Sources from 1999-2008',
           ylab = 'Emmission in Tons') +
    geom_smooth(aes(group = 1),
                method = "lm", se = T)

print(g)

dev.off()