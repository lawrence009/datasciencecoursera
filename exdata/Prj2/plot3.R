# 3. Of the four types of sources indicated by the type (point, nonpoint,
# onroad, nonroad) variable, which of these four sources have seen decreases in
# emissions from 1999-2008 for Baltimore City? Which have seen increases in
# emissions from 1999-2008?

library(data.table)

## Load and process data
if (!exists('NEI')) {
    NEI <- readRDS("summarySCC_PM25.rds")
    NEI$type <- as.factor(NEI$type)
}
if (!exists('SCC')) {
    SCC <- readRDS("Source_Classification_Code.rds")
}


pm25Baltimore <- subset(NEI,
                        fips == 24510 & Emissions > 0,
                        c(Emissions, type, year))

pm25Baltimore[, year:=as.factor(year)]


png('plot3.png', width = 640, height = 480)


library(ggplot2)

g <- qplot(year, Emissions,
           data = pm25Baltimore,
           #stat = 'bin',
           geom = c('point'),
           facets = . ~ type,
           log = 'y',
           main = 'Baltimore City\nPM25 Emissions by Sources from 1999-2008',
           ylab = 'Emmission in Tons') +
    geom_smooth(aes(group = 1),
                method = "lm", se = T)

print(g)

dev.off()