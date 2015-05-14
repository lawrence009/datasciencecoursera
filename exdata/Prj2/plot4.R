# 4. Across the United States, how have emissions from coal combustion-related
# sources changed from 1999–2008?

## Load and process data
if (!exists('NEI')) {
    NEI <- readRDS("summarySCC_PM25.rds")
}

if (!exists('SCC')) {
    SCC <- readRDS("Source_Classification_Code.rds")
}


coal.sources <- with(SCC, as.character(SCC[grepl('[Cc]oal', Short.Name)]))

pm25coal <- subset(NEI,
                   SCC %in% coal.sources,
                   c(Emissions, type, year))


png('plot4.png', width = 480, height = 480)


library(ggplot2)

g <- qplot(year, Emissions+1,
           data = pm25coal,
           facets = . ~ type,
           geom = c('point', 'smooth'),
           log = 'y',
           method = 'lm',
           main = 'PM25 Emissions from Coal Sources\nUnited States between 1999-2008',
           ylab = 'Emmission in Tons')

print(g)

dev.off()