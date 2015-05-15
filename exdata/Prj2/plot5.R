# 5. How have emissions from motor vehicle sources changed from 1999–2008 in
# Baltimore City?

library(data.table)
library(reshape2)
## Load and process data
if (!exists('NEI')) {
    NEI <- as.data.table(readRDS("summarySCC_PM25.rds"))
    NEI$Pollutant <- NULL
}

if (!exists('SCC')) {
    SCC <- readRDS("Source_Classification_Code.rds")
}



pm25Baltimore <- NEI[fips == '24510']

pm25Baltimore <- merge(pm25Baltimore, SCC, by = 'SCC') 

#motor vehicle sources
motov <- grepl('(Motor|Veh)', pm25Baltimore$Short.Name)

#pm25Baltimore[, year:=as.factor(year)]

df <- pm25Baltimore[motov,
              .(mean=mean(Emissions),
                sum=sum(Emissions),
                count=length(Emissions)),
              keyby=year]

df <- melt(df, 'year')


library(ggplot2)

g <- qplot(year, 
           value,
           data = df,
           geom = c('point', 'smooth'),
           color = variable,
           position = 'dodge',
           #log = 'y',
           main = 'PM25 Emissions from Motor Vehicle Sources\nBaltimore County between 1999-2008',
           ylab = 'Total Emmission in Tons') + facet_grid(variable ~ ., scales = 'free_y')

print(g)

