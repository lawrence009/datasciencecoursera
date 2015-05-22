# event frequency; adjusted

library(data.table)
library(reshape2)
library(lattice)

#casuality mulitplier
lamda <- 100

dt0 <- transform(dtx, econ=(PropDmg + CropDmg),
                      humn=(FATALITIES*lamda + INJURIES),
                      year=as.factor(year(BGN_DATE)),
                      month=as.factor(month(BGN_DATE))
                 )

dt1 <- table(dt0[, c('Region', 'Element1'), with=F])
dt1 <- melt(dt1)

p <- barchart(value ~ Element1 | Region, data = dt1,
              main = 'Frequency of Events',
              ylab = 'No. of Occurences',
              scales=list(x=list(rot = 90)))

print(p)

setorder(dt0, humn)
dt0
