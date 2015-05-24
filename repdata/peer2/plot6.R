# event frequency; adjusted

library(data.table)
library(reshape2)
library(lattice)

#casuality mulitplier
lamda <- 100

dtx[, `:=`(econ  =(PropDmg + CropDmg),
           humn  =(FATALITIES*lamda + INJURIES),
           year  =(year(BGN_DATE)),
           month =as.factor(month(BGN_DATE)))]

dt1 <- table(dtx[, c('Region', 'Element1'), with=F])
dt1 <- melt(dt1)

poly.colors <- trellis.par.get("superpose.polygon")$col
poly.colors <- c(poly.colors, 'plum')

p <- barchart(value ~ Element1, data = dt1,
              main = 'Frequency of Events by Element',
              groups = Region,
              beside = T,
              ylab = 'No. of Occurences',
              scales=list(x=list(rot = 90)),
              auto.key=list(x = .05, y=.6, corner = c(0, 0), cex = 0.7),
              par.settings = list(superpose.polygon = list(col = poly.colors)))

print(p)

setorder(dtx, humn)
dtx
