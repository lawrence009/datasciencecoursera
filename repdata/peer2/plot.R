library(data.table)
library(lattice)


dt0 <- transform(dtx, year=as.factor(year(BGN_DATE)),
                      month=as.factor(month(BGN_DATE)))




dt1 <- aggregate(cbind(econ=(PropDmg+CropDmg),
                       humn=(FATALITIES+INJURIES)) ~ Element1 + Region,
                 data = dt0, sum, na.rm = T)

l <- xyplot(Region~humn | Element1,
            data = dt1,
            scales=list(y=list(log10),x=list(log10)))

print(l)
