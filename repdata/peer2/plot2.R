library(data.table)
library(lattice)


dt0 <- transform(dtx, year=as.factor(year(BGN_DATE)),
                      month=as.factor(month(BGN_DATE)))


dt1 <- aggregate(cbind(econ=(PropDmg+CropDmg),
                       humn=(FATALITIES+INJURIES)) ~ EventName + STATE + year + month,
                 data = dt0, sum, na.rm = T)

bwplot(~econ | EventName, data = dt1)

stripplot(~humn | EventName, data = dt1)


dt1 <- aggregate(cbind(econ=(PropDmg+CropDmg),
                       humn=(FATALITIES+INJURIES)) ~ Element1 + Region,
                 data = dt0, sum, na.rm = T)

l <- xyplot(humn~econ | Element1*Region,
            data = dt1
            )

print(l)



