# property damage

library(data.table)
library(lattice)

#casuality mulitplier
lamda <- 100

dt0 <- transform(dtx, econ=(PropDmg + CropDmg),
                      humn=(FATALITIES*lamda + INJURIES),
                      year=as.factor(year(BGN_DATE)),
                      month=as.factor(month(BGN_DATE))
                 )


p <- stripplot(Element1~econ | Region, data = dt0[econ > 0],
               jitter.data = T)

print(p)


# dt1 <- aggregate(cbind(econ,humn) ~ EventName + STATE + year + month,
#                  data = dt0, sum, na.rm = T)
#


# bwplot(~econ | EventName, data = dt1)
#
# stripplot(~humn | EventName, data = dt1)


# bwplot(~econ | EventName, data = dt1)
#
# stripplot(~humn | EventName, data = dt1)
