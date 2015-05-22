# property damage; stripplot

library(data.table)
library(lattice)

#casuality mulitplier
lamda <- 100

dt0 <- transform(dtx, econ=(PropDmg + CropDmg),
                      humn=(FATALITIES*lamda + INJURIES),
                      year=as.factor(year(BGN_DATE)),
                      month=as.factor(month(BGN_DATE))
                 )


p <- stripplot(econ~Element1 | Region, data = dt0[econ > 0],
               jitter.data = T,
               scales=list(x=list(rot=90), y = list(log = 10)))

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
