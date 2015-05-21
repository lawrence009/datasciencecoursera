# human casuality dotplot

library(data.table)
library(lattice)

#casuality mulitplier
lamda <- 100

dt0 <- transform(dtx, econ=(PropDmg + CropDmg),
                      humn=(FATALITIES*lamda + INJURIES),
                      year=as.factor(year(BGN_DATE)),
                      month=as.factor(month(BGN_DATE))
                 )

p <- dotplot(Element1~humn | Region, data = dt0[humn > 0],
             main = 'Human Casulities (adjusted figures)')

print(p)


setorder(dt0, humn)
dt0
