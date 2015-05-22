# human casuality bwplot; unadjusted

library(data.table)
library(lattice)

#casuality mulitplier
lamda <- 1

dt0 <- transform(dtx, econ=(PropDmg + CropDmg),
                      humn=(FATALITIES*lamda + INJURIES),
                      year=as.factor(year(BGN_DATE)),
                      month=as.factor(month(BGN_DATE))
                 )

p <- bwplot(humn~Element1 | Region, data = dt0[humn > 0],
            main = 'Human Casulities',
            ylab = 'Fatalities and Injuries',
            scales=list(x=list(rot=90), y = list(log = 10)))

print(p)


setorder(dt0, humn)
dt0
