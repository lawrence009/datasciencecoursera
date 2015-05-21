# human casuality dotplot; unadjusted

library(data.table)
library(lattice)

#casuality mulitplier
lamda <- 1

dt0 <- transform(dtx, econ=(PropDmg + CropDmg),
                      humn=(FATALITIES*lamda + INJURIES),
                      year=as.factor(year(BGN_DATE)),
                      month=as.factor(month(BGN_DATE))
                 )

p <- bwplot(Element1~humn | Region, data = dt0[humn > 0],
             main = 'Human Casulities (unadjusted figures)')

print(p)


setorder(dt0, humn)
dt0

dt1 <- aggregate(data = dtx, FUN = sum,
                 INJURIES ~ Element1 + Region)


barchart(Element1~INJURIES | Region, data = dt1,
             main = 'Human Casulities (unadjusted figures)')
