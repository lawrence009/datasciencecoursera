# proprty damange dotplot

library(data.table)
library(lattice)

#casuality mulitplier
lamda <- 100

dt0 <- transform(dtx, econ=(PropDmg + CropDmg),
                      humn=(FATALITIES*lamda + INJURIES),
                      year=as.factor(year(BGN_DATE)),
                      month=as.factor(month(BGN_DATE))
                 )

p <- dotplot(Element1~econ | Region, data = dt0[econ > 0],
             par.strip.text=list(cex=0.9),
             #xlim = c(0, 1e7),
             scales=list(cex = 0.9))


print(p)


setorder(dt0, econ)
tail(dt0, 25)
