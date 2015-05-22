# property damage; bwplot

library(data.table)
library(lattice)

#casuality mulitplier
lamda <- 100

dt0 <- transform(dtx, econ=(PropDmg + CropDmg) * 1e3,
                      humn=(FATALITIES*lamda + INJURIES),
                      year=as.factor(year(BGN_DATE)),
                      month=as.factor(month(BGN_DATE))
                 )

p <- bwplot(econ~Element1 | Region, data = dt0[econ > 0],
            par.strip.text=list(cex=0.9),
            main = 'Economic Impact',
            ylab = 'Property and Crop Damages (dollar)',
            #xlim = c(0, 1e7),
            scales=list(cex = 0.9,
                        x =list(rot=90),
                        y = list(log = 10)))


print(p)


setorder(dt0, econ)
tail(dt0, 25)
