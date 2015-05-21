library(data.table)
library(lattice)


dt0 <- transform(dtx, STATE=as.factor(STATE),
                      year=as.factor(year(BGN_DATE)),
                      month=as.factor(month(BGN_DATE)))

dt0[is.na(PropDmg), PropDmg:=0]
dt0[is.na(CropDmg), CropDmg:=0]

dt0[is.na(FATALITIES), FATALITIES:=0]
dt0[is.na(INJURIES),   INJURIES:=0]


dt1 <- aggregate(cbind(econ=(PropDmg+CropDmg),
                       humn=(FATALITIES+INJURIES)) ~ Element1 + Region,
                 data = dt0, sum, na.rm = T)

l <- xyplot(humn~econ | Element1*Region,
            data = dt1)

print(l)



