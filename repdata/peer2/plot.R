library(lattice)
library(data.table)

state.f <- as.factor(dtx$STATE)
year.f <- as.factor(year(dtx$BGN_DATE))
mth.f <- as.factor(month(dtx$BGN_DATE))

dt0 <- transform(dtx, STATE=as.factor(STATE),
                      year=as.factor(year(BGN_DATE)),
                      month=as.factor(month(BGN_DATE)))

dt0[, PropDmg:=sapply(PropDmg, function(x) if(is.na(x)) {0} else {x})]
dt0[, CropDmg:=sapply(CropDmg, function(x) if(is.na(x)) {0} else {x})]
dt0[, FATALITIES:=sapply(FATALITIES, function(x) if(is.na(x)) {0} else {x})]
dt0[, INJURIES:=sapply(INJURIES, function(x) if(is.na(x)) {0} else {x})]


dt1 <- aggregate(cbind(econ=(PropDmg+CropDmg),
                       humn=(FATALITIES+INJURIES)) ~ EventName + STATE,
                 data = dt0, sum, na.rm = T)

#xyplot(mth.f~sum(PropDmgVal, na.rm = T) , data = dtx)

l <- xyplot(humn~econ | EventName,
            data = dt1,
            scales=list(y=list(log10),x=list(log10)))

print(l)

dt1 <- aggregate(cbind(econ=(PropDmg+CropDmg),
                       humn=(FATALITIES+INJURIES)) ~ EventName + STATE + year + month,
                 data = dt0, sum, na.rm = T)

bwplot(~econ | EventName, data = dt1)

bwplot(~humn | EventName, data = dt1)


