library(lattice)
library(data.table)

state.f <- as.factor(dtl$STATE)
year.f <- as.factor(year(dtl$BGN_DATE))
mth.f <- as.factor(month(dtl$BGN_DATE))

dt0 <- transform(dtl, STATE=as.factor(STATE),
                      year=as.factor(year(dtl$BGN_DATE)),
                      month=as.factor(month(dtl$BGN_DATE)))

dt1 <- na.omit(dt0)

#xyplot(mth.f~sum(PropDmgVal, na.rm = T) , data = dtl)

xyplot(CropDmg~PropDmg | STATE,
       data = dt0,
       scales=list(y=list(log10),x=list(log10)))
