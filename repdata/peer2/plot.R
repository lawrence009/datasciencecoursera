library(lattice)
library(data.table)

state.f <- as.factor(dtl$STATE)
year.f <- as.factor(year(dtl$BGN_DATE))
mth.f <- as.factor(month(dtl$BGN_DATE))



#xyplot(mth.f~sum(PropDmgVal, na.rm = T) , data = dtl)
