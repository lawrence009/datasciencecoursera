# event frequency; adjusted

library(data.table)
library(reshape2)
library(lattice)

#casuality mulitplier
lamda <- 100

dtx[, `:=`(econ  =(PropDmg + CropDmg),
           humn  =(FATALITIES*lamda + INJURIES),
           year  =(year(BGN_DATE)),
           month =as.factor(month(BGN_DATE)))]

p <- histogram( ~BGN_DATE | Element1*Region , data = dtx[econ >0 | humn > 0],
               par.strip.text = list(cex = 0.7),
              main = 'Frequency of Events',
              type = 'percent',
              nint = 25,
              #ylab = 'No. of Occurences',
              scales=list(x=list(rot = 90)))

print(p)

setorder(dtx, humn)
dtx
