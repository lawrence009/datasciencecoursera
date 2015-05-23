

table(dtx[(PropDmg + CropDmg) > 0, c('Region', 'Element1'), with=F]) -> dmg.freq
table(dtx[FATALITIES > 0, c('Region', 'Element1'), with=F]) -> ftl.freq
table(dtx[INJURIES   > 0, c('Region', 'Element1'), with=F]) -> inj.freq


freq.summary <- function(x) {
    y <- table(dtx[, c('Region', 'Element1'), with=F])

    z <- merge(x, y, by=c('Region', 'Element1'))

    z$ratio <- z$Freq.x / z$Freq.y

    with(z, z[order(Region, -Freq.x, -ratio), ])
}

freq.summary(dmg.freq)
freq.summary(ftl.freq)
freq.summary(inj.freq)
