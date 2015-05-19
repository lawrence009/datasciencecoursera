library(data.table)

mag <- function(x) {
    #x is magnitude
    #If it is not one of 'K', 'M' or 'B', return 'K'.
    #The resulting value is always upper case.

    x <- toupper(x)

    if (!grepl('[KMB]', x) | is.na(x)) {
        'K'
    } else {
        x
    }
}

dmgAmt <- function(value, magnitude = 'K') {
    #returns dollar value in mulitple of thousands.
    #If mangitude is [1-9], accepts it
    #If magnitude is not one of 'K', 'M' or 'B', assume 'K'.

    #assumes value and magnitude are both equal length

    val <- numeric(length(value))

    for (i in 1:length(value)) {
        mag <- toupper(magnitude[i])

        if (grepl('[1-9]', mag)) {
            val[i] <- value[i] * 10^(as.numeric(mag) - 3)
        } else if (!grepl('[MB]', mag) | is.na(mag)) {
            val[i] <- value[i]
        } else if (mag == 'M') {
            val[i] <- value[i] * 1e3
        } else if (mag == 'B') {
            val[i] <- value[i] * 1e6
        }
    }

    val
}



print(head(dtl[PROPDMG > 0, (dmgAmt(PROPDMG, PROPDMGEXP))]))

dtl[PROPDMG > 0, PropDmg:=dmgAmt(PROPDMG, PROPDMGEXP)]

dtl[CROPDMG > 0, CropDmg:=dmgAmt(CROPDMG, CROPDMGEXP)]


dtl[, .(PropDmg=sum(PropDmg, na.rm = T),
        CropDmg=sum(CropDmg, na.rm = T),
        Injuries=sum(INJURIES, na.rm = T),
        Fatalitites=sum(FATALITIES, na.rm = T)),
    by = STATE] -> dmg.summary



bad.propexp <- dtl[, PROPDMG > 0 & !grepl('[KMBkmb]', PROPDMGEXP)]


bad.cropexp <- dtl[, CROPDMG > 0 & !grepl('[KMBkmb]', CROPDMGEXP)]


propdmg <- dtl[bad.propexp, .(PropDmg=sum(PropDmg, na.rm = T),
                              PROPDMG=sum(PROPDMG, na.rm = T))]
cropdmg <- dtl[bad.cropexp, .(CropDmg=sum(CropDmg, na.rm = T),
                              CROPDMG=sum(CROPDMG, na.rm = T))]

rm(bad.propexp, bad.cropexp)

dmg.adj <- cbind(t(propdmg), t(cropdmg))
colnames(dmg.adj) <- c('Properties', 'Crops')
rownames(dmg.adj) <- c('Adjusted', 'Original')

print(dmg.adj)

rm(propdmg, cropdmg)

