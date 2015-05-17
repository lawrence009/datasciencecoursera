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
    #If magnitude is not one of 'K', 'M' or 'B', assume 'K'.

    #assumes value and magnitude are both equal length

    val <- numeric(length(value))

    for (i in 1:length(value)) {
        mag <- toupper(magnitude[i])

        if (!grepl('[MB]', mag) | is.na(mag)) {
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

dtl[PROPDMG > 0, PropDmgVal:=dmgAmt(PROPDMG, PROPDMGEXP)]

dtl[CROPDMG > 0, CropDmgVal:=dmgAmt(CROPDMG, CROPDMGEXP)]

print(
dtl[, .(PropDmg=sum(PropDmgVal, na.rm = T),
        CropDmg=sum(CropDmgVal, na.rm = T),
        Injuries=sum(INJURIES, na.rm = T),
        Fatalitites=sum(FATALITIES, na.rm = T)),
    by = STATE]
)
