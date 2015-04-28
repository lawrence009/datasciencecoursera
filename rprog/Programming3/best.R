library(dplyr)

best <- function(state, outcome) {
    #Read outcome data
    url <- 'outcome-of-care-measures.csv'

    #which columns contain 30-Day Death (mortality) rate
    colheads <- as.character(read.csv(url, header = F, nrows = 1, stringsAsFactors = F))
    mort <- grep('^Hospital 30-Day Death \\(Mortality', colheads)
    
    #trying to set the colClasses for mortality rate columns ahead of time doesn't seem to work 
#     colClasses <- rep_len('character', length(colheads))
#     colClasses[mort] <- 'numeric'
#     df <- read.csv(url, colClasses = colClasses)
    
    #read full dataset, subset and transform
    df <- read.csv(url, colClasses = 'character')
    df <- df[, c(1, 2, 7, mort)] #(1)provider number, (2)hopspital name, (7)state
    df[, 4:6] <- suppressWarnings(sapply(df[, 4:6], as.numeric))
    colnames(df)[4:6] <- substring(colnames(df)[4:6], first = 46) #remove the first 46 charaters
   
    
    ## Check that state param is valid
    if(!(state %in% df$State)) {
        stop('invalid param  state: ', state)
    }
   
    acceptable <- colnames(df[4:6])
    names(acceptable) <- c('heart attack', 'heart failure', 'pneumonia')

    ## Check that outcome param is valid
    if(!(outcome %in% names(acceptable))) {
        stop('invalid param outcome: ', outcome)
    }


    #none dpyrl solution
    df <- df[df$State == state, c('Hospital.Name', 'State', acceptable[outcome])]
    df <- df[order(df[1]), ] #order by states first
    df[order(df[3]), 1][1] #then order by outcome and return the first row hospital name



}