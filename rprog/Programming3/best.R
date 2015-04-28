#library(data.table)

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
    df[, 4:6] <- sapply(df[, 4:6], as.numeric)
    colnames(df)[4:6] <- substring(colnames(df)[4:6], first = 46) #remove the first 46 charaters
   
    
    ## Check that state and outcome are valid
    if(!(state %in% df$State)) {
        stop('invalid state')
    }
   
    acceptable <- c('heart attack', 'heart failure', 'pneumonia')
    if(!(outcome %in% acceptable)) {
        stop('invalid outcome')
    }

}