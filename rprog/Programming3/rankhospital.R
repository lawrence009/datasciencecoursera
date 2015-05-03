#Ranking hospitals by outcome in a state


rankhospital <- function(state, outcome, num = "best") {

    #read outcome data
    url <- 'outcome-of-care-measures.csv'
    
    #which columns contain 30-Day Death (mortality) rate
    colheads <- as.character(read.csv(url, header = F, nrows = 1, stringsAsFactors = F))
    mort <- grep('^Hospital 30-Day Death \\(Mortality', colheads)
    
    #read full dataset, subset and transform
    df <- read.csv(url, colClasses = 'character')
    df <- df[, c(1, 2, 7, mort)] #(1)provider number, (2)hopspital name, (7)state
    df[, 4:6] <- suppressWarnings(sapply(df[, 4:6], as.numeric))
    colnames(df)[4:6] <- substring(colnames(df)[4:6], first = 46) #remove the first 46 charaters
    
    
    #check that state param is valid
    if(!(state %in% df$State)) {
        stop('invalid param  state: ', state)
    }
    
    acceptable <- colnames(df[4:6])
    names(acceptable) <- c('heart attack', 'heart failure', 'pneumonia')
    
    ## Check that outcome param is valid
    if(!(outcome %in% names(acceptable))) {
        stop('invalid param outcome: ', outcome)
    }
    
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    
    NA
}