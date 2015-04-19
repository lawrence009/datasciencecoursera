library(data.table)

best <- function(state, outcome) {
    #Read outcome data
    url <- 'outcome-of-care-measures.csv'
    head <- read.csv(url, nrows = 1)
    mort <- grep('^Hospital.30.Day.Death..Mortality', colnames(head))
    acceptable <- c('heart attack', 'heart failure', 'pneumonia')
    
    df <- as.data.frame(read.csv(url, colClasses = "character")[, c(2, 7, mort)])
    df[, 3:5] <- sapply(df[, 3:5], as.numeric)
    colnames(df) <- c('hospital', 'state', 'heart.attack','heart.failure', 'pneumonia')
    
    dt <- as.data.table(df)
    #setNames(dt, c('hospital', 'state', 'heart attack', 'heart failure', 'pneumonia'))
  
    #rm(df, mort, outcomes)   
 
    ## Check that state and outcome are valid
    if(!(state %in% dt$state)) {
        stop('invalid state')
    }
    
    if(!(outcome %in% acceptable)) {
        stop('invalid outcome')
    }

    ## Return hospital name in that state with lowest 30-day death rate
    dt[dt$state == state]
}