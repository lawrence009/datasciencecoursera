library(data.table)

best <- function(state, outcome) {
    #Read outcome data
    url <- 'outcome-of-care-measures.csv'
    colheads <- as.character(read.csv(url, header = F, nrows = 1, stringsAsFactors = F))
    
    #which columns contain 30-Day Death
    mort <- grep('^Hospital 30-Day Death \\(Mortality', colheads)
    
    df <- as.data.frame(read.csv(url, colClasses = "character"))
    df <- df[, c(1, 2, 7, mort)] #(1)provider number, (2)hopspital name, (7)state
    df[, 4:6] <- sapply(df[, 4:6], as.numeric)
    colnames(df) <- c('provider_num', 'hospital', 'state', 'heart.attack','heart.failure', 'pneumonia')
    
    dt <- as.data.table(df)
    #setNames(dt, c('hospital', 'state', 'heart attack', 'heart failure', 'pneumonia'))
  
    #rm(df, mort, outcomes)   
    
    acceptable <- c('heart attack', 'heart failure', 'pneumonia')
    
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