#Ranking hospitals by outcome in all states

"-------------------------------------------------------------------------------
returns a 2-column data frame containing the hospital in each state that has the
ranking specified in num. For example rankall('heart attack', 'best') would
return a data frame containing the names of the hospitals that are the best in
their respective states for 30-day heart attack death rates. The function should
return a value for every state (some may be NA). The first column in the data
frame is named hospital, which contains the hospital name, and the second column
is named state, which contains the 2-character abbreviation for the state.

Hospitals that do not have data on a particular outcome should be excluded from
the set of hospitals when deciding the rankings.

Handling ties
-------------
The rankall function should handle ties in the 30-day mortality rates in the
same way that the rankhospital function handles ties.
-------------------------------------------------------------------------------"
    

rankall <- function(outcome, num = "best") {
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
    
    
    acceptable <- colnames(df[4:6])
    names(acceptable) <- c('heart attack', 'heart failure', 'pneumonia')
    
    ## Check that outcome param is valid
    if(!(outcome %in% names(acceptable))) {
        stop('invalid outcome') #this is what the test case expect
        #stop('invalid param outcome: ', outcome)
    }
    
    ## For each state, find the hospital of the given rank

    df <- df[, c('Hospital.Name', 'State', acceptable[outcome])]
    
    df <- df[order(df[, 2], df[, 3], df[, 1]), 1:3]
    
    dl <- split(df, df$State)
    
    length.state = length(dl)
    
    df <- data.frame(matrix(nrow = length.state, ncol = 2))
    colnames(df) <- c('hospital', 'state')
    

    
    for (i in 1:length.state) {

        if (num == 'best') {
            j <- 1
        } else if (num == 'worst') {
            dl[[i]] <- dl[[i]][complete.cases(dl[[i]]), ]
                       dl[[i]][complete.cases(dl[[i]]), ]
            j <- nrow(dl[[i]])
        } else if (!is.numeric(num)) {
            stop('invalid outcome')
        }
        
        df$hospital[i] <- dl[[i]]$Hospital.Name[j]
        df$state[i] <- dl[[i]]$State[1]
    }
    
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
    df
    
    #uncomment next line to return full subset for debugging
    #dl
}


#test cases
#tc <- rankall("heart attack", 20)
tc <- rankall("pneumonia", "worst")
#print(head(rankall("heart attack", 20), 10))
#print(tail(rankall("pneumonia", "worst"), 3))
#print(tail(rankall("heart failure"), 10))
