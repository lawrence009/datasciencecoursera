#Ranking hospitals by outcome in a state

"-------------------------------------------------------------------------------
Return a character vector containing the name of the hospital with the 5th
lowest 30-day death rate for heart failure. The num argument can take values
best, worst, or an integer indicating the ranking (smaller numbers are better).
If the number given by num is larger than the number of hospitals in that
state, then the function should return NA. Hospitals that do not have data on
a particular outcome should be excluded from the set of hospitals when deciding
the rankings.

Handling ties
-------------
It may occur that multiple hospitals have the same 30-day mortality rate for a
given cause of death. In those cases ties should be broken by using the hospital
name, i.e. order by Rate, then Hospital.Name
-------------------------------------------------------------------------------"

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
        stop('invalid state') #this is what the test case expect
#         stop('invalid param state: ', state)
    }
    
    acceptable <- colnames(df[4:6])
    names(acceptable) <- c('heart attack', 'heart failure', 'pneumonia')
    
    ## Check that outcome param is valid
    if(!(outcome %in% names(acceptable))) {
        stop('invalid outcome') #this is what the test case expect
#         stop('invalid param outcome: ', outcome)
    }
    
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    
    df <- df[df$State == state, c('Hospital.Name', 'State', acceptable[outcome])]
    df <- df[complete.cases(df), ]
    
    df <- df[order(df[, 3], df[, 1]), 1:3]
    df$Rank <- rank(df[, 3], ties.method = 'first') #not necessary for the assigment; extra credit; also for validating result
    
    if (num == 'best') {
        num <- 1
    } else if (num == 'worst') {
        num  <- nrow(df)
    } else if (!is.numeric(num)) {
        warning('invalid ranking; best is assumed')
    }
    
    df[num, 1]

    #uncomment next line to return full subset for debugging
    #df
    
}

#test case
#print(rankhospital("TX", "heart failure", 4))

#rankhospital("NY", "heart attak", 7) #typo in the submit script