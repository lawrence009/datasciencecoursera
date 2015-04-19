complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    nobs <- NULL
    
    for(i in id) {
        filename <- sprintf("%s/%03d.csv", directory, i)
        con <- file(filename, 'r')
        data  <- read.csv(con)
        close(con)
        
        good <- complete.cases(data[, 2], data[, 3])
        nobs <- c(nobs, nrow(data[good,]))        
    }
    
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    data.frame(id, nobs)
}

# test cases
# df1 <- complete('specdata', 1)
# df2 <- complete('specdata', c(2, 4, 8, 10, 12))
# df3 <- complete('specdata', 30:25)
# df4 <- complete('specdata', 3)
# 
# print(df1)
# print(df2)
# print(df3)
# print(df4)