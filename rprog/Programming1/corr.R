corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0

    obs.cor <- NULL    
    case <- complete(directory)
    monitor <- case[case$nobs > threshold, "id"]
    
    #print(length(monitor))
 
    if(length(monitor) > 0) {
        for(i in monitor) {
            filename <- sprintf("%s/%03d.csv", directory, i)
            con <- file(filename, 'r')
            data <- read.csv(con)
            close(con)
            
            obs.cor <- c(obs.cor, cor(data[, 2], data[, 3], use = "complete.obs"))
        }    
    } else {
        obs.cor <- vector(mode = "numeric")
    }

    
    ## Return a numeric vector of correlations
    obs.cor
}

## test cases
# cr <- corr("specdata", 150)
# print(head(cr))
# print(summary(cr))
# 
# cr <- corr("specdata", 400)
# print(head(cr))
# print(summary(cr))
# 
# cr <- corr("specdata", 5000)
# print(summary(cr))
# print(length(cr))
# 
# cr <- corr("specdata")
# print(summary(cr))
# print(length(cr))