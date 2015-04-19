pollutantmean <- function(directory, pollutant, id = 1:332) {
    # 'directory' is a character vector of length 1 indicating
    # the location of the CSV files
    
    # 'pollutant' is a character vector of length 1 indicating
    # the name of the pollutant for which we will calculate the
    # mean; either "sulfate" or "nitrate".
    
    # 'id' is an integer vector indicating the monitor ID numbers
    # to be used
    
    monitors <- list.files(directory, full.name=T)
    
    ppm <- NA  #is there another way to write this without first assigning NA
    
    for(i in id) {
        ppm <- c(ppm, read.csv(monitors[i])[, pollutant])
    }
    
    # Return the mean of the pollutant across all monitors list
    # in the 'id' vector (ignoring NA values)
    mean(ppm, na.rm=T)
}