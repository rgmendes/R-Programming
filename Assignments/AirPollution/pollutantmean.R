pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        ## NOTE: Do not round the result!
        ## Getting a list of filenames
        setwd("~/Dropbox/Data_Science/Coursera/Data_Science_02/Assignments/AirPollution")
        filenames <- list.files(path=directory, pattern="*.csv")
        values <- vector()
        for(i in id) { 
                filename <- sprintf("%03d.csv", i)
                filepath <- paste(directory, filename, sep="/")
                attr <- read.csv(filepath)
                columns <- attr[,pollutant]
                columns <- columns[!is.na(columns)]
                values <- c(values, columns)
        }
        round(mean(values), 3)
        ### VALIDATION ###
        #pollutantmean("specdata", "sulfate", 1:10)
        ## [1] 4.064
        #pollutantmean("specdata", "nitrate", 70:72)
        ## [1] 1.706
        #pollutantmean("specdata", "nitrate", 23)
        ## [1] 1.281
}