complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
        setwd("~/Dropbox/Data_Science/Coursera/Data_Science_02/Assignments/AirPollution/")
        filenames <- list.files(path=directory, pattern="*.csv")
        seq_id <-vector()
        seq_count = vector()
        for(i in id) {
                filename <- sprintf("%03d.csv", i)
                filepath <- paste(directory, filename, sep="/")
                data <- read.csv(filepath)
                seq_id <- c(seq_id, i)
                complete_cases <- data[complete.cases(data),]
                seq_count <- c(seq_count, nrow(complete_cases))
        }
        data.frame(id=seq_id, nobs=seq_count)
        
        #complete("specdata", 1)
        ##   id nobs
        ## 1  1  117
        #complete("specdata", c(2, 4, 8, 10, 12))
        ##   id nobs
        ## 1  2 1041
        ## 2  4  474
        ## 3  8  192
        ## 4 10  148
        ## 5 12   96
        #complete("specdata", 30:25)
        ##   id nobs
        ## 1 30  932
        ## 2 29  711
        ## 3 28  475
        ## 4 27  338
        ## 5 26  586
        ## 6 25  463
        #complete("specdata", 3)
        ##   id nobs
        ## 1  3  243
        
}