rankall <- function(outcome, num = "best") {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Format outcome data with default values of outcome
        outcomes = c("heart attack", "heart failure", "pneumonia")
        data <- data[c(2,7,11,17,23)]
        names(data)[3] <- outcomes[1]
        names(data)[4] <- outcomes[2]
        names(data)[5] <- outcomes[3]
        
        ## Check if outcome is valid
        if ((is.element(outcome,(levels(factor(outcomes))))) == FALSE) {stop("invalid outcome")}
        
        ## Return values without "Not Available" of the outcome
        data_outcome <- data[data[,outcome] != "Not Available",]
        
        ## Sort by outcome and State
        data_order <- data_outcome[order(as.numeric(data_outcome[,outcome]),data_outcome$Hospital.Name),]
        
        ## Split data_ordem by State
        data_split <- split(data_order, data_order$State)
        
        ## Test the position number and returns the name of hospital
        data_result <- lapply(data_split, function(x, num) {
                #
                # "x = x[order(x[,outcome], x.State),]"
                #
                # Alternatively to replace the command: ## Sort by outcome and State
                if(class(num) == "character") {
                        if(num == "best") {
                                return (x$Hospital.Name[1])
                        }
                        else if(num == "worst") {
                                return (x$Hospital.Name[nrow(x)])
                        }
                }
                else {
                        return (x$Hospital.Name[num])
                }
        }, num)
        
        ## Return result in Data Frame
        return (data.frame(hospital = unlist(data_result), state = names(data_result)))
}