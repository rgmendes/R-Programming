best <- function(state,outcome) {
        
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Format outcome data with default values of outcome
        outcomes = c("heart attack", "heart failure", "pneumonia")
        data <- data[c(2,7,11,17,23)]
        names(data)[3] <- outcomes[1]
        names(data)[4] <- outcomes[2]
        names(data)[5] <- outcomes[3]
        
        ## Check if state and outcome are valid
        if ((is.element(state,(levels(factor(data$State))))) == FALSE) {
                stop("invalid State")
        }
        else if ((is.element(outcome,(levels(factor(outcomes))))) == FALSE) {
                stop("invalid outcome")
        }
        
        ## Return values without "Not Available" of the outcome
        data_state <- data[data$State == state & data[,outcome] != "Not Available",]
        
        ## Sort by outcome and State
        order_state <- data_state[order(as.numeric(data_state[,outcome]),data_state$Hospital.Name),]
        
        ## Return the name of the hospital with the lowest value of outcome
        # Another possible answer: return(order_state[which.min(data_state[,outcome]),]$Hospital.Name)
        return(order_state[1,1])
}