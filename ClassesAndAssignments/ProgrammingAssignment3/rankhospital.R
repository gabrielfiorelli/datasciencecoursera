rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    if(!state %in% data$State){
        stop("Invalid state!")
    }
    
    index <- 0
    if(outcome == "heart attack"){
        index <- 11
    } else if (outcome == "heart failure"){
        index <- 17
    } else if (outcome == "pneumonia"){
        index <- 23
    } else {
        stop("Invalid outcome!")
    }
    
    data[,index] <- suppressWarnings(as.numeric(data[,index]))
    data <- na.omit(data)
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    
    dataSubset <- subset(data, State==state)
    dataSubset <- dataSubset[order(dataSubset[,index], dataSubset[,2], na.last=TRUE),2] #this is ordered by rate
    dataSubset <- na.omit(dataSubset)
    
    rank <- 0
    if(num == "best"){
        rank <- 1
    } else if(num == "worst"){
        rank <- length(dataSubset)
    } else {
        rank <- as.numeric(num)
    }
    
    return(dataSubset[rank])
    
}
