best <- function(state, outcome){
    
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    if( outcome !=  "heart attack" && outcome != "heart failure" 
        && outcome != "pneumonia"){
        stop("Invalid outcome!")
    }
    
    if(!state %in% data$State){
        stop("invalid state")
    }
    
    index <- 0
    if(outcome == "heart attack"){
        index <- 11
    } else if (outcome == "heart failure"){
        index <- 17
    } else {
        index <- 23
    }
    
    # change data type from character to numeric
    data[,index] <- suppressWarnings(as.numeric(data[,index]))
    data <- na.omit(data)
    
    dataSubset <- subset(data, State==state)
    dataSubset <- dataSubset[order(dataSubset[,index], na.last = TRUE), 2]
    dataSubset <- na.omit(dataSubset)
    
    return(dataSubset[1])
}

# tests
#best("TX", "heart attack")
#best("TX", "heart failure")
#best("MD", "heart attack")
#best("MD", "pneumonia")
#best("BB", "heart attack")