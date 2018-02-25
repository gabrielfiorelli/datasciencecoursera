rankall <- function(outcome, num = "best") {

    # 2 = Hospital Name
    # 7 = State
    # 11 = heart attack 30-day mortality rate
    # 17 = heart failure 30-day mortality rate
    # 23 = pneumonia  30-day mortality rate

    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    #data = data[,c(2, 7, 11, 17, 23)]

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
    data <- data[!is.na(data[,index]),]

    data.sorted <- data[order(data[,index], data[,2], na.last=TRUE),]

    data.sorted <- data.sorted[!is.na(data.sorted[,index]),]

    if(num == "best"){
        rank <- 1
    } else if(num == "worst"){
        rank <- length(data.sorted)
    } else if(num > length(data.sorted)){
        rank <- NA
    } else {
        rank <- as.numeric(num)
    }

    stateInitialsColumn <- 7
    states <- sort(unique(data.sorted[,stateInitialsColumn]))

    state_hospital_data <- function(state) {
        slice <- subset(data.sorted, State==state)
        slice <- slice[rank, c(2,7,index)]
        slice$State <- state
        return (slice)
    }

    state_data <- lapply(states, state_hospital_data)
    dframe <- as.data.frame(do.call(rbind, state_data), row.names=states)
    colnames(dframe) <- c("hospital", "state")
    return (dframe)
}

#ESTE ESTA CERTO: 

# 
# rankall <- function(outcome, num = "best") {
#     ## Read outcome data
#     hospital.data = read.csv("outcome-of-care-measures.csv",
#                              header = TRUE,
#                              colClasses = "character",
#                              na.strings = "Not Available")
#     hospital.data = hospital.data[,c(2, 7, 11, 17, 23)]
#     ## Check outcome is valid
#     if (sum(outcome %in% c("heart attack", "heart failure", "pneumonia")) == 0) {
#         stop("invalid outcome")
#     }
#     ## For each state, find the hospital of the given rank
#     ## Return a data frame with the hospital names and the
#     ## (abbreviated) state name
#     index = match(outcome, c("heart attack", "heart failure", "pneumonia"))
#     hosp.data = hospital.data[,c(1:2, (2 + index))]
#     hosp.data = hosp.data[order(hosp.data$Hospital.Name),]
#     state = sort(unique(hosp.data$State))
#     data.l = list()
#     for (i in 1:length(state)) {
#         data.l[[i]] = subset(hosp.data, State == state[i])
#     }
#     ranking = function(x) {
#         Ranks = rank(as.numeric(x[,3]), na.last = "keep", ties.method = "first")
#         x = data.frame(x, Rank = Ranks)
#     }
#     data.l = lapply(data.l, ranking)
#     if (num == "best") {
#         num = 1
#         hospitals = unlist(lapply(data.l, function(x) {x$Hospital.Name[match(num, x$Rank)]}))
#         data.frame(hospital = hospitals, state = state)
#     } else if (num == "worst") {
#         hospitals = unlist(lapply(data.l, function(x) {x$Hospital.Name[match(max(x$Rank, na.rm = TRUE), x$Rank)]}))
#         data.frame(hospital = hospitals, state = state)
#     } else if (is.numeric(num) == TRUE & num < max(unlist(lapply(data.l, function(x) {max(x$Rank, na.rm = TRUE)})))) {
#         hospitals = unlist(lapply(data.l, function(x) {x$Hospital.Name[match(num, x$Rank)]}))
#         data.frame(hospital = hospitals, state = state)
#     } else print("NA")
# }



##https://github.com/ahawker/data-analysis-coursera/blob/master/HW3/rankall.r