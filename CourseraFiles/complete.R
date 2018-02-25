##https://www.coursera.org/learn/r-programming/supplement/amLgW/programming-assignment-1-instructions-air-pollution

complete <- function(directory, id = 1:332)
{
    id_len <- length(id)
    completeResults <- rep(0, id_len)
    fileName <- rep(0, id_len)
    
    if (file.exists(directory)){
        directory <- paste("./", directory, "/", sep="")
        
        filesInDirectory <- GetAllFiles(directory)
        
        counter <- 1
        
        for(i in id) {
            fixedDir <- paste(directory, filesInDirectory[i], sep="")
            curr_file <- read.csv(fixedDir, header=T, sep=",")
            complete <- subset(curr_file, !is.na(nitrate) & !is.na(sulfate))
            
            completeResults[counter] <- nrow(complete)
            fileName[counter] <- filesInDirectory[i]
            
            counter <- counter + 1
        }
        result <- data.frame(id = id, nobs = completeResults)
        return(result)
    }
}

GetAllFiles <- function(directory){
    as.character(list.files(directory))
}

##CHECK RESULTS HERE:
##https://d3c33hcgiwev3.cloudfront.net/_3b0da118473bfa0845efddcbe29cc336_complete-demo.html?Expires=1506470400&Signature=IUBrgjAWtwSEwEgCRkPIFctxgtTGTFg6pjwaOoFE1eRS1G~A~gYou-nfg-LW-WqgwXIY8zF-Ch5crGB1nnc2e0SqERAUQthaYO~OLJeg7Nb4kmpM3rtSstTBmCYf7Bbvoso9NXG3bfvn9xnT~uHw-AX8fgibehgohj8hIZSmeKI_&Key-Pair-Id=APKAJLTNE6QMUY6HBC5A