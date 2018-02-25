##https://www.coursera.org/learn/r-programming/supplement/amLgW/programming-assignment-1-instructions-air-pollution

pollutantmean <- function(directory, pollutant, id = 1:332)
{
    if (file.exists(directory)){
        directory <- paste("./", directory, "/", sep="")
        
        mean_polldata <- c()
        filesInDirectory <- GetAllFiles(directory)
        
        for(i in id) {
            curr_file <- read.csv(paste(directory, filesInDirectory[i], sep=""), header=T, sep=",")
            remove_na <- curr_file[!is.na(curr_file[, pollutant]), pollutant]
            mean_polldata <- c(mean_polldata, remove_na)
        }
        
        pollutantMean <- mean(mean_polldata)
        return(round(pollutantMean, 3))
    }
}

GetAllFiles <- function(directory){
    as.character(list.files(directory))
}

##CHECK RESULTS HERE:
##https://d3c33hcgiwev3.cloudfront.net/_3b0da118473bfa0845efddcbe29cc336_pollutantmean-demo.html?Expires=1506470400&Signature=LVhKD6mcpfjsy5v0cfWNWB9bQPU1O2jgOJ0kWnq1rHeRvR6EohDokh7-3eEJvfPavyh~Lbxp5ipCHhBJVoH-1TLMHe-Dbtc2D-ZjAnKl7Rcv2mDJ9lm8CcplGVNdkh8-2zfDN7nk1ggc5NmMJuf2~ogxrDdok78W7ImuWwIlGuQ_&Key-Pair-Id=APKAJLTNE6QMUY6HBC5A