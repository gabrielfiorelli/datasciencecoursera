##https://www.coursera.org/learn/r-programming/supplement/amLgW/programming-assignment-1-instructions-air-pollution

corr <- function(directory, threshold = 0)
{
    tcorr <- function(fname) {
        data <- read.csv(file.path(directory, fname))
        nobs <- sum(complete.cases(data))
        if (nobs > threshold) {
            return (cor(data$nitrate, data$sulfate, use="complete.obs"))
        }
    }
    tcorrs <- sapply(list.files(directory), tcorr) #get all correlations + NULLs
    tcorrs <- unlist(tcorrs[!sapply(tcorrs, is.null)]) #remove NULLs
    return (tcorrs)
}

GetAllFiles <- function(directory){
    as.character(list.files(directory))
}

##CHECK RESULTS HERE:
##https://d3c33hcgiwev3.cloudfront.net/_e92e575b8e62dcb1e3a086d2ff0d5a1e_corr-demo.html?Expires=1506470400&Signature=jqXbptUKg4q3mIPm66KP1p~e-gSXxThfDdoFcg5LbKzbkv63UOBGTCF73Vq-z7jyyLoIXLT9tv-KgVROA6aAK2vJXqVwiYvJKWtq8f41OFuyJLKQIeaXkStP86EPlAV2Oma-CZCFAaoDzqg2wv4SniEn3cNRKlq4iW3qojLHLoE_&Key-Pair-Id=APKAJLTNE6QMUY6HBC5A