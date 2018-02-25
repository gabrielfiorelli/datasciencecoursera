MyFunction <- function(){
    x <- rnorm(100)
    mean(x)
}

SecondFunction <- function(x){
    x + rnorm(length(x))
}

ForLoopOne <- function(){
    x <- c("a", "b", "c", "d")
    
    for(i in 1:4){
        print(x[i])
    }
}

ForLoopTwo <- function(){
    x <- c("a", "b", "c", "d")
    
    for(i in seq_along(x)){
        print(x[i])
    }
}

ForLoopThree <- function(){
    x <- c("a", "b", "c", "d")
    
    for(letter in x){
        print(letter)
    }
}

WhileLoop <- function(){
    count <- 0
    
    while(count < 10){
        print(count)
        count <- count + 1
    }
}

RepeatLoop <- function(){
    #Repeat and Break Example
    x0 <- 1
    
    repeat{
        if(x0 > 10){
            break
        } else {
            print(x0)
            x0 <- x0 + 1
        }
    }
}

NextExample <- function(){
    for(i in 1:30){
        if(i <= 20){
            #Skip the first 20 iterations
            next
        }
        print(i)
    }
}

add2 <- function(x, y){
    x + y
}

above10 <- function(x){
    use <- x > 10
    #Isto retorna os valores de T/F para os
    #que sao maiores ou menores que 10
    x[use]
    
}

aboveY <- function(x, y){
    use <- x > y
    #Isto retorna os valores de T/F para os
    #que sao maiores ou menores que Y
    x[use]
}

aboveY <- function(x, y = 10){
    #Neste caso existe um valor padrao
    use <- x > y
    #Isto retorna os valores de T/F para os
    #que sao maiores ou menores que Y
    x[use]
}

columnMean <- function(matrix, removeNA = TRUE){
    colCount <- ncol(matrix)
    means <- numeric(colCount)
    for(i in 1:colCount){
        means[i] <- mean(matrix[, i], na.rm = removeNA)
    }
    means
}

#Function returning another Function

make.power <- function(n){
    pow <- function(x){
        x^n
    }
    pow
}