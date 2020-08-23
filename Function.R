# Calculate BMI
cal_BMI <- function(height, weight, result){
    BMI <- weight/(height^2)
    result <- BMI
    output <- c(height, weight, result)
    return(output)
}

# Calculate sum_even
sum_even.function <- function(start, end){
    sum_even <- 0
    for(i in start:end){
        if(i%%2 == 0){
            sum_even <- sum_even + i
        }
    }
    return(sum_even)
}