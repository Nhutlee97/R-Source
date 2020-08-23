# Chap 1
# Calculate sum, sub, mul, div of 2 number
func_basic_cal <- function(num_1, num_2){
    sum = num_1 + num_2
    sub = num_1 - num_2
    mul = num_1 * num_2
    div = num_1/num_2
    result <- c(sum, sub, mul, div)
    return(result)
}

# Chap 2
# Ex 2.1: Function Calculate BMI
func_BMI_cal <- function(weight, height){
    BMI <- weight/(height^2)
    result <- c(weight, height, BMI)
    return(result)
}

# Ex 2.2: Function calculate food drink bill
func_food_drink_cal <- function(sum_of_food_and_dink, tax, tip){
    tax.m = sum_of_food_and_dink*tax/100
    tip.m = sum_of_food_and_dink*tip/100
    total = sum_of_food_and_dink + tax.m + tip.m
    result <- c(sum_of_food_and_dink, tax.m, tip.m, total)
    return(result)
}

# Ex 2.3: Function Interest calculation on savings (tinh tien lai gui tiet kiem)
func_interest_cal_saving <- function(lai_suat_nam, tien_gui, so_ngay_gui){
    lai_suat_ngay <- lai_suat_nam/365/100
    tien_lai <- (tien_gui*so_ngay_gui)*lai_suat_ngay
    tong_tien <- tien_gui + tien_lai
    result <- c(lai_suat_nam, tien_gui, so_ngay_gui, lai_suat_ngay, tien_lai, tong_tien)
}

# Ex 2.4: Function calculate radius and perimeter of circle
func_cal_R_P_circle <- function(as.integer(S)){
    PI <- 3.14
    R <- sqrt(S/PI)
    P <- 2*PI*sqrt(S/PI)
    result <- c(R, P, S)
    return(result)
}





