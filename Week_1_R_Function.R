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
func_cal_R_P_circle <- function(S){
    PI <- 3.14
    R <- sqrt(S/PI)
    P <- 2*PI*sqrt(S/PI)
    result <- c(R, P, S)
    return(result)
}

# Ex 3.1: Check number odd or even
func_check_odd_even <- function(n){
    if(n%%2 == 0){
        str_out <- paste(n, "is even number")
    }else{
        str_out <- paste(n, "is odd number")
    }    
    return(str_out)
}

# Ex 3.2: electricity bill for living
tinh_tien_dien_ver2020 <- function(so_Kwh){
    muc_1 <- 1678
    muc_2 <- 1734
    muc_3 <- 2014
    muc_4 <- 2536
    muc_5 <- 2834
    muc_6 <- 2927
    
    bac_50 <- 50
    bac_100 <- 100
    tien_dien <- 0
    if(so_Kwh <= 50){
        tien_dien <- so_Kwh*muc_1
        return(tien_dien)
    }else if(so_Kwh <= 100){
        tien_dien <- bac_50*muc_1 + (so_Kwh-bac_50)*muc_2
        return(tien_dien)
    }else if(so_Kwh <= 200){
        tien_dien <- bac_50*(muc_1 + muc_2) + (so_Kwh-bac_100)*muc_3
        return(tien_dien)
    }else if(so_Kwh <= 300){
        tien_dien <- bac_50*(muc_1 + muc_2) + bac_100*muc_3 + (so_Kwh - bac_50 - bac_50 -bac_100)*muc_4
        return(tien_dien)
    }else if(so_Kwh <= 400){
        tien_dien <- bac_50*(muc_1 + muc_2) + bac_100*(muc_3 + muc_4) + (so_Kwh - bac_50 - bac_50 - bac_100 - bac_100)*muc_5
        return(tien_dien)
    }else{
        tien_dien <- bac_50*(muc_1 + muc_2) + bac_100*(muc_3 + muc_4 + muc_5) + (so_Kwh - bac_50 - bac_50 - bac_100 - bac_100 - bac_100)*muc_6
        return(tien_dien)
    }
}

# Ex 3.4: Calculate hotel bill
# func_cal_room_bill <- function(room_type, date_stay){
#     vip_1 <- 1000000
#     vip_2 <- 900000
#     vip_3 <- 850000
#     standard_1 <- 700000
#     standard_2 <- 550000
    
#     don_gia_phong <- switch(room_type, 
#                            vip_1, 
#                            vip_2, 
#                            vip_3, 
#                            standard_1,
#                            standard_2)
#     tien <- 0
#     if(date_stay <3){
#         tien <- don_gia_phong*date_stay
#     }else if(date_stay <7){
#         tien <- don_gia_phong*date_stay*0.9
#     }else{
#         tien <- don_gia_phong*date_stay*0.8
#     }
#     return(tien)
# }



