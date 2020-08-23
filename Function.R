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

# Function drink
drink.function <- function(price, type = "Tea"){
    print(paste("With", price, ", you can drink",type))
}

# Function tinh_tien_dien
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