###############################################################################
# Functions
###############################################################################

# Helper function for weighted standard deviation
weighted.sd <- function(x, w, na.rm = TRUE) {

    return(Hmisc::wtd.var(x, w, na.rm = na.rm))

}

############
# Inequality
############

# Function that calculates the Gini coefficient
gini <- function(x, w) {
    lorenz <- cumsum(x * w) / sum(x * w)
    w <- w/sum(w)
    # Calculate the Gini coefficient
    gini <- round(1 - 2*sum(lorenz*w),3)
    return(gini)
}

# Function that calculates the lambda-(1-lambda) ratio
ratio <- function(x, w, lambda) {
    
    # Calculate the 80-20 ratio
    ratio <- round(Hmisc::wtd.quantile(x, w, lambda) / Hmisc::wtd.quantile(x, w, 1-lambda),3)

    return(ratio)
}


############
# Poverty
############

# Function that calculates the P-alpha poverty measure

P_alpha <- function(x, w, alpha, z) {
    dm_poor <- ifelse(x <= z, 1, 0)
    # Calculate the P-alpha poverty measure
    P_alpha <- round(sum(w * ((z-x)/z)^alpha * dm_poor) / sum(w),3)
    
    return(P_alpha)
    
}
