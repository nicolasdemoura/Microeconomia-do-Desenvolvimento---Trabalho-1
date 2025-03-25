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

    # Calculate the Gini coefficient
    gini <- 1 - 2*sum(lorenz) / length(lorenz)
    
}

# Function that calculates the 90-10 ratio
ratio_90_10 <- function(x, w) {
    
    # Calculate the 90-10 ratio
    ratio <- Hmisc::wtd.quantile(x, w, 0.9) / Hmisc::wtd.quantile(x, w, 0.1)

    return(ratio)
}


############
# Poverty
############

# Function that calculates the P-alpha poverty measure

P_alpha <- function(x, w, alpha, z) {
    dm_poor <- ifelse(x <= z, 1, 0)
    # Calculate the P-alpha poverty measure
    P_alpha <- sum(w * ((z-x)/z)^alpha * dm_poor) / sum(w)
    
    return(P_alpha)
    
}
