###############################################################################
# Functions
###############################################################################


# Helper function for weighted standard deviation
weighted.sd <- function(x, w, na.rm = TRUE) {

    return(Hmisc::wtd.var(x, w, na.rm = na.rm))

}
