# FUNCTION WilsonScoreLimit
# Computes lower and upper CI limit using Wilson score method
# Similar to PropCIs::scoreci(), but returns a vector rather than a list containing a vector.
# Also, does not round result as scoreci() does. And has fewer duplicated calculations.
# Arguments:
#    n (# in cohort)
#    npos (number of positive responders)
#    ci (confidence interval as integer, e.g., 95 for 95% CI)
# Returns: a length-2 vector representing Wilson's score confidence interval limits as c(lower, upper)
WilsonScoreLim <- function(n, npos, ci=95){
  z <- qnorm(1 - (100-ci)/100/2)
  p <- npos/n
  denom <- 1+z*z/n
  t1 <- p + z*z/2/n
  t2 <- z*sqrt(p*(1-p)/n + z*z/4/n/n)
  return(c((t1 - t2)/denom, (t1 + t2)/denom))
}


# FUNCTION fmtPCT
# format percent string for output tables
# Arguments:
#    n (# in cohort)
#    npos (number of positive responders)
# Returns: Percent positive string, e.g., "8/14 = 57.14%"
fmtPCT <- function(n, npos){paste0(npos, "/", n, " = ", round(npos*100/n,2),"%")}

# FUNCTION fmtCI
# format CI string for output tables
# Arguments:
#    lcl (lower confidence interval limit)
#    ucl (upper confidence interval limit)
# Returns: CI string, e.g., "(32.6%, 78.6%)"
fmtCI <- function(lcl, ucl){paste0("(", round(lcl*100,1), "%, ", round(ucl*100,1), "%)")}


# FUNCTION  prtDT
# data.table implementation to compute response rates and CIs, formatted for report PDFs
# Arguments:
#    dt (name of input data.table that includes keyVars and responseVar variable)
#    assumes a variable "response is in the data.table -- should make variable name an argument instead
#    keyVars (summary ("by") variables, provided as a character vector
# Returns: data.table that includes keyVars, string of response counts, rates, and confidence intervals
prtDT <- function(dt, keyVars){
  dt[, {n = sum(!is.na(response))
        npos = sum(response, na.rm = TRUE)
        lcl = WilsonScoreLim(n, npos)[1] # could use alternate CI method function here
        lcl[lcl<0] <- 0                  # truncate at 0, should make this an argument-controlled option.
        ucl = WilsonScoreLim(n, npos)[2] # could use alternate CI method function here
        percent = fmtPCT(n, npos)
        CI = fmtCI(lcl, ucl)
        list(percent=percent, CI=CI)}, # this list plus keyVars is what's returned
     by = keyVars]
}
