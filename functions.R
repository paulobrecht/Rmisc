# FUNCTION: dataProfile()
# describe data (variable list, # rows, # unique non-missing values, # missing values, # blank strings)
dataProfile <- function(df){
  dat <- data.frame(Var = names(df), stringsAsFactors=FALSE)
  dat$varClass <- lapply(X=df, function(x) class(x))
  dat$NumRows <- lapply(X=df, function(x) length(x))
  dat$uniqVals <- lapply(X=df, function(x) length(which(!is.na(unique((x))))))
  dat$NumNAs <- lapply(X=df, function(x) length(which(is.na(x))))
  dat$NumBlankStr <- lapply(X=df, function(x){length(which(x==""))})
  return(dat)
}

# FUNCTION: freq()
# basic frequency by summaryVars with one column per value of targetVar
# requires reshape2 and plyr
freq <- function(dat, summaryVars, targetVar){
  k <- dcast(melt(dat,
                  id.vars=summaryVars,
                  measure.vars=targetVar),
             list(summaryVars, .(value)),
             length)
  names(k) <- make.names(names(k))
  return(k)
}


# FUNCTION: findDups()
# identify combinations of variables (inVar, given as char vector)
# that are duplicated in a given data frame
findDups <- function(dat, inVar){
  dups <- unique(dat[duplicated(dat[,inVar]),])[,inVar]
  if(nrow(dups)>0){
    dups <- merge(dat, dups, by=inVar)
    return(dups)
  } else {
    return()
  }
}


# FUNCTION englist()
# Looks at the unique values of some variable, lists them as text, and inserts "and" before the last one
# inputs:
#  df (name of input data.frame or data.table),
#  var (name of var to summarize),
#  varnice (optional text to prepend to numeric vars, e.g. "Week" to turn 4 into "Week 4")
#  excl (vector of values to exclude from list)
englist <- function(df, var, varnice="", excl){
  pars <- as.list(match.call()[-1])
  lst <- paste0(varnice, unique(eval(pars$df)[, as.character(pars$var)]))
  lst <- lst[!lst %in% excl]
  len <- length(lst)
  lst[len] <- paste("and", lst[len])
  return(paste(lst, collapse=", "))
}


