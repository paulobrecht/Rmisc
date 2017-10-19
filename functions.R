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


# FUNCTION englist()
# Looks at the unique values of some variable, lists them as text, and inserts "and" before the last one
# inputs:
#  df (name of ics data.table),
#  var (name of var to summarize),
#  varnice (optional text to prepend to numeric vars, e.g. "Week" to turn 4 into "Week 4")
englist <- function(df, var, varnice="", excl="NEG", oxford=TRUE){
  pars <- as.list(match.call()[-1])
	lst <- unique(df[[eval(quote(var))]])
  lst <- paste0(varnice, lst)
  lst <- lst[lst != excl]
  len <- length(lst)
	if(len>1){
		lst[len] <- paste("and", lst[len])
	}
	if(oxford==TRUE){
	  return(paste(lst, collapse=", "))
	} else {
		return(paste(c(paste(lst[1:len-1], collapse=", "), lst[len]), collapse=" "))
	}
}

# FUNCTION: getUsername()
# from user's login name (platform-specific determination), get user's full name from LDAP or from `net user /domain`
getUsername <- function(id=NULL){
  switch(Sys.info()[['sysname']],
         Windows = {
           if(is.null(id)){id <- Sys.getenv("USERNAME")}
           myargs <- paste("user /domain", id)
           user <- tryCatch({system2(command="net", args=myargs, stdout=TRUE)},
                            warning=function(w){NULL},
                            error=function(e){NULL})
           user <- gsub("Full Name\ *", "", user[grepl("FULL NAME", toupper(user))])
           if(length(user)>0){
             user <- paste(strsplit(gsub(",", "", user), " ")[[1]][c(2, 1)], collapse=" ")
           } else {
             stop("No userid ", id, " was found.")
           }
         },
         Linux   = {
           if(is.null(id)){id <- Sys.getenv("USER")}
           myargs <- paste0("-x -h ldap.your_org.com -b dc=your_org,dc=com uid=", id) # update here
           user <- system2("ldapsearch", args = myargs, stdout=TRUE)
           user <- user[grep("cn:", user)]
           if(length(user)>0){
             user <- gsub("[a-z]+: ", "", user)
           } else {
             stop("No userid ", id, " was found.")
           }
         },
         Darwin  = {
           if(is.null(id)){id <- Sys.getenv("USER")}
           myargs <- paste0("-x -h ldap.your_org.com -b dc=your_org,dc=com uid=", id) # update here
           user <- system2("ldapsearch", args = myargs, stdout=TRUE)
           user <- user[grep("cn:", user)]
           if(length(user)>0){
             user <- gsub("[a-z]+: ", "", user)
           } else {
             stop("No userid ", id, " was found.")
           }
         }
  )

  return(user)
}

                       
                       
                       
#' Change a vector to Latex-friendly values
#'
#' @param x A character vector.
#' @examples
#' x = c("antigen_1", "antigen_2", "antigen_3")
#' latexify(x)
#' y = c("46.2%", "95.4%", "99.9%")
#' latexify(y)
#' z = c("CD4", "CD8", "IL2", "IFNg", "TNFa")
#' latexify(z)
#' @export
latexify <- function(x){
  if(!is.character(x)){
    stop("Argument to latexify() must be a character vector.")
  }

  x <- gsub("CD([348])", "CD\\1${}^{+}$", x)
  x <- gsub("IFNg", "IFN$\\gamma$", x, fixed = TRUE)
  x <- gsub("TNFa", "TNF$\\alpha$", x, fixed = TRUE)
  x <- gsub("_", "\\_", x, fixed = TRUE)
  x <- gsub("%", "\\%", x, fixed = TRUE)
  return(x)
}

                       
                       
#' Purl a Rmd/Rnw file and execute
#'
#' makeLog() is intended to be executed by itself inside an R chunk in the
#' last 3 lines of an Rmd/Rnw file. It purls the R chunks from the document
#' currently being knitted into a file called tmp.R. It then uses
#' \code{\link{system2()}} to copy all but the last 3 lines of tmp.R (the
#' lines containing the R chunk) to an .R file with the same base name as the
#' document currently being knitted. The function then executes RMD CMD BATCH
#' on the resulting .R file to create an .Rout file, and then deletes tmp.R.
#'
#' The function works only in Linux at the present.
#'
#' @param inFile The name of the Rmd/Rnw file currently being knitted.
#' Default value is taken from knitr:::knit_concord$get("infile").
#' @param Rpath The path to the R executable of your choice.
#' Defaults to SCHARP location for latest R 3.x release
#' @param docLevel The level of documentation to include in the .R file. The
#' values follow those of the \code{documentation} argument to knitr::purl().
#' The function calls knitr::knit() for docLevel=1 and knitr::purl() for
#' docLevel=0 or 2.
#' @param lines The number of lines to truncate from the end of the knitr/rmarkdown document.
#' This allows for the last N lines to be the R chunk that contains the makeLog() call.
#' @param exec TRUE/FALSE: Execute the .R script? If FALSE, the function is essentially a purl wrapper.
#' @return The function performs operations and returns NULL in all cases,
#' even on success.
#' @examples
#' #```{r RmdChunk, eval=TRUE, echo=FALSE, results="hide"}
#' #  makeLog()
#' #```
#'
#' #<<RnwChunk, eval=TRUE, echo=FALSE, results="hide">>=
#' #  makeLog()
#' #@
#' @export
makeLog <- function(inFile = knitr::current_input(),
                    Rpath = file.path(R.home("bin"),"R"),
                    docLevel = 2,
                    lines = 3,
                    exec = TRUE)
{
  options(knitr.duplicate.label = "allow")
  outfn <- paste0(tools::file_path_sans_ext(inFile), ".R")

  p1 <- sample(LETTERS, 14, replace=TRUE)
  p2 <- gsub("[-: ]", "", paste0(Sys.time(), Sys.getpid(), ".R"))
  tmpfile <- paste(c(p1, "_", p2), collapse="")

  message("Knitting input file to temp file")
  if(docLevel %in% c(0, 2)){
    knitr::purl(input=inFile, output=tmpfile, documentation=docLevel, quiet=TRUE)
  } else {
    knitr::knit(input=inFile, output=tmpfile, tangle=TRUE, quiet=TRUE)
  }

  message(paste0("Copying temp file to ", outfn))
  argstring <- paste0("-$((`cat ", tmpfile, " | wc -l` - ", lines, "))")
  system2("head", args = argstring, stdout = outfn, stdin = tmpfile)

  if(exec){
    message(paste0("Executing ", outfn))
    system2(Rpath, args = c("CMD BATCH", outfn))
  }

  message("Removing temp file")
  system2("rm", args = tmpfile)
  return(NULL)
}
