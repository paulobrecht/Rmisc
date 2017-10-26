  #' Get the first and last name of the currently logged-in user
#'
#' @param id The network userid -- Mac users must provide id if their
#' Mac login id is not the same as their LDAP password. Other users
#' will get by with the default value.
#' @examples
#' getUsername("pobrecht")
#' @export
getUsername <- function(id=NULL, ldapServer){

  ldap <- strsplit(ldapServer, ".")[[1]]
  len <- length(ldap)
  dc1 <- ldap[len-1]
  dc2 <- ldap[len]

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
      myargs <- paste0("-x -h ", ldapServer, " -b dc=", dc1, ",dc=", dc2, " uid=", id)
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
      myargs <- paste0("-x -h ", ldapServer, " -b dc=", dc1, ",dc=", dc2, " uid=", id)
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
