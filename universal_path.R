#' Translate network filesystem paths in the SCHARP environment between platforms
#'
#' \code{universalpath()} is designed to work in a computing environment of mixed Windows, Linux, and Mac computers
#' that access shared network file systems mounted in standard ways. This utility function returns, for a given path,
#' what that path would be on a target platform. Most often, users will be 'translating' foreign platform paths to their own.
#' In this case, the user can provide \code{direction = NA} and the function will determine, from the form of the provided path,
#' what the source platform is, and then translate it to the user's current platform. If another use case arises,
#' the user can provide one of the six 'direction' values to force a particular input/output combination.
#' With \code{direction = NA}, the function assumes that the path exists on a currently mounted filesystem
#' on the user's machine and will try to intelligently find it if the default approach does not result in an extant path.
#'
#' \code{universal_path()} assumes that network filesystems are mounted in the standard locations. That is, Mac fileystems are mounted
#' under \code{/Volumes/} from the root of the network samba share, that linux fileystems are mounted under \code{/} (root) and
#' homedirs are mounted under \code{/home/}, and that Windows filesystems use the drive mapping regime available via
#' \code{WinNetworkDriveMap(<ldap id>)}.
#'
#' @param path The source path in need of translation. To avoid ambiguity, provide the full path, but attempts are made to resolve
#' partial paths. However, it is not permitted to use the '~' shortcut for home directories. Please use \code{/home/userid} or its
#' analogies on the other platforms.
#' @param direction NA results in automatic determination of source and target; otherwise, provide one of six directional options.
#' @param ldap If the user's user id on the current machine is the same as the LDAP user ID, use \code{ldap = TRUE}. This is most common
#' scenario. In some cases, Mac users have a different user ID on their local machine than they do on the network. In the latter case,
#' provide your LDAP (network) user ID as the value of \code{ldap}.
#' @param verbose If you'd like just a little more detail about what's happening behind the scenes, try \code{verbose = TRUE}.
#' @export
universalpath <- function(path, direction = c(NA, "mac2win", "win2mac", "win2lin", "lin2win", "mac2lin", "lin2mac"),
                         ldap = TRUE, verbose = FALSE){
  direction <- match.arg(direction)
  mustWork <- is.na(direction) # if we have to derive direction, translated path is on local machine and we will only return a path if it exists
  parsed <- .parsePath(path)
  if(is.na(direction))
    direction <- .deriveDirection(parsed)
    if(is.null(direction))
      return(path)
  func <- paste0(".", direction)
  parsed2 <- eval(call(func, parsed, ldap))
  pathOut <- .unparsePath(parsed2)

  # this might be a homedir, which has a different kind of mapping, so try one more thing
  hhflag <- (direction %in% c("mac2lin", "win2lin") & WinNetworkDriveMap(ldap)[[1]] == parsed2[1]) |
            (direction == "lin2mac" & parsed2[1] == "Volumes" & parsed2[2] == "home")
  if(hhflag)
    pathOut <- .unparsePath(.handleHomedir(parsed2, direction, ldap))

  # initial slash when we need it
  if(direction %in% c("mac2lin", "win2lin", "win2mac", "lin2mac") & substr(pathOut, 1, 1) != "/"){
    pathOut <- paste0("/", pathOut)
  }

  if(verbose) .vb(path, pathOut)

  # if mustWork is FALSE, we can return a non-existent path. Otherwise, if it's not on this machine, return NULL.
  if(dir.exists(pathOut) | file.exists(pathOut) | mustWork == FALSE){
    return(pathOut)
  } else {
    message("My best guess is '", pathOut , "', but no such filesystem is mounted on this machine. If your provided path is correct, there's nothing more I can do.")
    return(invisible(NULL))
  }
}

# ================================================================
#                      PARSING FUNCTIONS
# ================================================================

# take a path, return a character vector of its components
.parsePath <- function(path){
  path <- gsub("[\\]", "/", path) # convert windows paths to something manageable
  parsed <- strsplit(path, "/")[[1]]
  if("~" %in% parsed){
    stop("The 'path' argument cannot contain '~'. Please use the full path to that home directory so the platform can be derived.")
  }
  toDrop <- which(parsed == "")
  for(i in rev(toDrop)){parsed <- parsed[-i]}
  return(parsed)
}


# take a vector of path components, return a string
.unparsePath <- function(parsed){
  path <- paste0(parsed, collapse = "/")
  return(path)
}

# ================================================================
#                      CONVERSION FUNCTIONS
# ================================================================

# convert mac path to linux path (remove /Volumes)
.mac2lin <- function(parsed, ldap = TRUE){
  isMac <- parsed[1] == "Volumes"
  thisfunc <- gsub(".", "", as.character(match.call()[[1]]))
  if(isMac){
    parsed <- parsed[-1]
    parsed[1] <- parsed[1]
    return(parsed)
  } else {
    message(.msg(thisfunc, "Mac"))
    return(NULL)
  }
}

# convert mac path to windows path (remove /Volumes, add e.g. T:)
.mac2win <- function(parsed, ldap = TRUE){
  isMac <- parsed[1] == "Volumes"
  thisfunc <- gsub(".", "", as.character(match.call()[[1]]))
  if(isMac){
    parsed <- parsed[-1]
    if(tolower(parsed[1]) %in% WinNetworkDriveMap(ldap)){
      mappedNetworkDrive <- paste0(names(WinNetworkDriveMap(ldap)[which(WinNetworkDriveMap(ldap)==tolower(parsed[1]))]), ":")
      redundantDirs <- WinNetworkDriveMap(ldap)[which(WinNetworkDriveMap(ldap)==tolower(parsed[1]))][[1]]
      parsed <- c(mappedNetworkDrive, parsed[parsed != redundantDirs])
      return(parsed)
    } else {
      message("I don't know what Windows drive letter to associate with /Volumes/", parsed[1], ". I can't do anything with it.")
      return(NULL)
    }
  } else {
    message(.msg(thisfunc, "Mac"))
    return(NULL)
  }
}

# convert windows path to mac path (remove the letter, add /Volumes and the dir corresponding to the letter)
.win2mac <- function(parsed, ldap = TRUE){
  isWin <- grepl("[A-Za-z]:", parsed[1])
  thisfunc <- gsub(".", "", as.character(match.call()[[1]]))
  if(isWin){
    letterdrive <- gsub(":", "", parsed[1])
    parsed <- parsed[-1]
    if(letterdrive %in% names(WinNetworkDriveMap(ldap))){
      prepend <- c("Volumes", WinNetworkDriveMap(ldap)[letterdrive][[1]])
      parsed <- c(prepend, parsed)
      return(parsed)
    } else {
      message("I've never heard of drive ", letterdrive, ". I can't do anything with that.")
      return(NULL)
    }
  } else {
    message(.msg(thisfunc, "Windows"))
    return(NULL)
  }
}

# convert windows path to linux path (remove the letter, add back the dir associated with it)
.win2lin <- function(parsed, ldap = TRUE){
  isWin <- grepl("[A-Za-z]:", parsed[1])
  thisfunc <- gsub(".", "", as.character(match.call()[[1]]))
  if(isWin){
    letterdrive <- gsub(":", "", parsed[1])
    parsed <- parsed[-1]
    if(letterdrive %in% names(WinNetworkDriveMap(ldap))){
      prepend <- c(WinNetworkDriveMap(ldap)[letterdrive][[1]])
      parsed <- c(prepend, parsed)
      return(parsed)
    } else {
      message("I've never heard of drive ", letterdrive, ". I can't do anything.")
      return(NULL)
    }
  } else {
    message(.msg(thisfunc, "Windows"))
    return(NULL)
  }
}

# convert linux path to mac path (add /Volumes)
.lin2mac <- function(parsed, ldap = TRUE){
  isLin <- parsed[1] %in% c("home", WinNetworkDriveMap(ldap))
  thisfunc <- gsub(".", "", as.character(match.call()[[1]]))
  if(isLin){
    parsed <- c("Volumes", parsed)
    return(parsed)
  } else {
    message(.msg(thisfunc, "Linux"))
    return(NULL)
  }
}

# convert linux path to windows path (Find the leter, add it, drop any redundant dirs)
.lin2win <- function(parsed, ldap = TRUE){
  isLin <- parsed[1] %in% c("home", WinNetworkDriveMap(ldap))
  thisfunc <- gsub(".", "", as.character(match.call()[[1]]))
  if(isLin){
    if(tolower(parsed[1]) %in% WinNetworkDriveMap(ldap)){
      mappedNetworkDrive <- paste0(names(WinNetworkDriveMap(ldap)[which(WinNetworkDriveMap(ldap)==tolower(parsed[1]))]), ":")
      redundantDirs <- WinNetworkDriveMap(ldap)[which(WinNetworkDriveMap(ldap)==tolower(parsed[1]))][[1]]
      parsed <- c(mappedNetworkDrive, parsed[parsed != redundantDirs])
      return(parsed)
    } else if(tolower(parsed[2]) %in% WinNetworkDriveMap(ldap)){
      parsed <- parsed[-1]
      mappedNetworkDrive <- paste0(names(WinNetworkDriveMap(ldap)[which(WinNetworkDriveMap(ldap)==tolower(parsed[1]))]), ":")
      redundantDirs <- WinNetworkDriveMap(ldap)[which(WinNetworkDriveMap(ldap)==tolower(parsed[1]))][[1]]
      parsed <- c(mappedNetworkDrive, parsed[parsed != redundantDirs])
    }
  } else {
    message(.msg(thisfunc, "Linux"))
    return(NULL)
  }
}

# ================================================================
#                  DYNAMIC HANDLING FUNCTIONS
# ================================================================

# if direction arg in scharp_upath() is NA, derive direction arg from the form of the source path and Sys.info() of the target platform
.deriveDirection <- function(parsed){
  if(parsed[1] == "Volumes"){
    inplat <- "mac"
  } else if(grepl("[A-Za-z]:", parsed[1])){
    inplat <- "win"
  } else {
    inplat <- "lin"
  }

  switch(Sys.info()[['sysname']],
    Windows = {outplat = "win"},
    Linux = {outplat = "lin"},
    Darwin = {outplat = "mac"}
  )

  if(inplat == outplat){
    message("It looks like that is a valid path on your machine. Continuing without changes...")
    return(NULL)
  }

  direction <- paste0(inplat, "2", outplat)
  return(direction)
}

# identify all the scharpdata3 shares mounted on the current machine
# NO LONGER USED BUT MIGHT BE USEFUL SO LEAVING IT IN
.mounts <- function(){
  switch(Sys.info()[['sysname']],
         Darwin = {
           mounts <- system2("mount", stdout = TRUE)
           mounts <- gsub(" on (.*) \\(.*\\)", ",\\1", mounts[grepl("scharpdata", mounts)], perl = TRUE)
           mountsV <- sapply(strsplit(mounts, ","), function(x){x[2]})
           names(mountsV) <- gsub("^//.*@scharpdata\\d/", "", sapply(strsplit(mounts, ","), function(x){x[1]}), perl = TRUE)
         },
         Linux = {
           mounts <- system2("df", stdout = TRUE)
           mounts <- gsub("^([\\w.:/]+)\\s+[\\d%\\s]*(.*)", "\\1,\\2", mounts[grepl("scharpdata", mounts)], perl = TRUE)
           mounts <- mounts[!(grepl("apps", mounts) | grepl("admin", mounts) | grepl("mail", mounts))]
           mountsV <- sapply(strsplit(mounts, ","), function(x){x[2]})
           names(mountsV) <- sapply(strsplit(mounts, ","), function(x){gsub("^scharpdata\\d.pc.scharp.org:/[\\w_]+/", "", x[1], perl = TRUE)})
         },
         Windows = {
           mounts <- system2('net', args = 'use', stdout = TRUE)
           mounts <- gsub('^[OK]*\ +([A-Z]:)(\ )+', '\\1', mounts[grepl("scharpdata", mounts)], perl = TRUE)
           mounts <- gsub("[\\]", "/", gsub("\ + Microsoft Windows Network$", "", mounts))
           mounts <- gsub("//scharpdata\\d/", ",", mounts, perl = TRUE)
           mountsV <- sapply(strsplit(mounts, ","), function(x){paste0(x[1], x[2])})
           names(mountsV) <- sapply(strsplit(mounts, ","), function(x){x[2]})
           mountsV <- gsub("([A-Z]:).*", "\\1", mountsV, perl = TRUE)
         }
  )
  return(mountsV)
}

# special logic to map homedirs under certain directions if first pass didn't work
.handleHomedir <- function(parsed, direction, ldap){
  if(direction == "lin2mac" & parsed[2] == "home"){ # we get an extra "home" between "Volumes" and userid that we have to remove
    parsed <- parsed[-2]
  } else if(direction %in% c("mac2lin", "win2lin")){ # homedir mounts under /home/ not under / (root), so have to add "home" as element 1
    if(parsed[1] == WinNetworkDriveMap(ldap)[[1]]){ # discerns userid but takes ldap value when user provides it
      parsed <- c("home", parsed)
    }
  }
  return(parsed)
}

# Standard SCHARP Windows drive mapping schema
WinNetworkDriveMap <- function(ldap){
  if(ldap == TRUE){
    x <- list(
      "H" = .getUserID(),
      "N" = "networks",
      "Q" = "systems",
      "T" = "trials",
      "J" = "scharp",
      "O" = "data",
      "S" = "scratch",
      "W" = "dmz")
  } else if(tryCatch(getUsername(ldap), error = function(e){return(FALSE)}) != FALSE){
    x <- list(
      "H" = ldap,
      "N" = "networks",
      "Q" = "systems",
      "T" = "trials",
      "J" = "scharp",
      "O" = "data",
      "S" = "scratch",
      "W" = "dmz")
  } else {
    stop("The ldap argument should either be TRUE or an LDAP user id. I interpreted '", ldap, "' as a user id, but it doesn't exist on the network. If you're on a Mac, your local machine login may differ from your LDAP ID.")
  }
  return(x)
}

# Annoyingly, Windows and the other two don't have the same environmental variable to hold user ID.
.getUserID <- function(){
  if(Sys.info()[['sysname']]=="Windows"){
    return(Sys.getenv("USERNAME"))
  } else {
    return(Sys.getenv("USER"))
  }
}


# ================================================================
#                      MESSAGING FUNCTIONS
# ================================================================

.msg <- function(direction, platform){
  messagetext <- paste0("scharp_upath() was called with direction argument '", direction,
                        "' but the provided path does not appear to be a ", platform,
                        " path. I can't do anything.")
  return(messagetext)
}


.vb <- function(pathin, pathout){
  message(paste("Path In:", pathin))
  message(paste("Converted Path:", pathout))
  message("The converted path was ", ifelse(dir.exists(pathout) | file.exists(pathout), "found", "not found"), " on this machine.")
}
