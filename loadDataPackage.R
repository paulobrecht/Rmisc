#' Install an R package from a particular commit to a local filesystem git repo
#'
#' This function is required because there is no way to pass a particular commit to \code{devtools::install_git()}
#' the same way you can pass the \code{ref} argument to \code{devtools::install_github()}. The package
#' clones the the provided package to a temp directory (either user-specified or a default random-string folder
#' in the user's home directory). If a hash has been provided, that package is installed from that point in the
#' version history. Otherwise, the most recent commit is installed.
#'
#' @param package The name of an R package. Specifically, the portion of the git repo name without ".git".
#' @param path The full path to the git repo on the local filesystem
#' @param hash The hash of the particular commit you wish to install. If null, the most recent commit.
#' @param tmpdir By default the function creates a temporary directory in the user's homedir and clones
#' the repo there. Provide an alternate location with this argument.
#'
install_git_visc <- function(package, path, hash, tmp){
  package <- gsub("/$", "", gsub("\\.git$", "", package)) # strip .git if provided
  path <- gsub("/$", "", path) # strip trailing slash
  path <- normalizePath(path)
  if(!dir.exists(path)){stop("The provided path does not exist.", call.=FALSE)}
  if(length(path)>1 | length(package)>1){stop("Path and package must have length 1.", call.=FALSE)}
  if(!dir.exists(paste0(file.path(path, package), ".git"))){stop("The provided package does not exist in the provided path.", call.=FALSE)}

  if(!dir.exists(tmp)){system2(command = "mkdir", args = tmp)} # mkdir is both Windows and Unix-alike
  setwd(tmp)

  # create temp repo, pull remote, checkout hash, hard reset, install
  system2(command = "git", args = "init")
  system2(command = "git", args = paste0("remote add origin ", paste0(file.path(path, package), ".git")))
  message("Cloning ", paste0(file.path(path, package), ".git ..."))
  system2(command = "git", args = "pull origin master")

  if(!is.null(hash)){
    checkHash <- system2("git", args=c("rev-parse --quiet --verify", hash), stdout=TRUE)
    if(length(checkHash) == 1){
      system2(command = "git", args = paste("checkout", hash))
      system2(command = "git", args = "reset --hard")
    } else {
      stop("The provided hash does not exist in the git repository located at ", paste0(file.path(path, package), ".git", call.=FALSE))
    }
  }
  setwd(file.path(getwd(), ".."))
  devtools::install_local(tmp, local = FALSE, upgrade_dependencies=FALSE)
}


#' Install and/or load a given data package from github, a git repo, or a local directory
#'
#' This function attempts to be a flexible data package loading tool.
#'
#' @param package The name of an R package.
#' With type='github', the name of the repo (without the prepended organization name).
#' With type='git', the name of the git remote repo on the local filesystem (omit ".git" if part of the folder name).
#' With type='local', the name of the package subdirectory. Does not work with .tar.gz files.
#' @param install FALSE by default. Set to TRUE if you wish to install the package rather than loading
#' the currently-installed version via \code{library()}.
#' @param type Takes values "github", "git", or "local". Use "github" to install from github.fhcrc.org or github.com.
#' Use "git" to install from a git remote on the local machine or network filesystem. Use "local" to install
#' from an R package directory.
#' @param load TRUE by default. Set to false if you do not with to load the package via \code{library()}.
#' @param location With type='github', supply 'fhcrc' for github.fhcrc.org or supply 'github' for github.com).
#' With type='git' and type='local', supply the path to the github repo or R package, up to but not including
#' the repo/package object/folder.
#' @param org With type="github", supply the organazation, such as "VIDD-VISC" for github.fhcrc.org or "RGLab" for github.com.
#' @param auth_token With type='github', supply your personal access token, best stored as GITHUB_PAT in ~/.Renviron.
#' See https://help.github.com/articles/creating-a-personal-access-token-for-the-command-line/
#' @param hash The hash of a particular non-HEAD commit you wish to install (for type='github' or 'git' only).
#' @param tmpdir Used only when installing with type='git' and supplying a hash, and then only when the default
#' behavior, creating a randomly-named temp directory in your homedir, is problematic.
#' @param ... Additional argument(s) you wish to pass to the relevant devtools install_* function.
loadDataPackage <- function(package = NULL, install = FALSE, type = c("github", "git", "local"), load = TRUE,
                            location = "fhcrc", org = 'VIDD-VISC', auth_token = Sys.getenv("GITHUB_PAT"), hash = NULL, tmpdir = NULL, ...){

  type <- match.arg(type)

  if(is.null(tmpdir)){
    tmp <- file.path(Sys.getenv("HOME"), paste0("tmp", floor(runif(n = 1)*10^12))) # make up a tmpdir in user's homedir
  } else if(!dir.exists(normalizePath(tmpdir))){
    warning("Specified tmpdir does not exist. Using a temporary directory in user's homedir.")
    tmp <- file.path(Sys.getenv("HOME"), paste0("tmp", floor(runif(n = 1)*10^12)))
  } else {
    tmp <- tmpdir
  }

  if(location=="fhcrc"){
    location <- "github.fhcrc.org/api/v3"
    location_remotecheck <- "github.fhcrc.org"
    if(is.null(auth_token) | is.na(auth_token) | auth_token == ""){
      stop("When specifying type='github', for github.fhcrc.org, auth_token is required. See \
           https://help.github.com/articles/creating-a-personal-access-token-for-the-command-line/ \
           and save the resulting token as GITHUB_PAT in your ~/.Renvir file.", call.=FALSE)
    }
    } else if(location=="github"){
      location <- "api.github.com"
      location_remotecheck <- "github.com"
      auth_token <- NULL
    } else {
      if(type=="github"){stop("At this time, loadDataPackage() with type='github' only works on github.com and github.fhcrc.org repos.", call.=FALSE)}
    }

  if(install == TRUE){
    switch(type,
           "github" = { # "github" but no auth_token
             remotecheck <- .remotecheck_github(location_remotecheck, org, package) # test for existence of remote
             if(class(remotecheck)[1]=="simpleError"){stop(remotecheck)}

             message("Using devtools::install_github() with repo = ", org, "/", package, " and host = ", location)
             xt <- devtools::install_github(repo = paste0(org, "/", package), host=location, auth_token = auth_token, ref=hash, force=TRUE, ...)
             if(xt==FALSE){stop("Exiting. install_github() encountered problems.", call.=FALSE)}
           },

           "git" = {
             gitremote <- file.path(location, paste0(package, ".git"))
             remotecheck <- .remotecheck_git(gitremote)
             if(class(remotecheck)[1]=="simpleError"){stop(remotecheck)}

             # if hash is supplied, use install_git_visc(), else use install_git().
             if(is.null(hash)){
               message("Using devtools::install_git() to install ", gitremote, ".")
               xt <- devtools::install_git(url = gitremote, ...)
               if(xt==FALSE){stop("Exiting. install_git() encountered problems.", call.=FALSE)}
             } else {
               message("Using VISCfunctions::install_git_visc() to install from commit ", hash, " of ", gitremote, ".")
               install_git_visc(package=package, path=location, hash=hash, tmp=tmp)
             }
           },

           "local" = {
             remotecheck <- .remotecheck_local(location, package)
             if(class(remotecheck)[1]=="simpleError"){stop(remotecheck)}

             message("Using devtools::install_local() to install ", file.path(location, package), ".")
             xt <- devtools::install_local(path = file.path(location, package), force=TRUE, ...)
             if(xt==FALSE){stop("Exiting. install_local() encountered problems.", call.=FALSE)}
           }
    )
    message("\n* * * This installation of ", package, " has overwritten the prior installed version in ", .libPaths()[1], ". * * *")
  }

  if(load==TRUE){
    message("Loading ", package, " via library().")
    library(package, character.only = TRUE)
  }

  if(dir.exists(tmp)){
    message("Deleting tmp directory ", tmp)
    system2(command = "rm", args = paste("-rf", tmp)) # clean up tmp dir
  }
}


.remotecheck_github <- function(location, org, package){
  # Check that we have all info to ID the repo
  if(is.null(location) | is.null(org) | is.null(package) | location=="" | org=="" | package=="" | is.na(location) | is.na(org) | is.na(package)){
    result <- simpleError("When specifying type='github', location, org, and package are required.")
  } else {
    repotest <- tryCatch(system2("git", args=paste0("ls-remote git@", location, ":", org, "/", package), stdout=TRUE, stderr=TRUE)[1],
                         warning=function(w){"fatal"},
                         error=function(e){"fatal"})
    if(repotest=="fatal"){ # location exists but is not a git remote
      result <- simpleError(paste0("git@", location, ":", org, "/", package, " does not refer to a valid git repository."))
    } else {
      result <- TRUE
    }
  }
  return(result)
}


.remotecheck_git <- function(gitremote){
  if(!dir.exists(gitremote)){ # location doesn't exist
    result <- simpleError(paste("The directory", gitremote, "does not exist."))
  } else {
    repotest <- tryCatch(system2("git", args=paste("ls-remote", gitremote), stdout=TRUE, stderr=TRUE)[1],
                         warning=function(w){"fatal"},
                         error=function(e){"fatal"})
    if(repotest=="fatal"){ # location exists but is not a git remote
      result <- simpleError(paste("The directory", gitremote, "exists but does not appear to be a git repository."))
    } else {
      result <- TRUE
    }
  }
  return(result)
}


.remotecheck_local <- function(location, package){
  remote <- file.path(location, package)
  if(!dir.exists(remote)){ # dir doesn't exist
    result <- simpleError(paste("The directory", remote, "does not exist."))
  } else {
    repotest <- tryCatch(roxygen2:::read.description(file.path(remote, "DESCRIPTION"))$Package,
                         warning=function(w){"fatal"},
                         error=function(e){"fatal"})
    if(repotest=="fatal"){ # dir exists but is not a package
      result <- simpleError(paste("The directory", remote, "does not appear to be an R package."))
    } else {
      result <- TRUE
    }
  }
  return(result)
}

