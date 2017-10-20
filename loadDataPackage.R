#' Install and/or load a given data package from github, a git repo, or a local directory
#'
#' This function attempts to be a flexible data package loading tool.
#'
#' @param packageString The identifying string of the package -- in the form of a git@ or https:// link to github.com
#' or an enterprise github server, or the (preferably full) path to a repo on a local filesystem.
#' @param install FALSE by default. Set to TRUE if you wish to install the package rather than loading
#' the currently-installed version using \code{library()}.
#' @param load TRUE by default. Set to false if you do not with to load the package via \code{library()}.
#' @param auth_token Your personal access token, best stored as GITHUB_PAT in ~/.Renviron and
#' accessed here via \code{devtools::github_pat()}. See
#' https://help.github.com/articles/creating-a-personal-access-token-for-the-command-line/
#' @param sha The hash of a particular commit you wish to install.
#' @param ... Additional argument(s) you wish to pass to the relevant devtools install_* function.
#' @examples
#' \dontrun{
#' loadDataPackage("git@github.com:rstudio/rmarkdown.git", install=TRUE, load=TRUE)
#' loadDataPackage("https://github.fhcrc.org/VIDD-VISC/Pantaleo.662.git", install=TRUE, load=TRUE)
#' loadDataPackage("/Volumes/networks/cavd/studies/cvd477/pdata/Gallo.PA.477.git", sha = "001b8a0", install=TRUE, load=TRUE)
#' }
#' @export
loadDataPackage <- function(packageString = NULL, sha = NULL, install = FALSE, load = TRUE,
                            auth_token = devtools::github_pat(quiet = TRUE), ...){

  packageString <- gsub(paste0("[", .Platform$file.sep, "]$"), "", packageString) # strip trailing slash

  parsed <- .parsePackageString(packageString)

  if(is.null(parsed)){
    stop(paste0("It was not possible to parse ", packageString,
                ". Please provide a git@github or https link, or the full path to a remote repo on a local filesystem, ",
                "or the full path to an R package directory (not a .tar.gz file)."))
  } else if(length(parsed) > 1){
    package = gsub(".git$", "", parsed["package"])
  } else {
    reparsed <- strsplit(packageString, split = .Platform$file.sep)[[1]]
    package <- gsub(".git$", "", tail(reparsed, n=1))
  }

  if(install == TRUE){
    if(length(parsed) > 1){ # github
      rc <- .webInstall(parsed = parsed, sha = sha, auth_token = auth_token, ...)
    } else { # local package or network remote
      if(is.null(sha)){
        if(.is_R_package(packageString)){ # R package dir
          message("Installing from package directory ", packageString, " using install_local()")
          rc <- devtools::install_local(path = packageString, quiet = TRUE, ...)
        } else if(.is_git_remote(packageString)){ # network remote
          message("Installing from remote repo ", packageString, "  using install_git()")
          rc <- devtools::install_git(url = packageString, quiet = TRUE, ...)
        }
      } else {
        rc <- install_git_visc(path = packageString, sha = sha, ...)
      }
    }
    if(rc == TRUE)
      message("\n* * * This installation of ", package, " has overwritten the prior installed version in ", .libPaths()[1], ". * * *")
  }

  if(load == TRUE){
    parsed <- .parsePackageString(packageString)
    rc2 <- library(package, character.only = TRUE, logical.return = TRUE)
    if(rc2 == TRUE){
      message("Loading ", package, " via library().")
    }
  }

  if(install == TRUE)
    return(rc)
  else if(load == TRUE)
    return(rc2)
}


#' Install an R package from a particular commit to a local filesystem git repo
#'
#' This function is required because there is no way to pass a particular commit to \code{devtools::install_git()}
#' the same way you can pass the \code{ref} argument to \code{devtools::install_github()}. The package
#' clones the the provided package to a temp directory (either user-specified or a default random-string folder
#' in the user's home directory). If a hash has been provided, that package is installed from that point in the
#' version history. Otherwise, the most recent commit is installed.
#'
#' @param path The full path to the git repo on the local filesystem
#' @param sha The hash of the particular commit you wish to install. If null, the most recent commit.
#' the repo there. Provide an alternate location with this argument.
#'
install_git_visc <- function(path, sha, ...){
  # create a temp directory for cloning
  tmp <- file.path(tempfile(pattern="git2r-"), "temp")
  dir.create(tmp, recursive=TRUE)

  repo <- git2r::clone(path, local_path = tmp, progress = FALSE)

  # validate sha, and retrieve full sha is partial is provided
  shaV <- .validateSHA(repo, sha)
  git2r::checkout(git2r::revparse_single(repo, shaV)) # checkout sha if provided

  if(!.is_R_package(tmp)){
    stop("The repo was successfully cloned, but it does not seem to contain an R package.")
    return(FALSE)
  }

  message("Installing from ", path, " with SHA ", sha, ", cloned locally, using install_git_visc()")
  devtools::install_local(path = tmp, upgrade_dependencies=FALSE, ...)
  return(TRUE)
}



# utility function to install from web-based git GUIs
.webInstall <- function(parsed, sha = NULL, auth_token = NULL, ...){

  # basic sanity check on .git ending
  parsed["package"] <- ifelse(grepl(".git$", parsed["package"]), parsed["package"], paste0(parsed["package"], ".git"))

  packageString <- paste0("https://", paste(parsed, collapse=.Platform$file.sep)) # this forces mode to github.com_http or other_git_http
  mode <- .defineMode(packageString)

  # require auth_token for non-github web git
  if(is.null(auth_token) & mode == c("other_git_http"))
    stop("You must provide an auth_token when specifying enterprise GitHub packages.")

  if(!is.null(auth_token)){cred <- git2r::cred_token()} else {cred <- NULL}

  switch(mode,
         github.com_http = {
           if(!is.null(sha)){
             shout <- .validateSHA.string(packageString, sha, cred = cred)
             sha <- shout[["sha"]]
             localPath <- shout[["tempdir"]]
             git2r::checkout(git2r::revparse_single(shout[["repo"]], sha)) # checkout sha if provided

             if(!.is_R_package(localPath))
               stop("The repo was successfully cloned, but it does not seem to contain an R package.")

             message("Installing from ", packageString, " with SHA ", sha, ", cloned locally, using install_local()")
             rc <- devtools::install_local(localPath, quiet = TRUE, ...)  # install_local because we already had to clone to validate SHA
           } else  {
             package <- paste0(parsed[["org"]], "/", gsub(".git$", "", parsed["package"], fixed=FALSE))
             message("Installing ", package, " using install_github()...")
             rc <- devtools::install_github(package, auth_token = NULL, ...)
           }
         },
         other_git_http  = {
           if(!is.null(sha)){
             shout <- .validateSHA.string(packageString, sha, cred = cred)
             sha <- shout[["sha"]]
             localPath <- shout[["tempdir"]]
             git2r::checkout(git2r::revparse_single(shout[["repo"]], sha)) # checkout sha if provided

             if(!.is_R_package(localPath))
               stop("The repo was successfully cloned, but it does not seem to contain an R package.")

             message("Installing from ", packageString, " with SHA ", sha, ", cloned locally, using install_local()")
             rc <- devtools::install_local(localPath, quiet = TRUE, ...)  # install_local because we already had to clone to validate SHA
           } else  {
             package <- paste0(parsed[["org"]], "/", gsub(".git$", "", parsed["package"], fixed=FALSE))
             host <- paste0(parsed["server"], "/api/v3")
             message("Installing ", package, " from ", host, " using install_github()...")
             rc <- devtools::install_github(package, host = host, auth_token = auth_token, ...)
           }
         },
         unknown = {
           message(".webInstall() is unable to determine how to install ", packageString, " and will exit.")
           rc <- FALSE
         })
  return(rc)
}




# utility function to parse the provided packageString and determine how and from where to install a package
.parsePackageString <- function(packageString){
  remote_src <- ifelse(grepl("^http", packageString), "web_remote_http",
                       ifelse(grepl("^git\\@", packageString), "web_remote_git",
                              ifelse(dir.exists(packageString), "local", "unknown")))

  # distinguish if local is a local or a remote
  if(remote_src=="local"){
    remote_src <- ifelse(.is_R_package(packageString), "local_package",
                         ifelse(.is_git_remote(packageString), "local_remote", "nor_package_nor_remote"))
  }

  switch(remote_src,
         web_remote_http = {
           path <- strsplit(packageString, split = ":*/+", fixed=FALSE)[[1]]
           if(path[1] %in% c("http", "https")) path <- path[-1]
           names(path) <- c("server", "org", "package")
         },
         web_remote_git = {
           path <- strsplit(packageString, split = "[@:/]", fixed=FALSE)[[1]]
           if(path[1] == "git") path <- path[-1]
           names(path) <- c("server", "org", "package")
         },
         local_package = {
           if(.is_git_local_repo(packageString)){
             path <- normalizePath(packageString)
             names(path) <- "fullpath_git_local_repo"
           } else {
             path <- normalizePath(packageString)
             names(path) <- "fullpath_no_git"
           }
         },
         local_remote = {
           path <- normalizePath(packageString)
           names(path) <- "fullpath"
         })
  if(remote_src %in% c("unknown", "nor_package_nor_remote")){
    valid <- FALSE
  } else {
    if(! length(path) %in% c(1, 3)){stop(".parsePackageString() returned a parsed path with length other than 1 (local fs) or 3 (web). Can't handle it.")}
    valid <- .validatePath(path, method = remote_src)
  }
  if(valid){return(path)} else {return(NULL)}
}



# utility function to validate provided paths based on the type determined by .parsePackageString()
.validatePath <- function(path, method = c("web_remote_http", "web_remote_git", "local_package", "local_remote")){
  method = match.arg(method)

  # test for incorrect methods given a particular kind of path
  if(length(path)>1 & method %in% c("local_package", "local_remote")){
    message("The provided parsed path has length >1, indicating it's a web URL, but you've provided a local method.")
    return(FALSE)
  } else if(length(path)==1 & method %in% c("web_remote_http", "web_remote_git")){
    message("The provided parsed path has length 1, indicating it's a local path, but you've provided a web method.")
    return(FALSE)
  }

  switch(method,
         web_remote_http = {
           packageString <- paste0("https://", paste(path, collapse=.Platform$file.sep))
           if(grepl("github.com", packageString)){cred <- NULL} else {cred <- git2r::cred_token()}
           tryCatch(results <- git2r::remote_ls(packageString, credentials = cred),
                    error = function(e){message("No URL ", packageString, " exists.")})
         },
         web_remote_git = {
           packageString <- paste0("https://", paste(path, collapse=.Platform$file.sep))
           if(grepl("github\\.com$", packageString)){cred <- NULL} else {cred <- git2r::cred_token()}
           tryCatch(results <- git2r::remote_ls(packageString, credentials = cred),
                    error = function(e){message("No URL ", packageString, " exists.")})
         },
         local_package = {
           if(names(path) == "fullpath_git_local_repo"){
             tryCatch(results <- git2r::remote_ls(path),
                      error = function(e){message("The path ", path, " is an R package with git but remote_ls failed.")})
           } else if(names(path) == "fullpath_no_git"){
             results <- "Making this just so exists('results')==TRUE... no git repo to verify but it's a path and a package."
           }
         },
         local_remote = {
           tryCatch(results <- git2r::remote_ls(path),
                    error = function(e){message("The path ", path, " exists and is a git remote but remote_ls failed.")})
         }
  )
  if(exists("results")){
    return(TRUE)
  } else {
    return(FALSE)
  }
}



# is the provided path a git remote?
.is_git_remote <- function(path){
  test <- git2r::discover_repository(path)
  if(!is.null(test)){
    if(gsub(paste0("[", .Platform$file.sep, "]$"), "", git2r::discover_repository(path)) == path){
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    return(FALSE)
  }
}



# is the provided path a local git repo?
.is_git_local_repo <- function(path){
  test <- git2r::discover_repository(path)
  if(!is.null(test)){
    if(git2r::discover_repository(path) == paste0(file.path(path, ".git"), .Platform$file.sep)){
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    return(FALSE)
  }
}



# is the provided path an R package?
.is_R_package <- function(path){
  pkg <- tryCatch(devtools::as.package(path),
                  warning = function(w){message(w)},
                  error = function(e){
                    return(FALSE)
                  })
  return(devtools::is.package(pkg))
}



# utility function to define a type switch based on parsed packageString
.defineMode <- function(packageString){
  result <- ifelse(grepl("^http[s]*[:/]+[\\w_.-]*.*github.com/[\\w/._-]+.git$", packageString, perl=TRUE), "github.com_http",
                   ifelse(grepl("^git@github.com:[\\w+/._-]+.git$", packageString, perl=TRUE), "github.com_ssh",
                          ifelse(grepl("^http[s]*[:/]+[\\w+_.-]+(org|com|edu|gov|net)/[\\w/_.-]+.git$", packageString, perl=TRUE), "other_git_http",
                                 ifelse(grepl("^git@[\\w+_.-]+(org|com|edu|gov|net):[\\w/_.-]+.git$", packageString, perl=TRUE), "other_git_ssh",
                                        "unknown"))))
  return(result)
}


# utility function to validate the provided SHA from a packageString
.validateSHA.string <- function(packageString, sha, cred){
  # create a temp directory for cloning
  tmp <- file.path(tempfile(pattern="git2r-"), "temp")
  dir.create(tmp, recursive=TRUE)
  repo <- git2r::clone(packageString, local_path = tmp, credentials = cred, progress = FALSE)

  hashes <- sapply(git2r::commits(repo), function(x) x@sha)
  if(any(grep(sha, hashes))){
    sha <- hashes[which(grepl(paste0("^", sha), hashes))]
  } else {
    stop("The provided commit SHA does not appear in the commit history of ", packageString, ". Please verify it and try again.")
  }
  if(length(sha) > 1){stop("The provided commit sha is not unique. Please use a longer partial SHA.")}

  returnObject <- list(repo = repo, sha = sha, tempdir = tmp)
  return(returnObject)
}




# .scrapePackageString <- function(packageString){
#   scrape <- gsub("^git@([\\w.]*):([\\w/_-]+).git$", paste0("\\2,\\1/api/v3"), packageString, perl=TRUE)
#   scrape <- strsplit(scrape, ",")[[1]]
#   return(scrape)
# }
