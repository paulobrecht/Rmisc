#' Copy data files from latest version of a package to local repo of another package
#'
#' Note that the working directory when the function is called must be inside your local repo.
#'
#' @param package The name of the source package.
#' @param branch Supply if data to copy is on a branch other than 'master'
#' @param sha Supply if data version desired is tied to a particular commit of the source package
#' @export
updateVISCdata <- function(org = "https:github.com/paulobrecht/", package = NULL, branch = NULL, sha = NULL){

  cred <- git2r::cred_token() # this can be fleshed out later to include ssh with git2r::cred_ssh_key

  # create a temp directory for cloning
  local <- file.path(tempfile(pattern="git2r-"), package)
  dir.create(local, recursive=TRUE)

  # clone
  repo <- git2r::clone(paste0("https://github.fhcrc.org/VIDD-VISC/", package),
                       local_path = local, branch = branch, credentials = cred, progress = FALSE)

  # validate sha, and retrieve full sha is partial is provided
  if(!is.null(sha)){
    shaV <- .validateSHA(repo, sha)
    git2r::checkout(git2r::revparse_single(repo, shaV)) # checkout sha if provided
  }

  # copy data
  sep <- .Platform$file.sep
  WDV <- .validateWD()
  if(WDV != FALSE){
    dataFromPath <- file.path(local, "data")
    dataToPath <- WDV
    filesToCopy <- list.files(dataFromPath, pattern = ".[rR][dD][aA]$")
    if(length(filesToCopy)==0)
      message("No .rda files to copy!")
    for(f in filesToCopy){
      result <- file.copy(from = file.path(path, "data", f), to = dataToPath, overwrite = TRUE, copy.mode = TRUE, recursive = FALSE)
      message("Copying ", package, sep, "data", sep, f, " to ", dataToPath, sep, "... ", ifelse(result=="TRUE", "Success.", "Failure."))
    }
  }
  return(invisible())
}

# utility function to validate the provided SHA
.validateSHA <- function(repo, sha){
  hashes <- sapply(git2r::commits(repo), function(x) x@sha)
  if(any(grep(sha, hashes))){
    sha <- hashes[which(grepl(paste0("^", sha), hashes))]
  } else {
    stop("The provided commit SHA does not appear in the commit history. Please verify it and try again.")
  }
  if(length(sha) > 1){stop("The provided commit sha is not unique. Please use a longer partial SHA.")}
  return(sha)
}

# utility function to ensure working directory is within a git repo called VISCfunctions
.validateWD <- function(wd = getwd()){

  sep <- .Platform$file.sep
  curr_repo <- gsub(paste0("[", sep, "]$"), "",
                    gsub("/", sep, git2r::discover_repository(wd), fixed = TRUE))
  if(length(curr_repo)==0){stop("Working directory must be inside a git repo when calling updateVISCdata.")}

  repo_main <- gsub(paste0(sep, ".git"), "", file.path(curr_repo))
  tryCatch(this_package <- devtools::as.package(repo_main),
           error = function(e){stop("The current repo does not appear to be an R package.")})

  data_dir <- file.path(repo_main, "data")
  if(!dir.exists(data_dir))
    dir.create(data_dir)

  return(data_dir)
}
