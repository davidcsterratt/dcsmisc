##' @title Get all dependencies of package recursively
##' @param pkg Name of package
##' @return Vector of pakcage names and versions that \code{pkg} depends on
##' @author David Sterratt
getDeps <- function(pkg) {
  ## Remove package version
  pkg <- gsub(" (.*)", "", pkg)
  if (pkg == "R") {
    return(NULL)
  }
  depsstr <- packageDescription(pkg)$Depends
  deps <- c()
  if (!is.null(depsstr)) {
    deps <- strsplit(depsstr, ",[ \n]",perl=TRUE)[[1]]
  }
  importsstr <- packageDescription(pkg)$Imports
  imports <- c()
  if (!is.null(importsstr)) {
    deps <- strsplit(importsstr, ",[ \n]",perl=TRUE)[[1]]
  }
  toget <- c(deps, imports)
  if (is.null(toget)) {
    return(toget)
  }
  return(c(toget, unlist(sapply(toget, getDeps))))
}
