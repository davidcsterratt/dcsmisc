## Get dataset i.
## The first time this data is read it is loaded from file, and then cached.
## On subsequent calls, it is retreived from the cache

get.dataset <- function(dataset, i) {
  ## Create list if it doesn't exist
  if (!exists(dataset)) {
    eval(parse(text=paste(dataset, " <<- list()")))
  }
  ## The data may be there
  if (eval(parse(text=paste("length(", dataset, ") >=", i)))) {
    ## So check...
    dat <- eval(parse(text=paste(dataset, "[[", i, "]]",sep=""))) 
    if (!is.null(dat)) {
      return(dat) 
    }
  }
  ## Read the file
  dat <- try(read.table(file=sprintf("%s-%05d",dataset,i),header=TRUE),TRUE)
  if (inherits(dat,"try-error")) {
    dat <- NA
  }

  ## Cache the data before returning
  eval(parse(text=paste(dataset, "[[", i , "]] <<- dat",sep="")))
  return(dat)
}
