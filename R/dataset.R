##' Read a parameter file, in which each row contains a set of parameters.
##' @title Read parameter file
##' @param id Identity of parameter set in file to read
##' @param file Name of file
##' @return If \code{id} is specified, return return parameters from
##' row of parameter table specified by \code{id}; otherwise return
##' whole table
##' @author David Sterratt
read.pars <- function(id=NULL, file="pars.dat") {
  pars <- try(read.table(file), TRUE)
  if (inherits(pars, "try-error")) {
    return(NULL)
  }
  if (is.null(id)) {
    return(pars)
  }
  if (id>dim(pars)[1]) {
    return(NULL)
  }
  return(pars[id,])
}

## Write parameter file, returning data as a table
write.pars <- function(pars, file="pars.dat") {
  old.pars <- read.pars(file=file)
  if(!is.null(old.pars)) {
    id <- dim(old.pars)[1] + 1
    pars <- data.frame(id=id, pars)
    pars <- merge(old.pars, pars, all=TRUE)
  } else {
    id <- 1
    pars <- data.frame(id=id, pars)
    warning("New parameter file created")
  }
  write.table(pars, file)
  return(id)
}

## Get parameter file. 
get.pars <- function(file="pars.dat") {
  pars <- read.pars(file=file)
  if (is.null(pars)) {
    stop(paste("get.pars: Parameter file", file, "not found"))
  }
  return(pars)
}


##' The first time this data is read it is loaded from file, and then cached.
##' On subsequent calls, it is retreived from the cache
##' @title Read a dataset from file
##' @param dataset The name of the dataset. This will be stored in
##' files called \code{<dataset>-0000i<suffix>} where \code{i} is the
##' index of the dataset
##' @param i The index of the dataset
##' @param cache If \code{TRUE}, cache data
##' @param suffix The suffix of the data files
##' @return A data frame containing the dataset
##' @author David Sterratt
##' @export
read.dataset <- function(dataset, i=NULL, cache=TRUE, suffix=".dat") {
  ## Set flag noarray if i is null:
  if (is.null(i)) {
    noarray <- TRUE
    i <- 1
  }
  ## Create cache list if it doesn't exist 
  cachename <- paste("cache.", dataset, sep="")
  if (!exists(cachename)) {
    eval(parse(text=paste(cachename, "<<- list()")))
  }
       
  ## Collect data from the cache if it is there
  if (cache & eval(parse(text=paste("length(", cachename, ") >= i")))) {
    if (eval(parse(text=paste("!is.null(", cachename, "[[i]])", sep="")))) {
      return(eval(parse(text=paste(cachename, "[[i]]", sep=""))))
    }
  }

  ## Otherwise read the file
  if (noarray) {
    file <- sprintf("%s%s", dataset, suffix)
  } else {
    file <- sprintf("%s-%05d%s", dataset, i, suffix)
  }

  dat <- try(read.table(file=file, header=TRUE), TRUE)
  if (!inherits(dat,"try-error")) {
    ## Cache the data before returning
    eval(parse(text=paste(cachename, "[[i]] <<- dat", sep="")))
    return(dat)
  }
  return(NULL)
}

## Get a number of datasets and return as a list
read.datasets <- function(dataset, inds, suffix=".dat") {
  dat <- list()
  for (i in inds) {
    dat[[i]] <- read.dataset(dataset, i, suffix)
  }
  return(dat)
}

##' @title Write a dataset to file
##' @param x Data frame containing dataset 
##' @param dataset Name of dataset
##' @param suffix The suffix of the data file
##' @author David Sterratt
##' @export
write.dataset <- function(x, dataset, suffix=".dat") {
  write.table(x, paste(dataset, suffix, sep=""))
}

## Report the run numbers of any data missing from all the runs specified in
## the parameter file
find.missing.datasets <- function(dataset, ...) {
  pars <- get.pars(...)
  runs <- pars[,"run"]
  dat <- read.datasets(dataset, runs)
  return(runs[sapply(dat, is.null)])
}

##' @title Filter data frame
##' @param tab Data frame to filter
##' @param filt List of values of table headings to filter on
##' @return Filtered data frame
##' @author David Sterratt
##' @export
##' @examples
##' dat <- data.frame(a=1:5,
##'                   b=c(2, 2, 3, 3, 3),
##'                   c=c(5, 5, 5, 7, 7))
##' filter.table(dat, list(a=1))
##' filter.table(dat, list(b=3, c=5))
filter.table <- function(tab, filt=list()) {
  warning("This function is deprecated in favour of R's built-in subset()".)
  missing <- ((names(filt) %in% names(tab))==FALSE)
  if (any(missing)) {
    print("filter.table: These filters do not exist in table:")
    print(names(filt)[which(missing)])
    print("Choose from:")
    print(names(tab))
  }
  if (length(filt) >= 1) {
    return(filter.table(tab[tab[,names(filt[1])] %in% filt[1],], filt[-1]))
  } else {
    return(tab)
  }
}
