## Read parameter file, returning data as a table
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
}

## Get parameter file. 
get.pars <- function(file="pars.dat") {
  pars <- read.pars(file=file)
  if (is.null(pars)) {
    stop(paste("get.pars: Parameter file", file, "not found"))
  }
  return(pars)
}

## Get dataset i.
## The first time this data is read it is loaded from file, and then cached.
## On subsequent calls, it is retreived from the cache

get.dataset <- function(dataset, i, cache=TRUE) {
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
  dat <- try(read.table(file=sprintf("%s-%05d",dataset,i),header=TRUE),TRUE)
  if (!inherits(dat,"try-error")) {
    ## Cache the data before returning
    eval(parse(text=paste(cachename, "[[i]] <<- dat", sep="")))
    return(dat)
  }
  return(NULL)
}

## Get a number of datasets and return as a list
get.datasets <- function(dataset, inds) {
  dat <- list()
  for (i in inds) {
    dat[[i]] <- get.dataset(dataset, i)
  }
  return(dat)
}

## Report the run numbers of any data missing from all the runs specified in
## the parameter file
find.missing.datasets <- function(dataset, ...) {
  pars <- get.pars(...)
  runs <- pars[,"run"]
  dat <- get.datasets(dataset, runs)
  return(runs[sapply(dat, is.null)])
}

## Filter table tab by the values of table headings specified in the list
## filt
filter.table <- function(tab, filt=list()) {
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
