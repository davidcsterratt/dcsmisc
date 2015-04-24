merge.data.frames <- function(dat1, dat2) {
  if (is.null(dat1)){
    dat <- dat2
  } else {
    dat <- data.frame(rbind(dat1, dat2),row.names=NULL)
  }
  return(dat)
}

append.pars <- function(pars=NULL,
                        datapoints=NULL,
                        repeats=1,
                        prefix=".") {
  ## Backup existing parameter file
  file.copy(paste(prefix, "/pars.dat", sep=""),
            paste(prefix, "/pars.dat.bak", sep=""), overwrite = TRUE)
  
  ## Read in existing parameter file
  parfile <- paste(prefix, "/pars.dat", sep="")
  if (file.exists(parfile)) {
    oldpars <- read.table(parfile)
    run.offset <- max(oldpars[,"run"])
  } else {
    run.offset <- 0
    oldpars <- NULL
  }

  ## Create table of parameters
  newpars <- NULL
  for (i in 1:repeats) {
    newpars <- merge.data.frames(newpars, pars)
  }

  ## Make sure the runs are numbered
  Nruns  <- dim(newpars)[1]
  runs <- run.offset+(1:Nruns)
  newpars <- cbind(run=runs,newpars)

  ## Write the pars file
  pars <- rbind(oldpars, newpars)
  write.table(pars, file=parfile)

  ## Create the data files which contain the datapoints
  if (!is.null(datapoints)) {
    dat <- expand.grid(datapoints)
    for (i in 1:Nruns) {
      write.table(dat,file=paste(sim.dir,"/run",pars[i,"run"],"pars.dat",sep=""))
    }
  }
  return(runs)
}

condor.prepare <- function(pars=NULL,
                           datapoints=NULL,
                           repeats=1,
                           prefix=".",
                           submit.file="condor.submit",
                           executable="",
                           universe="vanilla",
                           arguments="",
                           input="",
                           memory=256) {

  ## Write the parameter file
  runs <- append.pars(pars=pars, datapoints=datapoints, repeats=repeats,
                      prefix=prefix)

  ## Write the condor submit file
  condor.write.submit.file(runs=runs, prefix=prefix,
                           submit.file=submit.file,
                           executable=executable,
                           universe=universe,
                           arguments=arguments,
                           input=input,
                           memory=memory)
}

condor.write.submit.file <- function(runs, prefix=".",
                                     submit.file="condor.submit",
                                     executable="",
                                     universe="vanilla",
                                     arguments="",
                                     input="",
                                     memory=256
                                     ) {
  cat(paste("Universe = ", universe, "
Executable = ", executable, "
Arguments = ", arguments, "
Initialdir   = ", prefix, "
Input = ", input, "
Getenv = true
on_exit_remove = (ExitBySignal == False) || (ExitSignal != 9)
Notification = Always
Requirements = Memory >= ", memory, "
", sep=""),file=paste(prefix,"/",submit.file,sep=""))
  for (i in runs) {
    cat(paste("Environment = i=", i,"\n",
              "Arguments = ", i,"\n", 
              "Error = sim", i, ".err\n",
              "Log = sim", i, ".log\n",
              "Queue\n", sep=""),
        file=paste(prefix,"/",submit.file,sep=""),append=TRUE)
  }
}
