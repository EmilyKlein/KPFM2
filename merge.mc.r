merge.mc<-function(mc.object1, mc.object2, ssds = FALSE)
{
	# auxiliary function to merge the output from two independent
	# runs of ssmu.mc() -- BE CAREFUL!  there is minimal error trapping
	# here and it may not be appropriate to merge the results of some
	# runs
	#
	# George Watters
	# code last edited 18 July 2006
  #
  # mc.out <- list(call = cl, setup=list(ntrials = ntrials, ntimes=ntimes, nseasons=param.list$NSEASONS, nareas=nareas, nssmus=nssmus, ntubs=param.list$NTUBS, init.season=param.list$INIT.SEASON), N=list(krill=krill, seals=seals, pengs=pengs, whales=whales, fish=fish), R=list(R=list(krill=Rkrill, seals=Rseals, pengs=Rpengs, whales=Rwhales, fish=Rfish),
  #  maxRage=list(krill=krill.Rage, seals=seals.maxRage, pengs=pengs.maxRage, whales=whales.maxRage, fish=fish.maxRage)),
  #  Q=list(demand=list(seals=Qseals, pengs=Qpengs, whales=Qwhales, fish=Qfish), consumption=list(seals=Qseals.performance, pengs=Qpengs.performance, whales=Qwhales.performance, fish=Qfish.performance)), fishery=list(allocation=earmarks, catch=catch))
  #
  # check that the two objects are compatible -- this compatibility check is very stringent
  # all components of setup EXCEPT ntrials must be the same for both objects
  compatible <- do.call("all.equal",list(mc.object1$setup[c("ntimes","nseasons","nareas","nssmus","ntubs","init.season","start.fishing","stop.fishing","relative")],mc.object2$setup[c("ntimes","nseasons","nareas","nssmus","ntubs","init.season","start.fishing","stop.fishing","relative")]))
  if(is.character(compatible)){
    fault.message <- cat(paste("FAULT -- incompatible mc.objects!\nntimes, nseasons, nareas, nssmus, ntubs, init.season, start.fishing, stop.fishing, relative\n",compatible,"\n",fill=TRUE))
    print(fault.message)
    stop()
  }
  # check that Rages are same across mc.objects
  compatible <- do.call("all.equal",list(mc.object1$R$maxRage,mc.object2$R$maxRage))
  if(is.character(compatible)){
    fault.message <- cat(paste("FAULT -- incompatible mc.objects!\nkrill.Rage, seals.maxRage, pengs.maxRage, whales.maxRage, fish.maxRage\n",compatible,"\n",fill=TRUE))
    print(fault.message)
    stop()
  }
  #
	new.dim3 <- mc.object1$setup$ntrials + mc.object2$setup$ntrials
	out.object <- vector(mode = "list", length = length(mc.object1))
  names(out.object) <- names(mc.object1)
  # bind the calls
  out.object$call <- c(mc.object1$call, mc.object2$call)
  # bind the setups
  out.object$setup <- mc.object1$setup
  out.object$setup$ntrials <- new.dim3
  # bind the abundance arrays
  out.object$N <- mc.object1$N
  out.object$N$krill <- array(c(mc.object1$N$krill, mc.object2$N$krill),dim=c(dim(mc.object1$N$krill)[1],dim(mc.object1$N$krill)[2],new.dim3))
  out.object$N$seals <- array(c(mc.object1$N$seals, mc.object2$N$seals),dim=c(dim(mc.object1$N$seals)[1],dim(mc.object1$N$seals)[2],new.dim3))
  out.object$N$pengs <- array(c(mc.object1$N$pengs, mc.object2$N$pengs),dim=c(dim(mc.object1$N$pengs)[1],dim(mc.object1$N$pengs)[2],new.dim3))
  out.object$N$whales <- array(c(mc.object1$N$whales, mc.object2$N$whales),dim=c(dim(mc.object1$N$whales)[1],dim(mc.object1$N$whales)[2],new.dim3))
  out.object$N$fish <- array(c(mc.object1$N$fish, mc.object2$N$fish),dim=c(dim(mc.object1$N$fish)[1],dim(mc.object1$N$fish)[2],new.dim3))
  # bind the recruitment arrays
  # note, don't need to overwrite mc.object1$R$maxRage because checked that these are the same
  out.object$R <- mc.object1$R
  out.object$R$R$krill <- array(c(mc.object1$R$R$krill, mc.object2$R$R$krill),dim=c(dim(mc.object1$R$R$krill)[1],dim(mc.object1$R$R$krill)[2],new.dim3))
  out.object$R$R$seals <- array(c(mc.object1$R$R$seals, mc.object2$R$R$seals),dim=c(dim(mc.object1$R$R$seals)[1],dim(mc.object1$R$R$seals)[2],new.dim3))
  out.object$R$R$pengs <- array(c(mc.object1$R$R$pengs, mc.object2$R$R$pengs),dim=c(dim(mc.object1$R$R$pengs)[1],dim(mc.object1$R$R$pengs)[2],new.dim3))
  out.object$R$R$whales <- array(c(mc.object1$R$R$whales, mc.object2$R$R$whales),dim=c(dim(mc.object1$R$R$whales)[1],dim(mc.object1$R$R$whales)[2],new.dim3))
  out.object$R$R$fish <- array(c(mc.object1$R$R$fish, mc.object2$R$R$fish),dim=c(dim(mc.object1$R$R$fish)[1],dim(mc.object1$R$R$fish)[2],new.dim3))
  # bind the demand and consumption arrays
  out.object$Q <- mc.object1$Q
  out.object$Q$demand$seals <- array(c(mc.object1$Q$demand$seals, mc.object2$Q$demand$seals),dim=c(dim(mc.object1$Q$demand$seals)[1],dim(mc.object1$Q$demand$seals)[2],new.dim3))
  out.object$Q$demand$pengs <- array(c(mc.object1$Q$demand$pengs, mc.object2$Q$demand$pengs),dim=c(dim(mc.object1$Q$demand$pengs)[1],dim(mc.object1$Q$demand$pengs)[2],new.dim3))
  out.object$Q$demand$whales <- array(c(mc.object1$Q$demand$whales, mc.object2$Q$demand$whales),dim=c(dim(mc.object1$Q$demand$whales)[1],dim(mc.object1$Q$demand$whales)[2],new.dim3))
  out.object$Q$demand$fish <- array(c(mc.object1$Q$demand$fish, mc.object2$Q$demand$fish),dim=c(dim(mc.object1$Q$demand$fish)[1],dim(mc.object1$Q$demand$fish)[2],new.dim3))
  out.object$Q$consumption$seals <- array(c(mc.object1$Q$consumption$seals, mc.object2$Q$consumption$seals),dim=c(dim(mc.object1$Q$consumption$seals)[1],dim(mc.object1$Q$consumption$seals)[2],new.dim3))
  out.object$Q$consumption$pengs <- array(c(mc.object1$Q$consumption$pengs, mc.object2$Q$consumption$pengs),dim=c(dim(mc.object1$Q$consumption$pengs)[1],dim(mc.object1$Q$consumption$pengs)[2],new.dim3))
  out.object$Q$consumption$whales <- array(c(mc.object1$Q$consumption$whales, mc.object2$Q$consumption$whales),dim=c(dim(mc.object1$Q$consumption$whales)[1],dim(mc.object1$Q$consumption$whales)[2],new.dim3))
  out.object$Q$consumption$fish <- array(c(mc.object1$Q$consumption$fish, mc.object2$Q$consumption$fish),dim=c(dim(mc.object1$Q$consumption$fish)[1],dim(mc.object1$Q$consumption$fish)[2],new.dim3))
  # bind the fishery arrays
  out.object$fishery <- mc.object1$fishery
  out.object$fishery$allocation <- array(c(mc.object1$fishery$allocation, mc.object2$fishery$allocation),dim=c(dim(mc.object1$fishery$allocation)[1],dim(mc.object1$fishery$allocation)[2],new.dim3))
  out.object$fishery$catch <- array(c(mc.object1$fishery$catch, mc.object2$fishery$catch),dim=c(dim(mc.object1$fishery$catch)[1],dim(mc.object1$fishery$catch)[2],new.dim3))
  out.object$fishery$threshold.violations <- rbind(mc.object1$fishery$threshold.violations,mc.object2$fishery$threshold.violations)
  # now add in color tags
  # "same sim different seed" results in assigning color.tag 1 to the whole lot
  if(ssds){
    out.object$color.tags <- rep(1, new.dim3)
  }
  else{
	  if(!is.null(mc.object1$color.tags) && is.null(mc.object2$color.tags)) {
		  new.tag <- max(mc.object1$color.tags) + 1
		  out.object$color.tags <- c(mc.object1$color.tags, rep(new.tag, mc.object2$setup$ntrials))
	  }
	  else {
		  if(is.null(mc.object1$color.tags) && !is.null(mc.object2$color.tags)) {
        stop("Please reverse order of mc.objects in call to mc.merge(); otherwise order of calls would not match order of color tags.")
		  }
		  else {
			  out.object$color.tags <- c(rep(1, mc.object1$setup$ntrials), rep(2, mc.object2$setup$ntrials))
		  }
	  }
	}
	out.object
}
