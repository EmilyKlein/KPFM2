ssmu.mc<-function(param.list, saved.state = NULL, ggp=NULL,ntrials = 2, quants=c(0.2, 0.5, 0.8), plot.now = TRUE,
	nyears = 30, start.fishing.yr = 1, stop.fishing.yr = NULL, rescale.catch = FALSE, random.Rkrill = TRUE,
	sd.krill.Rdev = 0.4, fishing.option = NULL, actual.gamma = 0.17, precautionary.limit.mmt = NULL,
	allocation.mult = 1., allocation.CV = 0.2, B0.mult = 1., B0.CV = 0.2, reassessment.interval.yr = NULL, plot.trials = FALSE, annual.plots = TRUE, 
	safe.mode = TRUE, annual.connector = "default", Rylimits = c(0, 3), Nylimits = c(0, 3), Cylimits = c(0, 3), page.layout = c(4, 4),
	point.size = 0.001, mc.seed = 123)
{
	# George Watters
	# last edited 11 July 2006
  # edited 30 July 2013 by JTH to include 'GGP=NULL' in the function call to ssmu.ss() for use of mc simulations when a time-varying driver of krill weight is used
	# edited 25 July 2014 by JTH to include ggp=NULL in the function call to ssmu.mc(). 
  #
  cl <- match.call()
  set.seed(mc.seed)
	#
	if(!is.null(saved.state) && safe.mode){
	  if(cl$param.list != saved.state$call$param.list) stop("FAULT -- the continuing simulation does not have the same parameter set as the saved state!")
  }
  #
  nareas <- dim(param.list$V.ARRAY)[1]
	nssmus <- nareas - param.list$NTUBS
	#
 	# get maximum recruitment ages for predators
	# recruitment age is converted from units of years to seasons
	# krill are assumed to recruit to fisheries and predators
	# at the same age no matter what ssmu they are in
	# initiate the model at the TIME OF REPRODUCTION
  tt.age <- sapply(param.list$SEALS,FUN=function(x){x$Rage})
  if(all(is.na(tt.age))){seals.maxRage <- 0}else{seals.maxRage<-max(tt.age,na.rm=TRUE)}
  seals.maxRage <- (seals.maxRage * param.list$NSEASONS)
	#
  tt.age <- sapply(param.list$PENGS,FUN=function(x){x$Rage})
  if(all(is.na(tt.age))){pengs.maxRage <- 0}else{pengs.maxRage<-max(tt.age,na.rm=TRUE)}
	pengs.maxRage <- (pengs.maxRage * param.list$NSEASONS)
  #
  tt.age <- sapply(param.list$WHALES,FUN=function(x){x$Rage})
  if(all(is.na(tt.age))){whales.maxRage <- 0}else{whales.maxRage<-max(tt.age,na.rm=TRUE)}
	whales.maxRage <- (whales.maxRage * param.list$NSEASONS)
	#
  tt.age <- sapply(param.list$FISH,FUN=function(x){x$Rage})
  if(all(is.na(tt.age))){fish.maxRage <- 0}else{fish.maxRage<-max(tt.age,na.rm=TRUE)}
	fish.maxRage <- (fish.maxRage * param.list$NSEASONS)
	#
	krill.Rage <- (param.list$KRILL.RAGE * param.list$NSEASONS)
	#
	# now initialize a bunch of stuff
	#
	ntimes <- nyears * param.list$NSEASONS
	start.fishing <- (start.fishing.yr * param.list$NSEASONS) - param.list$NSEASONS + 1
  if(!is.null(stop.fishing.yr)){
    stop.fishing <- (stop.fishing.yr * param.list$NSEASONS) - param.list$NSEASONS + 1
  }
  else {
    stop.fishing <- ntimes + 1
  }
  #
  # first the abundance arrays
  # add a few years to the top of the matrix to
	# account for recruitment lags
	krill <- array(0, dim = c(ntimes + krill.Rage, nareas, ntrials))
	seals <- array(0, dim = c(ntimes + seals.maxRage, nssmus, ntrials))
	pengs <- array(0, dim = c(ntimes + pengs.maxRage, nssmus, ntrials))
	whales <- array(0, dim = c(ntimes + whales.maxRage, nssmus, ntrials))
	fish <- array(0, dim = c(ntimes + fish.maxRage, nssmus, ntrials))
	#
	# Q is per-capita demand for prey by predators breeding in area i
	# note that predators can forage in bathtubs, but this doesn't affect prey dynamics in the tubs
	# Q is annual, per-capita consumption
	Qseals <- array(0, dim = c(ntimes + seals.maxRage, nssmus, ntrials))
	Qpengs <- array(0, dim = c(ntimes + pengs.maxRage, nssmus, ntrials))
	Qwhales <- array(0, dim = c(ntimes + whales.maxRage, nssmus, ntrials))
	Qfish <- array(0, dim = c(ntimes + fish.maxRage, nssmus, ntrials))
  #
  # Qperformance is per-capita consumption (as opposed to demand)
  Qseals.performance <- Qseals
  Qpengs.performance <- Qpengs
  Qwhales.performance <- Qwhales
  Qfish.performance <- Qfish
  #
  # recruitment arrays
	Rkrill <- array(0, dim = c(ntimes, nssmus, ntrials))
	Rseals <- array(0, dim = c(ntimes, nssmus, ntrials))
	Rpengs <- array(0, dim = c(ntimes, nssmus, ntrials))
	Rwhales <- array(0, dim = c(ntimes, nssmus, ntrials))
	Rfish <- array(0, dim = c(ntimes, nssmus, ntrials))
  #
 	# catch stuff
	catch <- array(0, dim = c(ntimes, nssmus, ntrials))
	earmarks <- catch
	threshold.violations <- matrix(0,nrow=ntrials,ncol=nssmus)
  #
  # krill weight stuff
  wbars<-array(0, dim=c(ntimes, nareas, ntrials))
	# OK do the simulations
	# first remember what was in the bathtubs -- will select from here as necessary
	# to do the MC trials
	orig.bathtubs <- param.list$BATHTUB.ABUNDANCE
	for(j in 1:ntrials) {
    #
    # allow the user to input a matrix of bathtub abundances
		# the matrix should have nrow=nyears and ncol=ntrials
		# the following code removes the jth column vector from
		# the matrix so that it can be passed to ssmu.model()
		for(k in 1:param.list$NTUBS) {
			 if(is.matrix(orig.bathtubs[[k]])) {
	      param.list$BATHTUB.ABUNDANCE[[k]] <- orig.bathtubs[[k]][, j]
			 }
		}
		#
		tt <- ssmu.ss(param.list = param.list, GGP=ggp, saved.state = saved.state, nyears = nyears, start.fishing.yr = start.fishing.yr, stop.fishing.yr = stop.fishing.yr, rescale.catch = rescale.catch, random.Rkrill = random.Rkrill, sd.krill.Rdev = sd.krill.Rdev,
          	fishing.option = fishing.option, actual.gamma = actual.gamma, precautionary.limit.mmt = precautionary.limit.mmt, allocation.mult = allocation.mult, allocation.CV = allocation.CV, B0.mult = B0.mult, B0.CV = B0.CV, reassessment.interval.yr = reassessment.interval.yr,
            safe.mode = safe.mode, annual.plots = annual.plots, annual.connector = annual.connector, Rylimits =	Rylimits, Nylimits = Nylimits,
            Cylimits = Cylimits, page.layout = page.layout, point.size = point.size, single.sim = FALSE, ss.seed = NULL)
    #
    krill[,  , j] <- tt$N$krill
    seals[,  , j] <- tt$N$seals
		pengs[,  , j] <- tt$N$pengs
		whales[,  , j] <- tt$N$whales
		fish[,  , j] <- tt$N$fish
		Qseals[,  , j] <- tt$Q$demand$seals
		Qpengs[,  , j] <- tt$Q$demand$pengs
		Qwhales[,  , j] <- tt$Q$demand$whales
		Qfish[,  , j] <- tt$Q$demand$fish
		Qseals.performance[,  , j] <- tt$Q$consumption$seals
		Qpengs.performance[,  , j] <- tt$Q$consumption$pengs
		Qwhales.performance[,  , j] <- tt$Q$consumption$whales
		Qfish.performance[,  , j] <- tt$Q$consumption$fish
		Rkrill[,  , j] <- tt$R$R$krill
		Rseals[,  , j] <- tt$R$R$seals
		Rpengs[,  , j] <- tt$R$R$pengs
		Rwhales[,  , j] <- tt$R$R$whales
		Rfish[,  , j] <- tt$R$R$fish
		catch[,  , j] <- tt$fishery$catch
		earmarks[,  , j] <- tt$fishery$allocation
		threshold.violations[j,] <- tt$fishery$threshold.violations
    wbars[, , j] <- tt$wbar
	}
	# put the original bathtub parameters back into the parameter list
	param.list$BATHTUB.ABUNDANCE <- orig.bathtubs
	#
	mc.out <- list(call = cl, setup=list(ntrials = ntrials, ntimes=ntimes, nseasons=param.list$NSEASONS, nareas=nareas, nssmus=nssmus, ntubs=param.list$NTUBS, init.season=param.list$INIT.SEASON, start.fishing=start.fishing, stop.fishing=stop.fishing), N=list(krill=krill, seals=seals, pengs=pengs, whales=whales, fish=fish), R=list(R=list(krill=Rkrill, seals=Rseals, pengs=Rpengs, whales=Rwhales, fish=Rfish),
    maxRage=list(krill=krill.Rage, seals=seals.maxRage, pengs=pengs.maxRage, whales=whales.maxRage, fish=fish.maxRage)),
    Q=list(demand=list(seals=Qseals, pengs=Qpengs, whales=Qwhales, fish=Qfish), consumption=list(seals=Qseals.performance, pengs=Qpengs.performance, whales=Qwhales.performance, fish=Qfish.performance)), fishery=list(allocation=earmarks, catch=catch, threshold.violations=threshold.violations), wbar=wbars)
  #
	if(plot.now) {
	  if(!is.null(fishing.option)){
		  if(fishing.option == 6) {
			  point.size <- 0.75
		  }
	  }
		# plot krill
		plot.mc.N(mc.out, spp.string = "krill", quants = quants, plot.trials = plot.trials, annual.plots = annual.plots, connector = annual.connector, page.layout = page.layout, Nylimits = Nylimits, color.tags = NULL)
		plot.mc.R(mc.out, spp.string = "krill", quants = quants, plot.trials = plot.trials, annual.plots = annual.plots, connector = annual.connector, page.layout = page.layout, Rylimits = Rylimits, color.tags = NULL)
		#
		# plot seals
		plot.mc.N(mc.out, spp.string = "seals", quants = quants, plot.trials = plot.trials, annual.plots = annual.plots, connector = annual.connector, page.layout = page.layout, Nylimits = Nylimits, color.tags = NULL)
		plot.mc.R(mc.out, spp.string = "seals", quants = quants, plot.trials = plot.trials, annual.plots = annual.plots, connector = annual.connector, page.layout = page.layout, Rylimits = Rylimits, color.tags = NULL)
		#
		# plot pengs
		plot.mc.N(mc.out, spp.string = "pengs", quants = quants, plot.trials = plot.trials, annual.plots = annual.plots, connector = annual.connector, page.layout = page.layout, Nylimits = Nylimits, color.tags = NULL)
		plot.mc.R(mc.out, spp.string = "pengs", quants = quants, plot.trials = plot.trials, annual.plots = annual.plots, connector = annual.connector, page.layout = page.layout, Rylimits = Rylimits, color.tags = NULL)
		#
		# plot whales
		plot.mc.N(mc.out, spp.string = "whales", quants = quants, plot.trials = plot.trials, annual.plots = annual.plots, connector = annual.connector, page.layout = page.layout, Nylimits = Nylimits, color.tags = NULL)
		plot.mc.R(mc.out, spp.string = "whales", quants = quants, plot.trials = plot.trials, annual.plots = annual.plots, connector = annual.connector, page.layout = page.layout, Rylimits = Rylimits, color.tags = NULL)
		#
		# plot fish
		plot.mc.N(mc.out, spp.string = "fish", quants = quants, plot.trials = plot.trials, annual.plots = annual.plots, connector = annual.connector, page.layout = page.layout, Nylimits = Nylimits, color.tags = NULL)
		plot.mc.R(mc.out, spp.string = "fish", quants = quants, plot.trials = plot.trials, annual.plots = annual.plots, connector = annual.connector, page.layout = page.layout, Rylimits = Rylimits, color.tags = NULL)
		#
		# plot catch
		plot.mc.C(mc.out, quants = quants, plot.trials = plot.trials, annual.plots = annual.plots, connector = annual.connector, page.layout = page.layout, Cylimits = Cylimits, color.tags = NULL)
	}
	#
	mc.out
}
