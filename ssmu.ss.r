ssmu.ss<-function(param.list, saved.state = NULL, GGP=NULL, nyears = 38, start.fishing.yr =1, stop.fishing.yr = NULL, rescale.catch = FALSE, random.Rkrill = FALSE, sd.krill.Rdev = 0.4,
	use.R.bias.correction=FALSE, fishing.option = NULL, actual.gamma = 0.093, precautionary.limit.mmt = NULL, allocation.mult = 1., allocation.CV = 0.2, B0.mult = 1., B0.CV = 0.2, reassessment.interval.yr = NULL,
  safe.mode = TRUE, annual.plots = TRUE, annual.connector = "default", Rylimits =	c(0, 3), Nylimits = c(0, 3), Cylimits = c(0, 3), page.layout = c(4, 4), point.size = 0.001, single.sim = TRUE, suppress.messages=FALSE, ss.seed = 123, kitchen.sink=FALSE)
{
	if(kitchen.sink){
	  plot.kitchen.sink("c:/work/kpfm2/r/kitchensink.pgm")
	  stop()
  }
  # George Watters
	# edited 23 February 2008 by GMW -- adjusted initial conditions to deal with winter foraging performance
  # edited 7 May 2008 by GMW -- fixed indexing problem in B0 arguments to allocate.catch() when doing reassessments
  # edited 8 May 2008 by GMW -- fixed code that set up initial conditions when starting from saved end state
  # edited 4 February 2008 by JTH
  # edited 20 June 2013 by JTH to enable SSMU-specific ENV indices
  # edited 30 July 2013 by GMW and JTH to enable SSMU- and time-specific krill weights to drive model
	# Delay-difference predator-prey model to examine possible outcomes of allocating
	# precautionary krill catches among SSMUs in Atlantic sector of Southern Ocean
	# -- should be generalizable to other situations
	#
	# "ss" indicates "single simulation"
	#
	# new code to facilitate seasonal variation in consumption, catch, movement, and recruitment
	#
	# also made substantive changes to organization of code to allow predators to forage in any SSMU
	# even though they breed in a "home SSMU" and to allow user to specify an abitrary matrix
	# (catch.setup) with rows = time periods (usually seasons) and columns = SSMUs
	#
	# planned new functionality
	# DONE (facilitates simple MSE) 1) modularize code for setting up the catches
	# 2) checking dimensions of input vectors, matrices, etc. that can be turned off for Monte Carlo
	# DONE -- 3) add possibility to initialize predator abundance from census data rather than estimates of demand
	# DONE -- 4) build ability to save end state (e.g., an equilibrium) and start further simulations from an end.state object (for facilitating MC)
	# DONE -- 5) use set.seed() to get same set of random deviates
	# DONE -- 6) set fishing mortality = 0 if krill density gets below some threshold
	# DONE -- 7) for fishing option 5 sample from a user-selected distribution of krill densities
	# DONE -- 8) pass the call and other stuff into the results for future use
	# 9) add multipliers so that reporting, etc. can be done in e.g., millions of krill
	#
	cl <- match.call()
	#
  # READ IN AND PARSE OUT STUFF FROM THE INPUT LIST
	krill.params.list <- param.list$KRILL
  seals.params.list <- param.list$SEALS
  pengs.params.list <- param.list$PENGS
  whales.params.list <- param.list$WHALES
  fish.params.list <- param.list$FISH
  competition.matrix <- param.list$COMPETITION.MATRIX
  areas <- param.list$AREAS
  nseasons <- param.list$NSEASONS
  init.season <- param.list$INIT.SEASON
  available.fraction <- param.list$AVAILABLE.FRACTION
  catch.setup <- param.list$CATCH.SETUP
  threshold.density <- param.list$THRESHOLD.DENSITY
  v.array <- param.list$V.ARRAY
  ntubs <- param.list$NTUBS
  bathtub.abundance <- param.list$BATHTUB.ABUNDANCE
  krill.Rage <- param.list$KRILL.RAGE
  env.index <- param.list$ENV.INDEX
  historical.catch <- param.list$HISTORICAL.CATCH
  option5.list <- param.list$OPTION5.LIST
  #
  nareas <- dim(v.array)[1]
	nssmus <- nareas - ntubs
	#
	# if available.fraction is a scalar apply it to all areas
	# if it is a vector of length less than nareas then stop
	# and tell the user to get their act together
	if(length(available.fraction) == 1) {
		available.fraction <- rep(available.fraction, nareas)
	}
	if(length(available.fraction) > 1 && length(available.fraction) < nareas) {
		stop("Sionara -- you need to specify either 1 available.fraction or have as many as there are areas (SSMUs + tubs)")
	}
  # set up fishing thresholds
  if(length(threshold.density) == 1) {
		threshold.density <- rep(threshold.density, nssmus)
	}
	if(length(threshold.density) > 1 && length(threshold.density) < nssmus) {
		stop("Sionara -- you need to specify either 1 threshold.density or have as many as there are SSMUs")
	}
	#
	# get maximum recruitment ages for predators
	# recruitment age is converted from units of years to seasons
	# krill are assumed to recruit to fisheries and predators
	# at the same age no matter what ssmu they are in
	# initiate the model at the TIME OF REPRODUCTION
  tt.age <- sapply(seals.params.list,FUN=function(x){x$Rage})
  if(all(is.na(tt.age))){seals.maxRage <- 0}else{seals.maxRage<-max(tt.age,na.rm=TRUE)}
  seals.maxRage <- (seals.maxRage * nseasons)
	#
  tt.age <- sapply(pengs.params.list,FUN=function(x){x$Rage})
  if(all(is.na(tt.age))){pengs.maxRage <- 0}else{pengs.maxRage<-max(tt.age,na.rm=TRUE)}
	pengs.maxRage <- (pengs.maxRage * nseasons)
  #
  tt.age <- sapply(whales.params.list,FUN=function(x){x$Rage})
  if(all(is.na(tt.age))){whales.maxRage <- 0}else{whales.maxRage<-max(tt.age,na.rm=TRUE)}
	whales.maxRage <- (whales.maxRage * nseasons)
	#
  tt.age <- sapply(fish.params.list,FUN=function(x){x$Rage})
  if(all(is.na(tt.age))){fish.maxRage <- 0}else{fish.maxRage<-max(tt.age,na.rm=TRUE)}
	fish.maxRage <- (fish.maxRage * nseasons)
	#
	krill.Rage <- (krill.Rage * nseasons)
	#
	# now initialize a bunch of stuff
	#
	ntimes <- nyears * nseasons
  start.fishing <- (start.fishing.yr * nseasons) - nseasons + 1
  if(!is.null(stop.fishing.yr)){
    stop.fishing <- (stop.fishing.yr * nseasons) - nseasons + 1
  }
  else {
    stop.fishing <- ntimes + 1
  }
  #
  if(!is.null(reassessment.interval.yr) && (fishing.option!=6)){
    reassessment.interval <- reassessment.interval.yr * nseasons
    # note could have reassessments start only if there is fishing,
    # but here we allow reassessments to occur prior to onset of fishing
    # to try and "do better" before fishing starts -- be careful, however,
    # because reassessment times will be computed from the beginning of the simulation
    # not from start.fishing
    reassessment.times <- seq(from=reassessment.interval,to=ntimes,by=reassessment.interval)
    reassessment.times <- reassessment.times[(reassessment.times < stop.fishing)]
    reassessment.times <- reassessment.times[(reassessment.times < ntimes)]
    if(length(reassessment.times)>100){stop("FAULT:  Please increase interval between reassessments or shorten length of simulation.\nMax number of reassessments is 100")}
  }
  else {
    reassessment.times <- NULL
  }
  #print(reassessment.times)
	#
	# add a few years to the top of the matrix to have spawners
	# back in time
	krill <- matrix(0, nrow = ntimes + krill.Rage, ncol = nareas)
	#
	# the bathtubs are always the last areas
	# fill the bathtubs
	# note that you can pass constants or vectors with length ntimes in the list
	# be careful about the order of elements in the list
	# rates of input from the bathtub to the ssmus are provided in v.array
	for(i in 1:ntubs) {
	  # JTH -- trap to stop simulation if time series parameter setup is faulty
	  #if(length(bathtub.abundance[[i]])>1){
	  #  if(length(bathtub.abundance[[i]])!=ntimes) {
	  #    stop(cat("Error in setup of bathtub abundances.  Ensure length(tub", i, ") == nseasons * nyears"))
	  #  }
    #} # END OF TRAP
    #krill[(krill.Rage + 1):(krill.Rage + ntimes), nssmus + i] <- bathtub.abundance[[i]]
    #
    # if bathtub abundance is a matrix only use the first column (matrices
    # are now reserved for MC runs in which each column provides the bathtub
    # abundance for one trial).
    # also, instead of trapping just force bathtub abundances to be the correct length
    # this will recycle as necessary to fill up ntimes
    if(is.matrix(bathtub.abundance[[i]])){
      tt <- rep(bathtub.abundance[[i]][,1],length.out=ntimes)
    } else {
      tt <- rep(bathtub.abundance[[i]],length.out=ntimes)
    }
		krill[(krill.Rage + 1):(krill.Rage + ntimes), nssmus + i] <- tt
		krill[1:krill.Rage, nssmus+i] <- tt[1]
	}
	#
	# add a few years to top of matrices to account for
	# lags to recruitment
	seals <- matrix(0, nrow = ntimes + seals.maxRage, ncol = nssmus)
	pengs <- matrix(0, nrow = ntimes + pengs.maxRage, ncol = nssmus)
	whales <- matrix(0, nrow = ntimes + whales.maxRage, ncol = nssmus)
	fish <- matrix(0, nrow = ntimes + fish.maxRage, ncol = nssmus)
	#
	# Q is per-capita demand for prey by predators breeding in area i
	# note that predators can forage in bathtubs, but this doesn't affect prey dynamics in the tubs
	Qseals <- matrix(0, nrow = ntimes + seals.maxRage, ncol = nssmus)
	Qpengs <- matrix(0, nrow = ntimes + pengs.maxRage, ncol = nssmus)
	Qwhales <- matrix(0, nrow = ntimes + whales.maxRage, ncol = nssmus)
	Qfish <- matrix(0, nrow = ntimes + fish.maxRage, ncol = nssmus)
 	#
  Qseals.performance <- Qseals
  Qpengs.performance <- Qpengs
  Qwhales.performance <- Qwhales
  Qfish.performance <- Qfish
  #
	# recruitment matrices
	Rkrill <- matrix(0, nrow = ntimes, ncol = nssmus)
	Rseals <- matrix(0, nrow = ntimes, ncol = nssmus)
	Rpengs <- matrix(0, nrow = ntimes, ncol = nssmus)
	Rwhales <- matrix(0, nrow = ntimes, ncol = nssmus)
	Rfish <- matrix(0, nrow = ntimes, ncol = nssmus)
	#
	# v.array is the array instantaneous movement
	# rates from ssmu i to ssmu j during season k
	# make sure each seasonal matrix in v.array has ZEROS ON THE DIAGONAL or
	# you will have too much mortality
	# V is the total, ssmu-specific instantaneous rate of succumbing
	# to movement (moving out of the ssmu that you are currently in)
	# V has rows identifying areas and columns identifying seasons
	V <- apply(v.array,3,rowSums)
	#
	# setup all possible random numbers -- even if don't use them because
	# need to be able to generate the same sets of random numbers with set.seed()
	#
	if(single.sim){
    set.seed(ss.seed)
    # else the seed will be set in the call to ssmu.mc()
  }
  # first sample recruitment deviates for krill
  krill.Rdev <- rnorm(ntimes, mean =  -(sd.krill.Rdev^2)/2, sd = sd.krill.Rdev)
  # then sample stuff for fishing option 5
  # first the densities for the prior observation period
  # here I generate more random numbers than I should ever need so that the set of numbers
  # will always be the same for a given seed
  tt.density <- matrix(0, nrow = 100, ncol = nareas)
  # then the relQdevs for the prior observation period
  relQdev1 <- matrix(0, nrow = 100, ncol=nssmus)
  # then the relQdevs for the simulation period
  relQdev2 <- matrix(0, nrow = ntimes, ncol = nssmus)
  for(i in 1:nareas) {
    # tt.density is developed from a user input probability model (one per area)
    tt.density[, i] <- eval(parse(text=option5.list$obs.density.dists[i]))
    if(!is.na(option5.list$monitored.spp[i]) && i <= nssmus){
      relQdev1[, i] <- rnorm(n = dim(relQdev1)[1], mean = 0, sd = option5.list$obs.sd[i])
      relQdev2[, i] <- rnorm(n = ntimes, mean = 0, sd = option5.list$obs.sd[i])
    }
    else {
      if(i <= nssmus){
        relQdev1[, i] <- rnorm(n = dim(relQdev1)[1])
        relQdev2[, i] <- rnorm(n = ntimes)
      }
    }
  }
  #
	# setup vectors for krill recruitment so don't have to do this inside loops
	if(!random.Rkrill) {
  	krill.Rdev <- rep(0, ntimes)
	}
	#
  if(is.null(env.index)) {
    krill.Renv <- rep(0, ntimes)
  }
  else {
    krill.Renv <- rep(env.index, length.out=ntimes)
  }
  # now compute the bias corrections so that if mean of anomaly series is zero
  # a model with env forcing will have same mean R as model without env forcing
  # bias correction from Maunder and Watters (2002)
  bias.correction<-matrix(0,nrow=nseasons, ncol=nssmus)
  if(use.R.bias.correction){
  if(!all(krill.Renv==0)){
    tt<-rep(1:nseasons,length.out=ntimes)
    for(i in 1:nssmus){
      for(j in 1:nseasons){
        keep.me <- (tt==j)
        bias.correction[j,i]<-log(sum(keep.me)/sum(exp(krill.Rdev[keep.me]+(krill.Renv[keep.me]*krill.params.list[[i]]$Rphi[j]))))
      }
    }
  }
  }
  #
	# rescale the foraging matrices so rows sum to 1
	seals.foraging.matrix <- vector(mode="list",length=nssmus)
	pengs.foraging.matrix <- vector(mode="list",length=nssmus)
	whales.foraging.matrix <- vector(mode="list",length=nssmus)
	fish.foraging.matrix <- vector(mode="list",length=nssmus)
	
	for(i in 1:nssmus){
	  tt.sum <- apply(seals.params.list[[i]]$foraging.matrix,1,sum)
    seals.foraging.matrix[[i]] <-  sweep(seals.params.list[[i]]$foraging.matrix,1,tt.sum,"/")
    # if there is no foraging then the rescaled matrices would be NaN so set these = 0 now too
    seals.foraging.matrix[[i]][is.nan(seals.foraging.matrix[[i]])]<-0
    tt.sum <- apply(pengs.params.list[[i]]$foraging.matrix,1,sum)
    pengs.foraging.matrix[[i]] <-  sweep(pengs.params.list[[i]]$foraging.matrix,1,tt.sum,"/")
    pengs.foraging.matrix[[i]][is.nan(pengs.foraging.matrix[[i]])]<-0
    tt.sum <- apply(whales.params.list[[i]]$foraging.matrix,1,sum)
    whales.foraging.matrix[[i]] <-  sweep(whales.params.list[[i]]$foraging.matrix,1,tt.sum,"/")
    whales.foraging.matrix[[i]][is.nan(whales.foraging.matrix[[i]])]<-0
    tt.sum <- apply(fish.params.list[[i]]$foraging.matrix,1,sum)
    fish.foraging.matrix[[i]] <-  sweep(fish.params.list[[i]]$foraging.matrix,1,tt.sum,"/")
    fish.foraging.matrix[[i]][is.nan(fish.foraging.matrix[[i]])]<-0
  }
  # setup observation errors for initial conditions and the reassessment-reallocation process
  sd.allocation.errors <- sqrt(log((allocation.CV^2)+1))
  allocation.errors <- matrix(rnorm(101*nssmus, mean = -(sd.allocation.errors^2)/2, sd = sd.allocation.errors), nrow=101, ncol=nssmus)
  sd.B0.errors <- sqrt(log((B0.CV^2)+1))
  B0.errors <- rnorm(101, mean = -(sd.B0.errors^2)/2, sd = sd.B0.errors)
  #
	# loop over areas to set up initial conditions within each area
	# density should be in grams per sq. meter
	init.krill.biomass <- numeric(nssmus)
	init.krill.density <- numeric(nareas)
	init.seals.demand <- numeric(nssmus)
	init.pengs.demand <- numeric(nssmus)
	init.whales.demand <- numeric(nssmus)
	init.fish.demand <- numeric(nssmus)
	wbar.vector <- as.vector(sapply(krill.params.list,FUN=function(x){x$wbar}))
  #
### Basic set up is to here###
	# NOTE WE ARE NOT ASSUMING THAT THE MODEL IS INITIALIZED AT SOME EQUILIBIRIUM
	# THUS, THE DYNAMIC, RELATIVE TRAJECTORIES WILL GENERALLY TREND AWAY FROM 1.0
	# UNLESS THE PARAMETERS ARE APPROPRIATELY TUNED
	# THE PROBLEM HERE IS THAT TO GET THE FIRST FEW RECRUITMENTS RIGHT
	# WE DON'T KNOW WHAT CONSUMPTION RATES AND DENSITIES OCCURED
	# IN THE PAST -- FOR NOW I ASSUME THAT IT'S JUST EQUAL TO OUR INITIALIZATION VALUES
	# IT WOULD BE NICE TO DRAW THESE FROM A DISTRIBUTION OR HAVE SOME MODEL THAT
	# GIVES US SOME REASONABLE EXPECTATION -- I'VE TRIED ALL SORTS OF EQUILIBRIUM ASSUMPTIONS
	# THAT I REALLY COULDN'T GET TO WORK, AND DIDN'T LIKE ANYHOW,
	# BUT THEN IS THAT REALLY ANY DIFFERENT THAN ASSUMING THE INIT CONDITIONS EXTENDED BACK INTO
	# THE PAST A FEW YEARS?  THIS MIGHT BE A GOOD TOPIC FOR FUTURE WORK
	#
	# first need to do krill so can get density and compute the avg. density that foraging predators encounter
	if(is.null(saved.state)){
	  for(i in 1:(nssmus)) {
		  krill[1:krill.Rage, i] <- (krill.params.list[[i]]$init.density * areas[i])/krill.params.list[[i]]$wbar
		}
		init.krill.density <- (krill[1,]*wbar.vector)/areas
	} else {
	 # first a little fail safe to ensure that continuing simulations use the same
	 # parameter set as was used to generate the saved state
	 if(single.sim && safe.mode){
     if(cl$param.list != saved.state$call$param.list) stop("FAULT -- the continuing simulation does not have the same parameter set as the saved state!")
   }
	 # read in stuff from saved end state
    krill.dim <- dim(saved.state$N$krill)[1]
		krill[1:krill.Rage, ] <- saved.state$N$krill[(krill.dim-krill.Rage+1):krill.dim, ]
		init.krill.density <- (krill[krill.Rage, ]*wbar.vector)/areas
	}
	#
	# now the predators (mostly)
	if(is.null(saved.state)){
	if(init.season==1) {funky.season <- 1:nseasons} else {funky.season <- c(init.season:nseasons,1:(init.season-1))}
	for(i in 1:(nssmus)) {
		# per-capita demand for krill (in numbers) by predators
		if(seals.params.list[[i]]$init.value > 0){
      tt.Q<-numeric(nseasons)
      for(j in 1:nseasons){
        tt.init <- sum(init.krill.density * seals.params.list[[i]]$foraging.matrix[funky.season[j],])
        tt.Q[j] <- (seals.params.list[[i]]$QQmax[funky.season[j]] * (tt.init^(
  			 seals.params.list[[i]]$Qq[funky.season[j]] + 1)))/((seals.params.list[[i]]$Qk5[funky.season[j]]^(seals.params.list[[i]]$Qq[funky.season[j]] +
  			 1)) + (tt.init^(seals.params.list[[i]]$Qq[funky.season[j]] + 1)))
      }
      Qseals[1:seals.maxRage, i]<-rep(tt.Q,length=length(1:seals.maxRage))
		}
		if(pengs.params.list[[i]]$init.value > 0){
      tt.Q<-numeric(nseasons)
      for(j in 1:nseasons){
        tt.init <- sum(init.krill.density * pengs.params.list[[i]]$foraging.matrix[funky.season[j],])
        tt.Q[j] <- (pengs.params.list[[i]]$QQmax[funky.season[j]] * (tt.init^(
  			 pengs.params.list[[i]]$Qq[funky.season[j]] + 1)))/((pengs.params.list[[i]]$Qk5[funky.season[j]]^(pengs.params.list[[i]]$Qq[funky.season[j]] +
  			 1)) + (tt.init^(pengs.params.list[[i]]$Qq[funky.season[j]] + 1)))
      }
      Qpengs[1:pengs.maxRage, i]<-rep(tt.Q,length=length(1:pengs.maxRage))
		}
    if(whales.params.list[[i]]$init.value > 0){
      tt.Q<-numeric(nseasons)
      for(j in 1:nseasons){
        tt.init <- sum(init.krill.density * whales.params.list[[i]]$foraging.matrix[funky.season[j],])
        tt.Q[j] <- (whales.params.list[[i]]$QQmax[funky.season[j]] * (tt.init^(
  			 whales.params.list[[i]]$Qq[funky.season[j]] + 1)))/((whales.params.list[[i]]$Qk5[funky.season[j]]^(whales.params.list[[i]]$Qq[funky.season[j]] +
  			 1)) + (tt.init^(whales.params.list[[i]]$Qq[funky.season[j]] + 1)))
      }
      Qwhales[1:whales.maxRage, i]<-rep(tt.Q,length=length(1:whales.maxRage))
		}
    if(fish.params.list[[i]]$init.value > 0){
      tt.Q<-numeric(nseasons)
      for(j in 1:nseasons){
        tt.init <- sum(init.krill.density * fish.params.list[[i]]$foraging.matrix[funky.season[j],])
        tt.Q[j] <- (fish.params.list[[i]]$QQmax[funky.season[j]] * (tt.init^(
  			 fish.params.list[[i]]$Qq[funky.season[j]] + 1)))/((fish.params.list[[i]]$Qk5[funky.season[j]]^(fish.params.list[[i]]$Qq[funky.season[j]] +
  			 1)) + (tt.init^(fish.params.list[[i]]$Qq[funky.season[j]] + 1)))
      }
      Qfish[1:fish.maxRage, i]<-rep(tt.Q,length=length(1:fish.maxRage))
		}
		#
		# now initialize total predator demand and abundance
		#
    # if init.value is demand (init.type=="Q") then ...
    # 1) units should be in biomass (grams) and demand is identified by BREEDING AREA
    # 2) transform demand to numbers to get resulting predator abundance correct
    # 3) compute initial predator abundance as total initial demand/per capita demand
    # 4) transform demand back to biomass to get catch allocation correct
    #
    # if init.value is abundance (init.type=="N") then ...
    # 1) abundance should be identified by BREEDING AREA
    # 2) compute initial demand for krill (in numbers)
    # 3) transform initial demand to biomass to get catch allocation correct
		if(seals.params.list[[i]]$init.value > 0){
      if(seals.params.list[[i]]$init.type=="Q"){
  		  init.seals.demand[i] <- seals.params.list[[i]]$init.value/krill.params.list[[i]]$wbar
  		  seals[1:seals.maxRage,i] <- init.seals.demand[i]/Qseals[1,i]
  		}
  		else {
  		  if(seals.params.list[[i]]$init.type=="N"){
  		    seals[1:seals.maxRage,i] <- seals.params.list[[i]]$init.value
  		    init.seals.demand[i] <- seals[1,i]*Qseals[1,i]
        }
        else {stop(cat("init.type for seals that breed in SSMU ", i, " must be Q (demand) or N (abundance)",sep="",fill=TRUE))}
      }
    }
    #
    if(pengs.params.list[[i]]$init.value > 0){
  		if(pengs.params.list[[i]]$init.type=="Q"){
        init.pengs.demand[i] <- pengs.params.list[[i]]$init.value/krill.params.list[[i]]$wbar
  		  pengs[1:pengs.maxRage,i] <- init.pengs.demand[i]/Qpengs[1,i]
      }
      else {
  		  if(pengs.params.list[[i]]$init.type=="N"){
  		    pengs[1:pengs.maxRage,i] <- pengs.params.list[[i]]$init.value
  		    init.pengs.demand[i] <- pengs[1,i]*Qpengs[1,i]
        }
        else {stop(cat("init.type for pengs that breed in SSMU ", i, " must be Q (demand) or N (abundance)",sep="",fill=TRUE))}
      }
    }
    #
    if(whales.params.list[[i]]$init.value > 0){
      if(whales.params.list[[i]]$init.type=="Q"){
        init.whales.demand[i] <- whales.params.list[[i]]$init.value/krill.params.list[[i]]$wbar
        whales[1:whales.maxRage,i] <- init.whales.demand[i]/Qwhales[1,i]
  		}
  		else {
  		  if(whales.params.list[[i]]$init.type=="N"){
  		    whales[1:whales.maxRage,i] <- whales.params.list[[i]]$init.value
  		    init.whales.demand[i] <- whales[1,i]*Qwhales[1,i]
        }
        else {stop(cat("init.type for whales that breed in SSMU ", i, " must be Q (demand) or N (abundance)",sep="",fill=TRUE))}
      }
    }
		#
		if(fish.params.list[[i]]$init.value > 0){
  		if(fish.params.list[[i]]$init.type=="Q"){
        init.fish.demand[i] <- fish.params.list[[i]]$init.value/krill.params.list[[i]]$wbar
        fish[1:fish.maxRage,i] <- init.fish.demand[i]/Qfish[1,i]
  		}
  		else {
  		  if(fish.params.list[[i]]$init.type=="N"){
  		    fish[1:fish.maxRage,i] <- fish.params.list[[i]]$init.value
  		    init.fish.demand[i] <- fish[1,i]*Qfish[1,i]
        }
        else {stop(cat("init.type for seals that breed in SSMU ", i, " must be Q (demand) or N (abundance)",sep="",fill=TRUE))}
      }
    }
    # now compute init krill biomass
		# units are grams
		init.krill.biomass[i] <- krill.params.list[[i]]$init.density * areas[i]
	}
	#stop() 
  #
	# Also track Qperformance across foraging areas for use in the predator recruitment functions
	# this is the mean per-capita consumption achieved by predators breeding in area i
	Qseals.performance <- Qseals
	Qpengs.performance <- Qpengs
	Qwhales.performance <- Qwhales
	Qfish.performance <- Qfish
  #
	# now convert demand back to biomass for finding the catch allocation
	# the following is demand by breeding area and will convert to demand by feeding area
	init.seals.demand <- init.seals.demand * wbar.vector[1:nssmus]
	init.pengs.demand <- init.pengs.demand * wbar.vector[1:nssmus]
	init.whales.demand <- init.whales.demand * wbar.vector[1:nssmus]
	init.fish.demand <- init.fish.demand * wbar.vector[1:nssmus]
## SECOND CHECK IS TO HERE ##	
	# convert to demand by feeding area -- since this is the true basis of the catch allocation scheme
	# sumQseals.parsed etc. is used later -- I'm just recycling these to save memory
  sumQseals.parsed<-matrix(0,nrow=nssmus,ncol=nareas)
	sumQpengs.parsed<-matrix(0,nrow=nssmus,ncol=nareas)
	sumQwhales.parsed<-matrix(0,nrow=nssmus,ncol=nareas)
	sumQfish.parsed<-matrix(0,nrow=nssmus,ncol=nareas)
  for(i in 1:nssmus){
    if(seals.params.list[[i]]$init.value > 0){
      sumQseals.parsed[i,] <- init.seals.demand[i]*seals.foraging.matrix[[i]][init.season,]
    }
    if(pengs.params.list[[i]]$init.value > 0){
      sumQpengs.parsed[i,] <- init.pengs.demand[i]*pengs.foraging.matrix[[i]][init.season,]
    }
    if(whales.params.list[[i]]$init.value > 0){
      sumQwhales.parsed[i,] <- init.whales.demand[i]*whales.foraging.matrix[[i]][init.season,]
    }
    if(fish.params.list[[i]]$init.value > 0){
      sumQfish.parsed[i,] <- init.fish.demand[i]*fish.foraging.matrix[[i]][init.season,]
    }
  }
	init.seals.demand <- colSums(sumQseals.parsed)
  init.seals.demand <- ifelse(is.na(init.seals.demand),0,init.seals.demand)
  init.pengs.demand <- colSums(sumQpengs.parsed)
  init.pengs.demand <- ifelse(is.na(init.pengs.demand),0,init.pengs.demand)
  init.whales.demand <- colSums(sumQwhales.parsed)
  init.whales.demand <- ifelse(is.na(init.whales.demand),0,init.whales.demand)
  init.fish.demand <- colSums(sumQfish.parsed)
  init.fish.demand <- ifelse(is.na(init.fish.demand),0,init.fish.demand)
  init.tot.demand <- init.seals.demand + init.pengs.demand + init.whales.demand + init.fish.demand
  # we only need to allocate based on demand in the SSMUs so drop the bathtubs
  init.tot.demand <- init.tot.demand[1:nssmus]
  #stop()
	#
	B0 <- sum(init.krill.biomass)
	#
### SET UP CHECK TO HERE ###
	} else {
	 # first a little fail safe to ensure that continuing simulations use the same
	 # parameter set as was used to generate the saved state
	 if(single.sim && safe.mode){
     if(cl$param.list != saved.state$call$param.list) stop("FAULT -- the continuing simulation does not have the same parameter set as the saved state!")
   }
	 # read in stuff from saved end state
   #
    seals.dim <- dim(saved.state$Q$consumption$seals)[1]
		pengs.dim <- dim(saved.state$Q$consumption$pengs)[1]
		whales.dim <- dim(saved.state$Q$consumption$whales)[1]
		fish.dim <- dim(saved.state$Q$consumption$fish)[1]
		#
    seals[1:seals.maxRage, ] <- saved.state$N$seals[(seals.dim-seals.maxRage+1):seals.dim, ]
    Qseals[1:seals.maxRage, ] <- saved.state$Q$demand$seals[(seals.dim-seals.maxRage+1):seals.dim, ]
    Qseals.performance[1:seals.maxRage, ] <- saved.state$Q$consumption$seals[(seals.dim-seals.maxRage+1):seals.dim, ]
    # initial demand is measured in the corresponding season of the previous year...
		init.seals.demand <- seals[seals.maxRage-nseasons+1, ] * Qseals[seals.maxRage-nseasons+1, ]
		#
    pengs[1:pengs.maxRage, ] <- saved.state$N$pengs[(pengs.dim-pengs.maxRage+1):pengs.dim, ]
    Qpengs[1:pengs.maxRage, ] <- saved.state$Q$demand$pengs[(pengs.dim-pengs.maxRage+1):pengs.dim, ]
    Qpengs.performance[1:pengs.maxRage, ] <- saved.state$Q$consumption$pengs[(pengs.dim-pengs.maxRage+1):pengs.dim, ]
    # initial demand is measured in the corresponding season of the previous year...
		init.pengs.demand <- pengs[pengs.maxRage-nseasons+1, ] * Qpengs[pengs.maxRage-nseasons+1, ]
    #
    whales[1:whales.maxRage, ] <- saved.state$N$whales[(whales.dim-whales.maxRage+1):whales.dim, ]
    Qwhales[1:whales.maxRage, ] <- saved.state$Q$demand$whales[(whales.dim-whales.maxRage+1):whales.dim, ]
    Qwhales.performance[1:whales.maxRage, ] <- saved.state$Q$consumption$whales[(whales.dim-whales.maxRage+1):whales.dim, ]
    # initial demand is measured in the corresponding season of the previous year...
		init.whales.demand <- whales[whales.maxRage-nseasons+1, ] * Qwhales[whales.maxRage-nseasons+1, ]
    #
    fish[1:fish.maxRage, ] <- saved.state$N$fish[(fish.dim-fish.maxRage+1):fish.dim, ]
    Qfish[1:fish.maxRage, ] <- saved.state$Q$demand$fish[(fish.dim-fish.maxRage+1):fish.dim, ]
    Qfish.performance[1:fish.maxRage, ] <- saved.state$Q$consumption$fish[(fish.dim-fish.maxRage+1):fish.dim, ]
    # initial demand is measured in the corresponding season of the previous year...
		init.fish.demand <- fish[fish.maxRage-nseasons+1, ] * Qfish[fish.maxRage-nseasons+1, ]
    #
    #
    # now compute init krill biomass and density
		# units are, respectively, grams and grams per sq. meter
		init.krill.biomass <- krill[1, 1:nssmus] * wbar.vector[1:nssmus]
    #init.krill.density <- init.krill.biomass / areas[1:nssmus]
   	#
    # now convert demand back to biomass for finding the catch allocation
   	# the following is demand by breeding area and will convert to demand by feeding area
	  init.seals.demand <- init.seals.demand * wbar.vector[1:nssmus]
    init.pengs.demand <- init.pengs.demand * wbar.vector[1:nssmus]
	  init.whales.demand <- init.whales.demand * wbar.vector[1:nssmus]
	  init.fish.demand <- init.fish.demand * wbar.vector[1:nssmus]
	  # convert to demand by feeding area -- since this is the true basis of the catch allocation scheme
	  # sumQseals.parsed etc. is used later -- I'm just recycling these to save memory
    sumQseals.parsed<-matrix(0,nrow=nssmus,ncol=nareas)
	  sumQpengs.parsed<-matrix(0,nrow=nssmus,ncol=nareas)
	  sumQwhales.parsed<-matrix(0,nrow=nssmus,ncol=nareas)
	  sumQfish.parsed<-matrix(0,nrow=nssmus,ncol=nareas)
    for(i in 1:nssmus){
      sumQseals.parsed[i,] <- init.seals.demand[i]*seals.foraging.matrix[[i]][init.season,]
      sumQpengs.parsed[i,] <- init.pengs.demand[i]*pengs.foraging.matrix[[i]][init.season,]
      sumQwhales.parsed[i,] <- init.whales.demand[i]*whales.foraging.matrix[[i]][init.season,]
      sumQfish.parsed[i,] <- init.fish.demand[i]*fish.foraging.matrix[[i]][init.season,]
    }
    sumQseals.parsed[is.na(sumQseals.parsed)]<-0
    init.seals.demand <- colSums(sumQseals.parsed)
    sumQpengs.parsed[is.na(sumQpengs.parsed)]<-0
    init.pengs.demand <- colSums(sumQpengs.parsed)
    sumQwhales.parsed[is.na(sumQwhales.parsed)]<-0
    init.whales.demand <- colSums(sumQwhales.parsed)
    sumQfish.parsed[is.na(sumQfish.parsed)]<-0
    init.fish.demand <- colSums(sumQfish.parsed)
    #print(init.seals.demand)
    #print(init.pengs.demand)
    #print(init.whales.demand)
    #print(init.fish.demand)
    init.tot.demand <- init.seals.demand + init.pengs.demand + init.whales.demand + init.fish.demand
    # we only need to allocate based on demand in the SSMUs so drop the bathtubs
    init.tot.demand <- init.tot.demand[1:nssmus]
    #
	  B0 <- sum(init.krill.biomass)
	  #
	  # since starting from a saved state, we can add some observation error to B0
    B0 <- B0 * exp(B0.errors[1])
  }
  #
	# user can input gamma (a precautionary harvest rate) or the precautionary limit itself
	# in the latter case, gamma will be calculated as precautionary.limit.mmt/B0
	if(!is.null(precautionary.limit.mmt)){
    actual.gamma <- precautionary.limit.mmt/(B0/1000/1000/1000000)
  }
  #
	# now fill out the catch matrix according to the option of choice
	# first make a matrix with 0 catches that can be overwritten depending
	# on when fishing starts and depending on the outcome of the simulation
	catch <- matrix(0, nrow = ntimes, ncol = nssmus)
	threshold.violations <- numeric(nssmus)
  if(single.sim && !suppress.messages){
		cat("",fill=TRUE)
		cat("Make sure the first row in catch.setup matches what you set for init.season",fill=TRUE)
		cat("",fill=TRUE)
	}
	# replicate the catch.setup matrix to cover the entire simulation period
	# catch.setup should be input as a matrix with rows = time periods (usually seasons) and columns = SSMUs
	catch.setup<-matrix(rep(t(catch.setup),length=(ntimes-start.fishing+1)*nssmus),ncol=nssmus,byrow=TRUE)
  # earmarks is the initial, allocated catch for each ssmu
	# allocation is the proportion of the total initial catch that is assigned to each ssmu
	earmarks <- catch
	allocation <- rep(0, nssmus)
	area.earmark <- rep(0, nssmus)
	option.string = "no fishing"
	if(!is.null(fishing.option)) {
	  # if NOT starting from a saved state then observation errors already built into initialization
	  # therefore, overwrite the first row of allocation.errors with zeros
	  # NOTE: at this point B0 already has observation errors and bias built in (either because initializing from data
	  # or because at the end of the saved.state initialization I already added error to B0)
	  if(is.null(saved.state)){
	    allocation.errors[1,] <- 0
	  }
	  # option 1 is historical catch
    if(fishing.option == 1){
      tt <- allocate.catch(fishing.option=1, historical.catch=historical.catch, init.tot.demand=NULL, init.krill.biomass=NULL, option5.list=NULL,
        tt.density=NULL, relQdev1=NULL, seals.params.list=NULL, pengs.params.list=NULL, earmarks=earmarks, B0=B0, actual.gamma=actual.gamma,
        wbar.vector=wbar.vector[1:nssmus], catch.setup=catch.setup, ntimes=ntimes, nssmus=nssmus,
        start.fishing=start.fishing, obs.errors=allocation.errors[1,], obs.bias = 1.)
    }
		# option 2 is predator demand
		if(fishing.option == 2){
		  tt <- allocate.catch(fishing.option=2, historical.catch=NULL, init.tot.demand=init.tot.demand, init.krill.biomass=NULL, option5.list=NULL,
        tt.density=NULL, relQdev1=NULL, seals.params.list=NULL, pengs.params.list=NULL, earmarks=earmarks, B0=B0, actual.gamma=actual.gamma,
        wbar.vector=wbar.vector[1:nssmus], catch.setup=catch.setup, ntimes=ntimes,  nssmus=nssmus,
        start.fishing=start.fishing, obs.errors=allocation.errors[1,], obs.bias = 1.)
    }
		# option 3 is krill standing stock
		if(fishing.option == 3){
		  tt <- allocate.catch(fishing.option=3, historical.catch=NULL, init.tot.demand=NULL, init.krill.biomass=init.krill.biomass, option5.list=NULL,
        tt.density=NULL, relQdev1=NULL, seals.params.list=NULL, pengs.params.list=NULL, earmarks=earmarks, B0=B0, actual.gamma=actual.gamma,
        wbar.vector=wbar.vector[1:nssmus], catch.setup=catch.setup, ntimes=ntimes,  nssmus=nssmus,
        start.fishing=start.fishing, obs.errors=allocation.errors[1,], obs.bias = 1.)
    }
		# option 4 is krill standing stock minus predator demand
		if(fishing.option == 4){
		  tt <- allocate.catch(fishing.option=4, historical.catch=NULL, init.tot.demand=init.tot.demand, init.krill.biomass=init.krill.biomass, option5.list=NULL,
        tt.density=NULL, relQdev1=NULL, seals.params.list=NULL, pengs.params.list=NULL, earmarks=earmarks, B0=B0, actual.gamma=actual.gamma,
        wbar.vector=wbar.vector[1:nssmus], catch.setup=catch.setup, ntimes=ntimes,  nssmus=nssmus,
        start.fishing=start.fishing, obs.errors=allocation.errors[1,], obs.bias = 1.)
    }
		# option 5 is adjustable
		if(fishing.option == 5){
		  tt <- allocate.catch(fishing.option=5, historical.catch=historical.catch, init.tot.demand=init.tot.demand, init.krill.biomass=init.krill.biomass, option5.list=option5.list,
        tt.density=tt.density, relQdev1=relQdev1, seals.params.list=seals.params.list, pengs.params.list=pengs.params.list, earmarks=earmarks, B0=B0, actual.gamma=actual.gamma,
        wbar.vector=wbar.vector[1:nssmus], catch.setup=catch.setup, ntimes=ntimes,  nssmus=nssmus,
        start.fishing=start.fishing, obs.errors=allocation.errors[1,], obs.bias = 1.)
      base.allocation <- tt$base.allocation
      coef.matrix <- tt$coef.matrix
		}
		# option 6 is pulse fishing
		if(fishing.option == 6){
		  tt <- allocate.catch(fishing.option=6, historical.catch=NULL, init.tot.demand=NULL, init.krill.biomass=NULL, option5.list=NULL,
        tt.density=NULL, relQdev1=NULL, seals.params.list=NULL, pengs.params.list=NULL, earmarks=earmarks, B0=B0, actual.gamma=actual.gamma,
        wbar.vector=wbar.vector[1:nssmus], catch.setup=catch.setup, ntimes=ntimes,  nssmus=nssmus,
        start.fishing=start.fishing, obs.errors=allocation.errors[1,], obs.bias = 1.)
      point.size = 0.75
		}
		#print(tt)
		earmarks <- tt$earmarks
    catch <- tt$catch
    allocation <- tt$allocation
    option.string <- tt$option.string
	}
	#print(catch[1:19,])
	#cat("",fill=TRUE)
	#
	# if elect to stop fishing at a certain time (to check for reversibility)
	# reset all catches starting at that time to zero
	if(stop.fishing <= start.fishing) {
		stop("stop.fishing.yr must be greater than start.fishing.yr")
	}
  if(stop.fishing < ntimes && stop.fishing > start.fishing) {
		catch[stop.fishing:ntimes,  ] <- 0
		earmarks[stop.fishing:ntimes,  ] <- 0
	}
	#
	# print some preliminary stuff to the screen
	if(single.sim && !suppress.messages) {
		cat(paste("You are using ", ntubs, " bathtubs and the movement matrix has ", nareas, " areas.", sep = ""), fill = TRUE)
		cat(paste("It is assumed that the last ", ntubs, " areas are the bathtubs and there are ", nssmus, " SSMUs.", sep = ""),
			fill = TRUE)
		cat("If this is not correct IGNORE THE FOLLOWING RESULTS.", fill = TRUE)
		cat("",fill=TRUE)
		print(list(data.frame(krill.biomass.mmt = round(init.krill.biomass/1000/1000/1000000, 2), krill.density = round(
			init.krill.density[1:nssmus], 2), k.seals = round(seals[1,  ]/1000, 2), k.pengs = round(pengs[1,  ]/1000, 2), k.whales =
			round(whales[1,  ]/1000, 2), k.fish = round(fish[1,  ]/1000, 2), tot.demand.mmt = round(init.tot.demand/1000/
			1000/1000000, 2), allocation = round(allocation, 2)), paste("fishing option = ", option.string, sep = ""),
			paste("gamma = ", actual.gamma, sep = ""), paste("B0 = ", B0/1000/1000/1000000, " mmt",sep="")))
	}
	#stop()
	#
	# rescale the competition matrix so max is 1
	# if there are NAs in the competition matrixt set these to zero
	competition.matrix[is.na(competition.matrix)]<-0
	# if there is no fishing determine max of competition matrix from predators alone
	if(!is.null(fishing.option)){
    tt.max <- apply(competition.matrix, 1, max)
  } else {
    tt.max <- apply(competition.matrix[,1:4], 1, max)
  }
	competition.matrix <- sweep(competition.matrix, 1, tt.max, "/")
	#
  # now do the work of projecting the populations forward
	catch.scalar <- rep(1.0, nssmus)
	season<-init.season
  # create a vector to house krill weights that vary with time and account for movement of krill of potentially different sizes from various model areas 
  wbar.vector.tprime<-wbar.vector
  # create a matrix to house krill weights each sim.t -- to check if the weights used in the analysis are reasonable. can be turned off along with line 1233
  wbar.matrix<-matrix(0, nrow=ntimes, ncol=18)
	#
  ### CHECK ACTUALLY GOOD TO HEAR B/C HAVE NOT UPDATED
	for(sim.t in 1:ntimes) {
    #wbar.matrix[i,]<-wbar.vector.tprime
	  #
		# watch out, the indices for the krill and predator matrices are probably different
		# because they recruit at different ages
		krill.t <- sim.t + krill.Rage
		seals.t <- sim.t + seals.maxRage
		pengs.t <- sim.t + pengs.maxRage
		whales.t <- sim.t + whales.maxRage
		fish.t <- sim.t + fish.maxRage
    #
    # update wbar.vector for each time step if a matrix for GGP is specified
    if(!is.null(GGP)){
      if(dim(GGP)[1]!=ntimes){
          stop("GGP must include nyears*nseasons rows")
      }
      # multiply the time-varying driver of krill weight by the current estimate of mean krill weight in each model area
      wbar.vector.t<-wbar.vector.tprime*GGP[sim.t,]
    } else {
      # to retain old vector, but with new name to accomodate a vector that changes according the initial parameters
      # if there is no GGP specfied, krill wieght remains the same as specified in input parameters throughout the simulation
      wbar.vector.t<-wbar.vector
    }
    #
	  # first thing to do is any reassessments (and reallocating catches) if necessary
	  if(!is.null(fishing.option) && !is.null(reassessment.times)){
	    if(is.element(sim.t,reassessment.times)){
        # start tt.index at 2 because the first column of allocation.errors
        # and first element of B0.errors were already used for initial conditions
	      if(match(sim.t,reassessment.times)==1){tt.index<-2}
	      if(fishing.option == 1){
	        # historical catches are means over previous time periods
	        use.me <- colMeans(catch[1:(sim.t-1),])
	        if(all(use.me==0)){use.me<-historical.catch}
	        tt <- allocate.catch(fishing.option=1, historical.catch=use.me, init.tot.demand=NULL, init.krill.biomass=NULL, option5.list=NULL,
           tt.density=NULL, relQdev1=NULL, seals.params.list=NULL, pengs.params.list=NULL, earmarks=earmarks, B0=B0.mult*sum(krill[krill.t-1, 1:nssmus] * wbar.vector.t[1:nssmus])*exp(B0.errors[tt.index]), actual.gamma=actual.gamma,
           wbar.vector=wbar.vector.t[1:nssmus], catch.setup=catch.setup, ntimes=ntimes, nssmus=nssmus,
           start.fishing=start.fishing, obs.errors=allocation.errors[tt.index,], obs.bias=allocation.mult)
        }
	      if(fishing.option == 2){
	        # predator demands are measured in the corresponding season of the previous year
	        use.me <- (seals[seals.t-nseasons,1:nssmus]*Qseals[seals.t-nseasons,1:nssmus])+(pengs[pengs.t-nseasons,1:nssmus]*Qpengs[pengs.t-nseasons,1:nssmus])+(whales[whales.t-nseasons,1:nssmus]*Qwhales[whales.t-nseasons,1:nssmus])+(fish[fish.t-nseasons,1:nssmus]*Qfish[fish.t-nseasons,1:nssmus])
	        # convert demand in numbers to demand in biomass
          use.me <- use.me * wbar.vector.t[1:nssmus]
          tt <- allocate.catch(fishing.option=2, historical.catch=NULL, init.tot.demand=use.me, init.krill.biomass=NULL, option5.list=NULL,
           tt.density=NULL, relQdev1=NULL, seals.params.list=NULL, pengs.params.list=NULL, earmarks=earmarks, B0=B0.mult*sum(krill[krill.t-1, 1:nssmus] * wbar.vector.t[1:nssmus])*exp(B0.errors[tt.index]), actual.gamma=actual.gamma,
           wbar.vector=wbar.vector.t[1:nssmus], catch.setup=catch.setup, ntimes=ntimes,  nssmus=nssmus,
           start.fishing=start.fishing, obs.errors=allocation.errors[tt.index,], obs.bias=allocation.mult)
	      }
	      if(fishing.option == 3){
	        # krill standing stock is measured in the corresponding season of the previous year
	        use.me <- krill[krill.t-nseasons, 1:nssmus] * wbar.vector.t[1:nssmus]
	        tt <- allocate.catch(fishing.option=3, historical.catch=NULL, init.tot.demand=NULL, init.krill.biomass=use.me, option5.list=NULL,
           tt.density=NULL, relQdev1=NULL, seals.params.list=NULL, pengs.params.list=NULL, earmarks=earmarks, B0=B0.mult*sum(krill[krill.t-1, 1:nssmus] * wbar.vector.t[1:nssmus])*exp(B0.errors[tt.index]), actual.gamma=actual.gamma,
           wbar.vector=wbar.vector.t[1:nssmus], catch.setup=catch.setup, ntimes=ntimes,  nssmus=nssmus,
           start.fishing=start.fishing, obs.errors=allocation.errors[tt.index,], obs.bias=allocation.mult)
        }
	      if(fishing.option == 4){
	        # predator demands are measured in the corresponding season of the previous year
	        use.me.demand <- (seals[seals.t-nseasons,1:nssmus]*Qseals[seals.t-nseasons,1:nssmus])+(pengs[pengs.t-nseasons,1:nssmus]*Qpengs[pengs.t-nseasons,1:nssmus])+(whales[whales.t-nseasons,1:nssmus]*Qwhales[whales.t-nseasons,1:nssmus])+(fish[fish.t-nseasons,1:nssmus]*Qfish[fish.t-nseasons,1:nssmus])
          use.me.demand <- use.me.demand * wbar.vector.t[1:nssmus]
          # krill standing stock is measured in the corresponding season of the previous year
	        use.me.biomass <- krill[krill.t-nseasons, 1:nssmus] * wbar.vector.t[1:nssmus]
          tt <- allocate.catch(fishing.option=4, historical.catch=NULL, init.tot.demand=use.me.demand, init.krill.biomass=use.me.biomass, option5.list=NULL,
           tt.density=NULL, relQdev1=NULL, seals.params.list=NULL, pengs.params.list=NULL, earmarks=earmarks, B0=B0.mult*sum(krill[krill.t-1, 1:nssmus] * wbar.vector.t[1:nssmus])*exp(B0.errors[tt.index]), actual.gamma=actual.gamma,
           wbar.vector=wbar.vector.t[1:nssmus], catch.setup=catch.setup, ntimes=ntimes,  nssmus=nssmus,
           start.fishing=start.fishing, obs.errors=allocation.errors[tt.index,], obs.bias=allocation.mult)
	      }
	      if(fishing.option == 5){
	        # historical catches are means over previous time periods
	        use.me.catch <- colMeans(catch[1:(sim.t-1),])
	        #print(catch[1:(sim.t-1),])
	        #stop()
          if(all(use.me.catch==0)){use.me.catch<-historical.catch}
	        #print(historical.catch)
          # predator demands are measured in the corresponding season of the previous year
	        use.me.demand <- (seals[seals.t-nseasons,1:nssmus]*Qseals[seals.t-nseasons,1:nssmus])+(pengs[pengs.t-nseasons,1:nssmus]*Qpengs[pengs.t-nseasons,1:nssmus])+(whales[whales.t-nseasons,1:nssmus]*Qwhales[whales.t-nseasons,1:nssmus])+(fish[fish.t-nseasons,1:nssmus]*Qfish[fish.t-nseasons,1:nssmus])
          use.me.demand <- use.me.demand * wbar.vector.t[1:nssmus]
          # krill standing stock is measured in the corresponding season of the previous year
	        use.me.biomass <- krill[krill.t-nseasons, 1:nssmus] * wbar.vector.t[1:nssmus]
	        # density is measured at all previous times
          use.me.density <- sweep(krill[1:(krill.t-1), ],2,wbar.vector.t/areas,"*")
	        # here Qdevs are relQdev2 that are from time=1 to time=sim.t-1 (to match the densities)
	        use.me.Qdev <- relQdev2[1:(sim.t-1),]
          tt <- allocate.catch(fishing.option=5, historical.catch=use.me.catch, init.tot.demand=use.me.demand, init.krill.biomass=use.me.biomass, option5.list=option5.list,
           tt.density=use.me.density, relQdev1=use.me.Qdev, seals.params.list=seals.params.list, pengs.params.list=pengs.params.list, earmarks=earmarks, B0=B0.mult*sum(krill[krill.t-1, 1:nssmus] * wbar.vector.t[1:nssmus])*exp(B0.errors[tt.index]), actual.gamma=actual.gamma,
           wbar.vector=wbar.vector.t[1:nssmus], catch.setup=catch.setup, ntimes=ntimes,  nssmus=nssmus,
           start.fishing=start.fishing, obs.errors=allocation.errors[tt.index,], obs.bias=allocation.mult)
          base.allocation <- tt$base.allocation
          coef.matrix <- tt$coef.matrix
	      }
	      earmarks[sim.t:ntimes,]<-tt$earmarks[sim.t:ntimes,]
	      catch[sim.t:ntimes,]<-tt$catch[sim.t:ntimes,]
	      if(stop.fishing < ntimes && stop.fishing > start.fishing) {
	      	catch[stop.fishing:ntimes,  ] <- 0
		      earmarks[stop.fishing:ntimes,  ] <- 0
	      }
        tt.index <- tt.index+1
      }
	  }
		#
		# note the following are things I want to be area-specific
		# but write over every time step to save memory
		M2F <- numeric(nssmus)
		krill.resident.prime <- numeric(nareas)
		krill.resident <- numeric(nareas)
		Z <- numeric(nareas)
		fraction<-rep(1,nareas)
		sumQseals <- numeric(nssmus)
		sumQpengs <- numeric(nssmus)
		sumQwhales <- numeric(nssmus)
		sumQfish <- numeric(nssmus)
		sumQseals.parsed<-matrix(0,nrow=nssmus,ncol=nareas)
		sumQpengs.parsed<-matrix(0,nrow=nssmus,ncol=nareas)
		sumQwhales.parsed<-matrix(0,nrow=nssmus,ncol=nareas)
		sumQfish.parsed<-matrix(0,nrow=nssmus,ncol=nareas)
		#
		# assume there is no loss of animals in the bathtub(s) other than movement into SSMUs
		krill.resident.prime[(nssmus + 1):nareas] <- 1
		krill.resident[(nssmus + 1):nareas] <- exp( - V[(nssmus + 1):nareas, season])
		Z[(nssmus + 1):nareas] <-  - log(krill.resident[(nssmus + 1):nareas])
		immigrants <- matrix(0, nrow = nareas, ncol = nareas)
		#
		# loop over ssmus to get demands for predators that eventually breed in the ssmu
		# these demands are eventually parsed among all areas, enabling predators to forage
		# where ever the user chooses without modeling movement
		krill.density <- (krill[krill.t-1,]*wbar.vector.t)/areas
    for(i in 1:nssmus) {
			seals.params <- seals.params.list[[i]]
			pengs.params <- pengs.params.list[[i]]
			whales.params <- whales.params.list[[i]]
			fish.params <- fish.params.list[[i]]
			krill.params <- krill.params.list[[i]]
			#
			# first compute area-specific total demand for krill
			# this demand is calculated by BREEDING area
			# note that I am using krill density on the x-axis
			# and per-capita consumption (numbers of krill) on the y-axis
			# of the functional response
   		if(seals.params$init.value > 0){
        tt.density <- sum(krill.density*seals.foraging.matrix[[i]][season,])
        Qseals[seals.t, i] <- (seals.params$QQmax[season] * (tt.density^(seals.params$Qq[season] + 1)))/((seals.params$Qk5[season]^(
  				seals.params$Qq[season] + 1)) + (tt.density^(seals.params$Qq[season] + 1)))
 				sumQseals[i] <- Qseals[seals.t, i] * seals[seals.t - 1, i]
 				sumQseals.parsed[i,]<-sumQseals[i]*seals.foraging.matrix[[i]][season,]
			}
			if(pengs.params$init.value > 0){
			  tt.density <- sum(krill.density*pengs.foraging.matrix[[i]][season,])
        Qpengs[pengs.t, i] <- (pengs.params$QQmax[season] * (tt.density^(pengs.params$Qq[season] + 1)))/((pengs.params$Qk5[season]^(
  				pengs.params$Qq[season] + 1)) + (tt.density^(pengs.params$Qq[season] + 1)))
			  sumQpengs[i] <- Qpengs[pengs.t, i] * pengs[pengs.t - 1, i]
			  sumQpengs.parsed[i,]<-sumQpengs[i]*pengs.foraging.matrix[[i]][season,]
			}
			if(whales.params$init.value > 0){
			  tt.density <- sum(krill.density*whales.foraging.matrix[[i]][season,])
        Qwhales[whales.t, i] <- (whales.params$QQmax[season] * (tt.density^(whales.params$Qq[season] + 1)))/((whales.params$Qk5[season]^
  				(whales.params$Qq[season] + 1)) + (tt.density^(whales.params$Qq[season] + 1)))
			  sumQwhales[i] <- Qwhales[whales.t, i] * whales[whales.t - 1, i]
  			sumQwhales.parsed[i,]<-sumQwhales[i]*whales.foraging.matrix[[i]][season,]
      }
			if(fish.params$init.value > 0){
			  tt.density <- sum(krill.density*fish.foraging.matrix[[i]][season,])
  			Qfish[fish.t, i] <- (fish.params$QQmax[season] * (tt.density^(fish.params$Qq[season] + 1)))/((fish.params$Qk5[season]^(fish.params$
  				Qq[season] + 1)) + (tt.density^(fish.params$Qq[season] + 1)))
 				sumQfish[i] <- Qfish[fish.t, i] * fish[fish.t - 1, i]
 				sumQfish.parsed[i,]<-sumQfish[i]*fish.foraging.matrix[[i]][season,]
			}
		}
		#print(round(Qseals[seals.t,]/10000,2))
    #
		# now sum the demands by FEEDING area
		# colSums of these matrices are demands needed to compute M2F in each area
		sumQseals.feeding<-colSums(sumQseals.parsed)
		sumQseals.feeding <- ifelse(is.na(sumQseals.feeding),0,sumQseals.feeding)
		sumQpengs.feeding<-colSums(sumQpengs.parsed)
		sumQpengs.feeding <- ifelse(is.na(sumQpengs.feeding),0,sumQpengs.feeding)
		sumQwhales.feeding<-colSums(sumQwhales.parsed)
		sumQwhales.feeding <- ifelse(is.na(sumQwhales.feeding),0,sumQwhales.feeding)
		sumQfish.feeding<-colSums(sumQfish.parsed)
		sumQfish.feeding <- ifelse(is.na(sumQfish.feeding),0,sumQfish.feeding)
		#
    # loop over all areas to get mortalities and "competition reduction fractions"
		for(i in 1:nareas){
      # use fishing thresholds to set catch = 0 if krill density is not "sufficient"
			if(i <= nssmus){
        #krill.density <- (krill[krill.t - 1, i] * krill.params$wbar)/areas[i]
        if(krill.density < threshold.density[i] && earmarks[sim.t,i] > 0){
          catch[sim.t, i] <- 0.
			    threshold.violations[i] <- threshold.violations[i]+1
        }
      # now compute the catch if fishing.option=5
        if(!is.null(fishing.option) && fishing.option == 5){
          if(!is.na(option5.list$monitoring.seasons[i])){
            if(season==option5.list$monitoring.seasons[i]){
     				  #if((sim.t < stop.fishing) && i <= nssmus) {
     				  if((sim.t >= start.fishing) && (sim.t < stop.fishing) && i <= nssmus) {
     					  tt.coef <- coef.matrix[i,  ]
                if(option5.list$monitored.spp[i] == "seals") {
     						  obs.relQ <- Qseals.performance[1:(seals.t-nseasons), i]/seals.params$QQmax[season]
     						  tt.foraging.vector <- seals.params$foraging.matrix[season,]
     					  }
     					  else {
     						  obs.relQ <- Qpengs.performance[1:(pengs.t-nseasons), i]/pengs.params$QQmax[season]
     						  tt.foraging.vector <- pengs.params$foraging.matrix[season,]
     					  }
     					  # old code in next line -- relQ was NOT standardized to it's mean (Hewitt et al. had such standardization but KPFM1 did not)
                # obs.relQ <- (obs.relQ^option5.list$obs.mult[area.index]) + relQdev2[sim.t, i]
                # now new code
                # get deviations from mean (this is the y in Hewitt et al's y = alpha + beta*ln(x) + epsilon)
                obs.relQ <- obs.relQ-(sum(obs.relQ)/length(obs.relQ))
                # now take the last deviation in the relevant monitoring season to use for making the current adjustment
                # note the "seals.t-nseasons" indexing for obs.relQ
                obs.relQ <- obs.relQ[length(obs.relQ)]
                # add in the bias and observation error
                obs.relQ <- (obs.relQ*option5.list$obs.mult[i]) + relQdev2[sim.t, i]
                #
                inferred.krill <- exp((obs.relQ - tt.coef[1])/tt.coef[2])
     					  adjustment <- inferred.krill/sum(init.krill.density*tt.foraging.vector)
     					  #cat(sim.t, i, relQdev2[sim.t, i], obs.relQ, inferred.krill, init.krill.density[i], adjustment, sep=", ", fill=TRUE)
                #
                # the following line of code only works when wbar is constant and teh same in all model areas--JTH bug find 7/26/2013
     					  #catch[sim.t, i] <- (B0 * actual.gamma * base.allocation[i] * adjustment)/krill.params$wbar
                # the following line of code should be generic for any wbar, whether changing with time or by ssmu
     					  catch[sim.t, i] <- (B0 * actual.gamma * base.allocation[i] * adjustment)/wbar.vector.t[i]
     					}
     		    }
   		    }
        }
 		  }
   		# holy crap those curly brackets are brutal
			#
		  # now compute "planned removals" that are the sum of catch and predation
			# if doing some spin up from initial conditions, we can
			# elect to rescale the catches based on the ratio of biomasses
			if(i <= nssmus){
	     			if((sim.t == start.fishing) && rescale.catch) {
	     				catch.scalar[i] <- (krill[krill.t - 1, i] * wbar.vector.t[i])/init.krill.biomass[i]
	     			}
	     			CsumQ <- (catch[sim.t, i] * catch.scalar[i]) + sumQseals.feeding[i] + sumQpengs.feeding[i] + sumQwhales.feeding[i] + sumQfish.feeding[i]
			}
		  else {
				CsumQ <- sumQseals.feeding[i] + sumQpengs.feeding[i] + sumQwhales.feeding[i] + sumQfish.feeding[i]
			}
			#
			# now get the mortality from predation and fishing
			if((available.fraction[i] * krill[krill.t - 1, i]) >= CsumQ) {
				# now we solve the catch equation to get the combined effect
				# of fishing and predation mortalities
				# NOTE: I divide CsumQ and N by 1e10 to prevent numerical errors, but the
				# resulting mortality estimate is the same
				# could change the 100s and 1e-10s to 250s and 1e-100s in calculating tt.removals
				# and in uniroot(...,interval = c(),...) if want to search over greater range of M2Fs
				tt.removals <- (100/(100 + krill.params$M0[season] + V[i,season])) * (1 - exp( - (100 + krill.params$M0[season] + V[i,season]))) *
					krill[krill.t - 1, i]
				if(tt.removals <= CsumQ) {
					M2F[i] <- 100.
				}
				else {
					tt.removals <- (1e-10/(1e-10 + krill.params$M0[season] + V[i,season])) * (1 - exp( - (1e-10 + krill.params$
						M0[season] + V[i,season]))) * krill[krill.t - 1, i]
					if(tt.removals >= CsumQ) {
						M2F[i] <- 1e-10
					}
					else {
						tt.root <- uniroot(m2f.root, interval = c(1e-10, 100.), M0 = krill.params$M0[season], V = V[
							i,season], CsumQ = CsumQ/10000000000., N = krill[krill.t - 1, i]/10000000000.)
						if(abs(tt.root$f.root) > 1) {
							print(paste("root=", tt.root$root, " f=", tt.root$f.root, " CsumQ=", CsumQ,
								" N=", krill[krill.t - 1, i], sep = ""))
						}
						M2F[i] <- tt.root$root
					}
				}
			}
			else {
				# if estimated planned removals are greater than available krill
				# make the consumption+catch almost
				# equal to available krill assuming that both consumption
				# and catch are reduced -- note I can't easily change M0[season] and V[i,season]
				# so these latter removals are assumed to remain at "full strength"
				# I have a small number of krill "left over" to prevent the root finder
				# from searching over huge M2Fs and because I don't think it's possible
				# to catch or eat every last krill (they're wiley buggers after all)
				if(single.sim && !suppress.messages) {
					print(paste("adjusting -- allocated catch + demand > available krill -- time = ", sim.t,
						"; area = ", i, sep = ""))
				}
				# leave a few individuals behind
				# BE CAREFUL -- YOU NEED TO THINK HOW DOING THIS
				# WILL INTERACT WITH THE KRILL S-R RELATIONSHIP
				newCsumQ <- available.fraction[i] * krill[krill.t - 1, i]
				fraction[i] <- newCsumQ/CsumQ
				if(fraction[i]>1){fraction[i]<-1.}
				tt.removals <- (100/(100 + krill.params$M0[season] + V[i,season])) * (1 - exp( - (100 + krill.params$M0[season] + V[i,season]))) *
					krill[krill.t - 1, i]
				if(tt.removals <= newCsumQ) {
					M2F[i] <- 100.
				}
				else {
					tt.removals <- (1e-10/(1e-10 + krill.params$M0[season] + V[i,season])) * (1 - exp( - (1e-10 + krill.params$
						M0[season] + V[i,season]))) * krill[krill.t - 1, i]
					if(tt.removals >= newCsumQ) {
						M2F[i] <- 1e-10
					}
					else {
						tt.root <- uniroot(m2f.root, interval = c(1e-10, 100.), M0 = krill.params$M0[season], V = V[
							i,season], CsumQ = newCsumQ/10000000000., N = krill[krill.t - 1, i]/10000000000.)
						if(abs(tt.root$f.root) > 1) {
							print(paste("root=", tt.root$root, " f=", tt.root$f.root, " CsumQ=", CsumQ,
								" N=", krill[krill.t - 1, i], sep = ""))
						}
						M2F[i] <- tt.root$root
					}
				}
			}
			#
			if(i <= nssmus){
			    # adjust catch if there was competition
			    catch[sim.t,i]<-catch[sim.t,i]*catch.scalar[i]
          if(fraction[i]<1.){
			      catch[sim.t, i] <- catch[sim.t, i] * fraction[i] * competition.matrix[i, 5]
			    }
     			#
     			# now compute krill resident and resident.prime and total krill "mortality"
     			# "resident.prime" represents survival from natural mortality and fishing
     			# "resident" represents survival from natural mortality, fishing, and movement
     			krill.resident.prime[i] <- exp( - (krill.params$M0[season] + M2F[i]))
     			krill.resident[i] <- krill.resident.prime[i] * exp( - V[i,season])
     			Z[i] <-  - log(krill.resident[i])
     			#print(Z[i])
     			#print(krill.resident[i])
			}
		}
		#
		# now loop over ssmus to get recruitment, move krill, and project abundance
		for(i in 1:nssmus){
			# get parameters
			krill.params <- krill.params.list[[i]]
			seals.params <- seals.params.list[[i]]
			pengs.params <- pengs.params.list[[i]]
			whales.params <- whales.params.list[[i]]
			fish.params <- fish.params.list[[i]]
			#
			# krill recruitment
			if(krill.params$Ralpha[season]>0){
			  Rkrill[sim.t, i] <- (krill.params$Ralpha[season] * krill[krill.t - krill.Rage, i])/(krill.params$Rbeta[season] + krill[krill.t -
				  krill.Rage, i]) * exp(krill.Rdev[sim.t] + (krill.params$Rphi[season] * krill.Renv[sim.t]) + bias.correction[season,i])
      }
			#
			if(seals.params$init.value > 0){
        # compute weights that depend on spatial distribution of foraging and degree of competition
        seals.x <- fraction * competition.matrix[,1]
        seals.x <- ifelse(fraction < 1., seals.x, 1.)
        # compute Qperformance for use in predator recruitment functions
        Qseals.performance[seals.t, i] <- Qseals[seals.t, i]*sum(seals.x*seals.foraging.matrix[[i]][season, ])
        # recruitment
        if(seals.params$Ralpha[season]>0){
  			  Ssquiggle <- seals[seals.t - (seals.params$Rage*nseasons), i] * ((Qseals.performance[seals.t - (seals.params$Rage*nseasons), i]/seals.params$
  				  QQmax[season])^seals.params$Rphi[season])
  			  if(seals[1,i]==0){
  			    Rseals[sim.t,i]<-0
          }
          else {
  			    Rgamma <- log(seals.params$RRpeak[season]/seals.params$Ralpha[season])/(log(seals.params$RSpeak[season])-1)
            Rbeta <- Rgamma/seals.params$RSpeak[season]
            Rseals[sim.t, i] <- seals.params$Ralpha[season] * (Ssquiggle^Rgamma) * exp( - Rbeta * seals[seals.t - (seals.params$Rage*nseasons), i])
            # compute Qperformance for use in juvenile survival functions - depends on Q from first winter season of life, assumed to be the first season after breeding
            if(!is.na(seals.params$Jphi)){
              juv.performance <- (Qseals.performance[seals.t-seals.params$Rage*nseasons+1, i]/seals.params$QQmax[season+1])^seals.params$Jphi
              Rseals[sim.t,i]<-Rseals[sim.t,i]*juv.performance
            }
          }
  			}
      }
      #
      if(pengs.params$init.value > 0){
			  # weights
			  pengs.x <- fraction * competition.matrix[,2]
        pengs.x <- ifelse(fraction < 1., pengs.x, 1.)
        # performance
        Qpengs.performance[pengs.t, i] <- Qpengs[pengs.t, i]*sum(pengs.x*pengs.foraging.matrix[[i]][season, ])
        # recruitment
        if(pengs.params$Ralpha[season]>0){
  			  Ssquiggle <- pengs[pengs.t - (pengs.params$Rage*nseasons), i] * ((Qpengs.performance[pengs.t - (pengs.params$Rage*nseasons), i]/pengs.params$
  			 	  QQmax[season])^pengs.params$Rphi[season])
          if(pengs[1,i]==0){
            Rpengs[sim.t,i]<-0
          }
          else {
  			    Rgamma <- log(pengs.params$RRpeak[season]/pengs.params$Ralpha[season])/(log(pengs.params$RSpeak[season])-1)
  			    Rbeta <- Rgamma/pengs.params$RSpeak[season]
            Rpengs[sim.t, i] <- pengs.params$Ralpha[season] * (Ssquiggle^Rgamma) * exp( - Rbeta * pengs[pengs.t - (pengs.params$Rage*nseasons), i])
            # compute Qperformance for use in juvenile survival functions - depends on Q from first winter season of life, assumed to be the first season after breeding
            if(!is.na(pengs.params$Jphi)){
              juv.performance <- (Qpengs.performance[pengs.t-(pengs.params$Rage*nseasons)+1, i]/pengs.params$QQmax[season+1])^pengs.params$Jphi
              Rpengs[sim.t,i]<-Rpengs[sim.t,i]*juv.performance
            }
          }
  			}
			}
			#
			if(whales.params$init.value > 0){
        # weights
        whales.x <- fraction * competition.matrix[,3]
        whales.x <- ifelse(fraction < 1., whales.x, 1.)
        # performance
        Qwhales.performance[whales.t, i] <- Qwhales[whales.t, i]*sum(whales.x*whales.foraging.matrix[[i]][season, ])
        # recruitment
        if(whales.params$Ralpha[season]>0){
  			  Ssquiggle <- whales[whales.t - (whales.params$Rage*nseasons), i] * ((Qwhales.performance[whales.t - (whales.params$Rage*nseasons), i]/
  			 	  whales.params$QQmax[season])^whales.params$Rphi[season])
  			  if(whales[1,i]==0){
            Rwhales[sim.t,i]<-0
          }
          else {
            Rgamma <- log(whales.params$RRpeak[season]/whales.params$Ralpha[season])/(log(whales.params$RSpeak[season])-1)
  			    Rbeta <- Rgamma/whales.params$RSpeak[season]
  			    Rwhales[sim.t, i] <- whales.params$Ralpha[season] * (Ssquiggle^Rgamma) * exp( - Rbeta * whales[whales.t - (whales.params$Rage*nseasons), i])
            # compute Qperformance for use in juvenile survival functions - depends on Q from first winter season of life, assumed to be the first season after breeding
            if(!is.na(whales.params$Jphi)){
              juv.performance <- (Qwhales.performance[whales.t-whales.params$Rage*nseasons+1, i]/whales.params$QQmax[season+1])^whales.params$Jphi
              Rwhales[sim.t,i]<-Rwhales[sim.t,i]*juv.performance
            }
          }
  			}
      }
      #
      if(fish.params$init.value > 0){
			  # weights
			  fish.x <- fraction * competition.matrix[,4]
        fish.x <- ifelse(fraction < 1., fish.x, 1.)
			  # performance
        Qfish.performance[fish.t, i] <- Qfish[fish.t, i]*sum(fish.x*fish.foraging.matrix[[i]][season, ])
			  # recruitment
        if(fish.params$Ralpha[season]>0){
  			  Ssquiggle <- fish[fish.t - (fish.params$Rage*nseasons), i] * ((Qfish.performance[fish.t - (fish.params$Rage*nseasons), i]/fish.params$QQmax[season])^
  				  fish.params$Rphi[season])
  			  if(fish[1,i]==0){
  			    Rfish[sim.t,i]<-0
  			  }
          else {
            Rgamma <- log(fish.params$RRpeak[season]/fish.params$Ralpha[season])/(log(fish.params$RSpeak[season])-1)
  			    Rbeta <- Rgamma/fish.params$RSpeak[season]
  			    Rfish[sim.t, i] <- fish.params$Ralpha[season] * (Ssquiggle^Rgamma) * exp( - Rbeta * fish[fish.t - (fish.params$Rage*nseasons), i])
            # compute Qperformance for use in juvenile survival functions - depends on Q from first winter season of life, assumed to be the first season after breeding
            if(!is.na(fish.params$Jphi)){
              juv.performance <- (Qfish.performance[fish.t-fish.params$Rage*nseasons+1, i]/fish.params$QQmax[season+1])^fish.params$Jphi
              Rfish[sim.t,i]<-Rfish[sim.t,i]*juv.performance
            }
          }
  			}
			}
			#
			# now compute krill immigrants and project abundance
     	# wish I could figure out a better way to do this but I'm dumb now and
     	# just need the code to work -- can go back and make everything more efficient later
			for(j in 1:nareas) {
				# compute immigrants from area j into area i
				if(V[j,season] > 0) {
					immigrants[j, i] <- krill[krill.t - 1, j] * (v.array[j, i, season]/Z[j]) * (1 - exp( - Z[j]))
        }
			}
			#if(i==1){
      #      cat("sim.t = ",sim.t," : krill.t-1 = ",krill.t-1," : season = ",season,fill=TRUE,sep="")
      #      for(j in 1:18){
      #        cat(j,v.array[j,1,season],Z[j],v.array[j,1,season]/Z[j],fill=TRUE,sep=", ")
      #      }
      #}
			#if(i==1){cat("sim.t = ",sim.t," : krill.t-1 = ",krill.t-1,fill=TRUE,sep="");cat("Z:",Z,sep=" ",fill=TRUE); print(v.array[,1,season]);print(Z[1]);print(v.array[,1,season]/Z[1])}
			krill[krill.t, i] <- (krill[krill.t - 1, i] * krill.resident[i]) + Rkrill[sim.t, i] + sum(immigrants[, i])
			#seals[seals.t, i] <- (seals[seals.t - 1, i] * exp( - seals.params$M[season])) + Rseals[sim.t, i]
			#pengs[pengs.t, i] <- (pengs[pengs.t - 1, i] * exp( - pengs.params$M[season])) + Rpengs[sim.t, i]
			#whales[whales.t, i] <- (whales[whales.t - 1, i] * exp( - whales.params$M[season])) + Rwhales[sim.t, i]
			#fish[fish.t, i] <- (fish[fish.t - 1, i] * exp( - fish.params$M[season])) + Rfish[sim.t, i]
			#
      # new code to include "survival anomalies" that are functions of consumption/demand
			# anomaly = m*(x^g) + b where m = 2*Mprop, b = -Mprop, g = log(-b/m)/log(Mswitch), and
			# x is the avg. consumption/demand over feeding areas
      seals.anomaly <- 0.
      pengs.anomaly <- 0.
      whales.anomaly <- 0.
      fish.anomaly <- 0.
      #
      if(seals.params$init.value > 0){
        if(seals.params$Mprop[season]>0 && seals.params$Mswitch[season]>0){
          seals.anomaly <- (seals.params$Mprop[season]*(Qseals.performance[seals.t,i]/seals.params$QQmax[season])^((-1*seals.params$M[season])/log(seals.params$Mswitch[season])))+(-1*seals.params$Mprop[season]*exp(-1*seals.params$M[season]))
          }
  			# project abundance
  			seals[seals.t, i] <- (seals[seals.t - 1, i] * (exp( - seals.params$M[season])+seals.anomaly)) + Rseals[sim.t, i]
		    }
			#
			if(pengs.params$init.value > 0){
  			if(pengs.params$Mprop[season]>0 && pengs.params$Mswitch[season]>0){
          pengs.anomaly <- (pengs.params$Mprop[season]*(Qpengs.performance[pengs.t,i]/pengs.params$QQmax[season])^((-1*pengs.params$M[season])/log(pengs.params$Mswitch[season])))+(-1*pengs.params$Mprop[season]*exp(-1*pengs.params$M[season]))
  			}
  			# project abundance
  			pengs[pengs.t, i] <- (pengs[pengs.t - 1, i] * (exp( - pengs.params$M[season])+pengs.anomaly)) + Rpengs[sim.t, i]
			}
			#
			if(whales.params$init.value > 0){
  			if(whales.params$Mprop[season]>0 && whales.params$Mswitch[season]>0){
          whales.anomaly <- (whales.params$Mprop[season]*(Qwhales.performance[whales.t,i]/whales.params$QQmax[season])^((-1*whales.params$M[season])/log(whales.params$Mswitch[season])))+(-1*whales.params$Mprop[season]*exp(-1*whales.params$M[season]))
  			}
  			# project abundance
  			whales[whales.t, i] <- (whales[whales.t - 1, i] * (exp( - whales.params$M[season])+whales.anomaly)) + Rwhales[sim.t, i]
			}
			#
			if(fish.params$init.value > 0){
  			if(fish.params$Mprop[season]>0 && fish.params$Mswitch[season]>0){
          fish.anomaly <- (fish.params$Mprop[season]*(Qfish.performance[fish.t,i]/fish.params$QQmax[season])^((-1*fish.params$M[season])/log(fish.params$Mswitch[season])))+(-1*fish.params$Mprop[season]*exp(-1*fish.params$M[season]))
  			}
  			# project abundance
  			fish[fish.t, i] <- (fish[fish.t - 1, i] * (exp( - fish.params$M[season])+fish.anomaly)) + Rfish[sim.t, i]
			}
      #
      #cat("perf = ", Qseals.performance[seals.t,i]/seals.params$QQmax[season], ", anom = ",seals.anomaly,", surv = ",exp( - seals.params$M[season])+seals.anomaly,sep="",fill=TRUE)
      #
    }
    # The line of code below updates the average weight of krill in each model area by accounting for the origin of the krill due to movement into each new area
    # The point here is to preserve the mixture of weights that are produced by movement and the time-varying estimate of GGP
    for(i in 1:nareas){
      wbar.vector.tprime[i]<-(sum(wbar.vector.t*immigrants[,i])+(wbar.vector.t[i]*(krill[krill.t,i]-sum(immigrants[,i]))))/krill[krill.t,i] 
    }
    # pass back the estimated average wt vector for later use: this can be safely turned off because it's only used to visualize krill weights over time
    wbar.matrix[sim.t,]<-wbar.vector.tprime
		#
		if(season==nseasons){season<-1}else{season<-season+1}
	}
	#
	# if there was no allocated catch make threshold.violations NA -- useful for PM stuff
	for(i in 1:nssmus){
	  if(all(earmarks[,i]==0)){threshold.violations[i]<-NA}
  }
  #
  # now build the output up as a list named ss.out (single sim output)
  ss.out <- list(call = cl, setup=list(ntimes=ntimes, nseasons=nseasons, nareas=nareas, nssmus=nssmus, ntubs=ntubs, init.season=init.season, start.fishing=start.fishing, stop.fishing=stop.fishing, reassessment.times=reassessment.times), N=list(krill=krill, seals=seals, pengs=pengs, whales=whales, fish=fish), R=list(R=list(krill=Rkrill, seals=Rseals, pengs=Rpengs, whales=Rwhales, fish=Rfish),
    maxRage=list(krill=krill.Rage, seals=seals.maxRage, pengs=pengs.maxRage, whales=whales.maxRage, fish=fish.maxRage)),
    Q=list(demand=list(seals=Qseals, pengs=Qpengs, whales=Qwhales, fish=Qfish), consumption=list(seals=Qseals.performance, pengs=Qpengs.performance, whales=Qwhales.performance, fish=Qfish.performance)), fishery=list(allocation=earmarks, catch=catch, threshold.violations=threshold.violations), wbar=wbar.matrix)
  #
  # now do some plotting
	if(single.sim) {
    plot.ss.N(ss.out, annual.plots = annual.plots, connector = annual.connector, page.layout = page.layout, Nylimits = Nylimits)
    plot.ss.R(ss.out, annual.plots = annual.plots, connector = annual.connector, page.layout = page.layout, Rylimits = Rylimits)
		plot.ss.C(ss.out, annual.plots = annual.plots, connector = annual.connector, page.layout = page.layout, Cylimits = Cylimits, point.size = point.size)
	}
	#
	# return the results
  #write.csv(round(wbar.matrix,4), "c:/users/jth/desktop/foosa_ggp/r/wbar_matrix.csv")
  ss.out
}