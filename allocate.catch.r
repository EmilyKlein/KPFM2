allocate.catch <- function(fishing.option, historical.catch, init.tot.demand, init.krill.biomass, option5.list, tt.density, relQdev1, seals.params.list, pengs.params.list, earmarks, B0, actual.gamma, wbar.vector, catch.setup, ntimes, nssmus, start.fishing, obs.errors, obs.bias){
  #
  # George Watters
  # code last edited 23 June 2006
  #
  # generalized function to allocate catches among SSMUs based on one of six policy options
  # can include observation error to simulate "future assessments and reallocations" for a mini-MSE approach
  #
  # option 1 is historical catch
  if(fishing.option == 1) {
    option.string <- "historical catch"
    tt <- obs.bias * historical.catch * exp(obs.errors)
    allocation <- tt/sum(tt)
    area.earmark <- (B0 * actual.gamma * allocation)/wbar.vector
    earmarks[start.fishing:ntimes,  ] <- matrix(rep(area.earmark, each = ntimes - start.fishing + 1), nrow = ntimes - start.fishing + 1, ncol = nssmus)*catch.setup
    catch <- earmarks
  }
  #
  # option 2 is predator demand
  if(fishing.option == 2) {
    option.string <- "predator demand"
    tt <- obs.bias * init.tot.demand * exp(obs.errors)
    allocation <- tt/sum(tt)
    area.earmark <- (B0 * actual.gamma * allocation)/wbar.vector
    earmarks[start.fishing:ntimes,  ] <- matrix(rep(area.earmark, each = ntimes - start.fishing + 1), nrow = ntimes - start.fishing + 1, ncol = nssmus)*catch.setup
    catch <- earmarks
  }
  #
  # option 3 is standing stock
  if(fishing.option == 3) {
    option.string <- "krill standing stock"
    tt <- obs.bias * init.krill.biomass * exp(obs.errors)
    allocation <- tt/sum(tt)
    area.earmark <- (B0 * actual.gamma * allocation)/wbar.vector
    earmarks[start.fishing:ntimes,  ] <- matrix(rep(area.earmark, each = ntimes - start.fishing + 1), nrow = ntimes - start.fishing + 1, ncol = nssmus)*catch.setup
    catch <- earmarks
  }
  #
  # option 4 is (standing stock - predator demand)
  if(fishing.option == 4) {
    option.string <- "(krill standing stock - predator demand)"
    tt <- obs.bias * (init.krill.biomass - init.tot.demand) * exp(obs.errors)
    tt <- ifelse(tt < 0, 0, tt)
    allocation <- tt/sum(tt)
    area.earmark <- (B0 * actual.gamma * allocation)/wbar.vector
    earmarks[start.fishing:ntimes,  ] <- matrix(rep(area.earmark, each = ntimes - start.fishing + 1), nrow = ntimes - start.fishing + 1, ncol = nssmus)*catch.setup
    catch <- earmarks
  }
  #
  # option 5 is "adjustable"
  if(fishing.option == 5) {
    allocation <- rep(NA, nssmus)
    if(option5.list$base.option == 1) {
      base.string <- "historical catch"
      tt <- obs.bias * historical.catch * exp(obs.errors)
      base.allocation <- tt/sum(tt)
    }
    if(option5.list$base.option == 2) {
      base.string <- "predator demand"
      tt <- obs.bias * init.tot.demand * exp(obs.errors)
      base.allocation <- tt/sum(tt)
    }
    if(option5.list$base.option == 3) {
      base.string <- "krill standing stock"
      tt <- obs.bias * init.krill.biomass * exp(obs.errors)
      base.allocation <- tt/sum(tt)
    }
    if(option5.list$base.option == 4) {
      base.string <- "(krill standing stock - predator demand)"
      tt <- obs.bias * (init.krill.biomass - init.tot.demand) * exp(obs.errors)
      base.allocation <- tt/sum(tt)
      base.allocation <- ifelse(base.allocation < 0, 0, base.allocation)
    }
    if(!is.element(option5.list$base.option, 1:4)) {
      stop("fishing.option=5 must have base.option in the set {1, 2, 3, 4}")
    }
    option.string <- paste("adjustable with ", base.string, " as base allocation", sep = "")
    # assume that areas without monitoring are fished according to the base allocation
    area.earmark <- (B0 * actual.gamma * base.allocation)/wbar.vector
    earmarks[start.fishing:ntimes,  ] <- matrix(rep(area.earmark, each = ntimes - start.fishing + 1), nrow = ntimes - start.fishing + 1, ncol = nssmus)*catch.setup
    catch <- earmarks
    # assume that only seals and penguins will be monitored
    # NOTE: for now the code will not work correctly if bathtubs are not the last
    # elements in the option5.list$option5.areas
    coef.matrix <- matrix(0, nrow = nssmus, ncol = 2)
    for(i in 1:nssmus) {
      if(!is.na(option5.list$monitored.spp[i])){
        # if it's a monitored area reset the catch to zero because it will
  	# have to be a function of monitoring and is therefore done in the main simulation code
        catch[, i] <- 0
  	#
  	# note, "fake" krill densities have already been sampled
  	#
  	# set up matrices of relative performance to build predictive model
  	# relative performance is related to consumption, but I add observation error
  	# and there is the possibility that the observation is biased
        if(option5.list$monitored.spp[i] == "seals") {
          if(seals.params.list[[i]]$init.value == 0) {
            stop(paste("inconsistent input -- initial demand or abundance = 0 but monitoring seals in area ", option5.list$option5.areas[i], sep = ""))
          }
  	  # WARNING, the following is tricky ...
          # We need the row indices for tt.density and relQdev1
          # rather than go from 1:npoints[i] we want to go from say
          # (dim(tt.density)[1]-npoints[i]+1):dim(tt.density)[1].
          # This selects the LAST npoints in tt.density and relQdev1.
          # This doesn't matter with respect to using the random values, but does
          # allow us to update the adjustment model during each reassessment.
          # To do this update we need to substitute some relQdev2s in the argument list for relQdev1
          # because these are the relQdevs that will have occurred during the simulation period
          # also want to substitute simulated krill densities in for tt.density -- as I said, tricky stuff!
          samp.size <- min(dim(tt.density)[1], dim(relQdev1)[1], option5.list$npoints[i])
          tt.rows1 <- (dim(tt.density)[1]-samp.size+1):(dim(tt.density)[1])
          tt.rows2 <- (dim(relQdev1)[1]-samp.size+1):(dim(relQdev1)[1])
          #
          # want to base performance on performance across the foraging areas
          ttt.density <- t(apply(tt.density, 1, FUN=function(x){
            x * seals.params.list[[i]]$foraging.matrix[option5.list$monitoring.seasons[i],]
          }))
          ttt.density <- rowSums(ttt.density)
          #
          relQ <- (ttt.density[tt.rows1]^(seals.params.list[[i]]$Qq[option5.list$monitoring.seasons[i]] + 1))/((seals.params.list[[i]]$Qk5[option5.list$monitoring.seasons[i]]^(seals.params.list[[i]]$Qq[option5.list$monitoring.seasons[i]] + 1)) + (ttt.density[tt.rows1]^(seals.params.list[[i]]$Qq[option5.list$monitoring.seasons[i]] + 1)))
          # get deviations from mean (this is the y in Hewitt et al's y = alpha + beta*ln(x) + epsilon)
          relQ <- relQ - (sum(relQ)/length(relQ))
          # add in the bias and observation error
          relQ <- (relQ*option5.list$obs.mult[i]) + relQdev1[tt.rows2, i]
        }
        else {
          if(option5.list$monitored.spp[i] == "pengs") {
            if(pengs.params.list[[i]]$init.value == 0) {
              stop(paste("inconsistent input -- initial demand or abundance = 0 but monitoring pengs in area ",	option5.list$option5.areas[i], sep = ""))
            }
            samp.size <- min(dim(tt.density)[1], dim(relQdev1)[1], option5.list$npoints[i])
            tt.rows1 <- (dim(tt.density)[1]-samp.size+1):(dim(tt.density)[1])
            tt.rows2 <- (dim(relQdev1)[1]-samp.size+1):(dim(relQdev1)[1])
  	    #
  	    # want to base performance on performance across the foraging areas
            ttt.density <- t(apply(tt.density, 1, FUN=function(x){
              x * pengs.params.list[[i]]$foraging.matrix[option5.list$monitoring.seasons[i],]
            }))
            ttt.density <- rowSums(ttt.density)
            #
            relQ <- (ttt.density[tt.rows1]^(pengs.params.list[[i]]$Qq[option5.list$monitoring.seasons[i]] + 1))/((pengs.params.list[[i]]$Qk5[option5.list$monitoring.seasons[i]]^(pengs.params.list[[i]]$Qq[option5.list$monitoring.seasons[i]] + 1)) + (ttt.density[tt.rows1]^(pengs.params.list[[i]]$Qq[option5.list$monitoring.seasons[i]] + 1)))
            relQ <- relQ - (sum(relQ)/length(relQ))
            relQ <- (relQ*option5.list$obs.mult[i]) + relQdev1[tt.rows2, i]
          }
          else {
            stop("problem in option5.list -- monitored.spp must be seals or pengs")
          }
        }
  	#
  	# now fit an approximating model to the data
  	# the approximating model is y=alpha+(beta*log(x)) as per Hewitt et al. 2004
        coef.matrix[i,  ] <- as.vector(coef(lm(relQ ~ log(ttt.density[tt.rows1]))))
      }
    }
    #print(relQ)
    #print(coef.matrix)
  }
  #
  # option 6 is pulse fishing
  if(fishing.option == 6) {
    allocation <- rep(NA, nssmus)
    option.string <- "pulse fishing"
    area.earmark <- (B0 * actual.gamma)/wbar.vector
    # new code should be more general because catch.setup can accomodate any pulse scheme
    # you might think up
    earmarks[start.fishing:ntimes,  ] <- t(apply(catch.setup,1,FUN=function(x){x*area.earmark}))
    catch <- earmarks
  }
  #print("allocation fraction")
  #print(round(allocation,4))
  #print("B0")
  #print(B0)
  #print("allocation amount")
  #print(round(earmarks[start.fishing,]/1E10,4))
  #print(sum(round(earmarks[start.fishing,]/1E10,4)))
  #
  # for now set actual catch (catch) = allocated catch (earmarks)
  # when krill is limiting then actual catch < allocated catch
  # and the catch matrix gets revised in the main body of ssmu.ss()
  if(fishing.option==5){
    list(catch=catch, earmarks=earmarks, base.allocation=base.allocation, allocation=allocation, option.string=option.string, coef.matrix=coef.matrix)
  }
  else {
    list(catch=catch, earmarks=earmarks, allocation=allocation, option.string=option.string)
  }
}
