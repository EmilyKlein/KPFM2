calc.pms.krill.fishery <-function(mc.object, annual=TRUE, connector="default", threshold.density=NULL, avg.weight=0.46)
{
  # auxiliary function to calculate so-called summary performance measures
  # for the fishery and for krill -- THERE ARE NO PREDATOR PMs AND NO AGGREGATE PMs HERE!
  #
  # originally called calc.pms.nopred()
  # George Watters
  # last edited 30 May 2008
  #
  # renamed by JM on 11/01/12
  # a section disabled by JM 1/2/12 - see below
	
  # stop if the user didn't simulate fishing
  if(is.null(mc.object$setup$start.fishing)){
    stop("Hey dork -- none of the PMs can be calculated because you didn't fish!")
  }
  # set up part of the output list hierarchy
  out <- vector(mode="list",length=5)
  names(out) <- c("call","mc.calls","krill","fishery","color.tags")
  #
  ntimes <- mc.object$setup$ntimes
  nseasons <- mc.object$setup$nseasons
  nssmus <- mc.object$setup$nssmus
  ntrials <- mc.object$setup$ntrials
  # yippee some stuff is easy to program!
  out$call <- match.call()
  out$mc.calls <- mc.object$call
  out$color.tags <- mc.object$color.tags
  if(!is.null(mc.object$color.tags)){
    object.labels <- paste("tag",unique(mc.object$color.tags),sep=".")
  }
  ssmu.labels <- paste("ssmu",1:nssmus,sep=".")
  if(!is.null(mc.object$color.tags)){
    tag.labels <- paste("tag",unique(mc.object$color.tags),sep=".")
  }
  #
  #

  # ******************  FIRST THE FISHERY PERFORMANCE MEASURES  ************************************************
  #
  out$fishery <- vector(mode="list",length=5)
  names(out$fishery) <- c("mean.catch","cv.catch","mean.relative.catch","cv.relative.catch","P.threshold.violation")
  #
  # NOTE from calc.pms.r: "summary PMs have NOT been completed for fishery"
  #
  start.fishing <- mc.object$setup$start.fishing
  stop.fishing <- mc.object$setup$stop.fishing
  #
  # first get the appropriate catch and allocation matrices to work with depending on whether
  # user wants to consider performance for annualized (and how) or seasonal simulation output	
  if(annual){
    start.fishing <- start.fishing%/%nseasons+1
    stop.fishing <- stop.fishing%/%nseasons+1
    season.vector <- rep(1:nseasons, length.out = ntimes)
    year.vector <- rep(1:(ntimes/nseasons), each=nseasons)
    if(is.character(connector)){
      # sum up the catches by year
      tt.catch <- array(apply(mc.object$fishery$catch, 3, FUN = function(x){
        apply(x, 2, FUN=function(x){
          tapply(x,list(year.vector), sum, na.rm=TRUE)
        })
      }), dim=c(ntimes/nseasons, nssmus, ntrials))
      # sum up the allocation by year
      tt.allocation <- array(apply(mc.object$fishery$allocation, 3, FUN = function(x){
        apply(x, 2, FUN=function(x){
          tapply(x,list(year.vector), sum, na.rm=TRUE)
        })
      }), dim=c(ntimes/nseasons, nssmus, ntrials))
    }
    if(is.numeric(connector)){
      # just use the stuff in the season of choice
      # first check that the connector is a feasible season
      if(connector > mc.object$setup$nseasons){stop("FAULT: connector season > nseasons")}
      keepers <- (season.vector == connector)
      tt.allocation <- mc.object$fishery$allocation[keepers,,]
      tt.catch <- mc.object$fishery$catch[keepers,,]
    }
  }
  else {
    tt.allocation <- mc.object$fishery$allocation
    tt.catch <- mc.object$fishery$catch
  }

  # NOTE:  could probably speed up this code by using fewer apply statements
  # and using a single for() loop across ntrials and then putting a bunch of
  # the PMs inside the for() loop...  -- ACTUALLY NOT BASED ON A SMALL TEST!
  #
  # now make the catch=NA in times where allocation=0
  # this will ensure that fishery performance is computed appropriately
  # if na.rm=TRUE in calls to colMeans etc. (i.e. don't penalize for zero
  # performance when the allocation was intended to be zero)
  tt.catch[tt.allocation == 0] <- NA
  #
  # Fishery PM1: SSMU-specific mean catch (over all trials)
  # Fishery PM2: SSMU-specific CV of catch (over all trials)
  tt <- as.vector(tt.catch[start.fishing:(stop.fishing - 1), , ])
  # convert to thousands of metric tons
  tt <- tt*avg.weight/1000/1000/1000
  nyearsfishing <- length(start.fishing:(stop.fishing -1))
  ssmu.vector <- rep(rep(1:nssmus,each=nyearsfishing),ntrials)
  if(is.null(mc.object$color.tags)){
    pm1 <- tapply(tt,list(ssmu.vector),mean,na.rm=TRUE)
    pm2 <- tapply(tt,list(ssmu.vector),sd,na.rm=TRUE)/pm1
    names(pm1)<-names(pm2)<-ssmu.labels
  } else {
    tags.vector <- rep(mc.object$color.tags,each=nyearsfishing*nssmus)
    pm1<-tapply(tt,list(tags.vector,ssmu.vector),mean,na.rm=TRUE)
    pm2<-tapply(tt,list(tags.vector,ssmu.vector),sd,na.rm=TRUE)/pm1
    dimnames(pm1)<-dimnames(pm2)<-list(tag.labels,ssmu.labels)
  }
  #
  out$fishery$mean.catch <- pm1
  out$fishery$cv.catch <- pm2
  #

  # Use similar logic to compute performance relative to the allocation
  # Fishery PM3: SSMU-specific mean relative catch (over all trials)
  # Fishery PM4: SSMU-specific CV of relative catch (over all trials)
  tt <- as.vector((tt.catch/tt.allocation)[start.fishing:(stop.fishing - 1), , ])
  nyearsfishing <- length(start.fishing:(stop.fishing -1))
  ssmu.vector <- rep(rep(1:nssmus,each=nyearsfishing),ntrials)
  if(is.null(mc.object$color.tags)){
    pm3 <- tapply(tt,list(ssmu.vector),mean,na.rm=TRUE)
    pm4 <- tapply(tt,list(ssmu.vector),sd,na.rm=TRUE)/pm3
    names(pm3)<-names(pm4)<-ssmu.labels
  } else {
    tags.vector <- rep(mc.object$color.tags,each=nyearsfishing*nssmus)
    pm3<-tapply(tt,list(tags.vector,ssmu.vector),mean,na.rm=TRUE)
    pm4<-tapply(tt,list(tags.vector,ssmu.vector),sd,na.rm=TRUE)/pm3
    dimnames(pm3)<-dimnames(pm4)<-list(tag.labels,ssmu.labels)
  }
  #
  out$fishery$mean.relative.catch <- pm3
  out$fishery$cv.relative.catch <- pm4
  #

  # Fishery PM5:  Probability of having "involuntary" adjustment to fishing strategy because
  # krill density crosses an SSMU-specific threshold -- by default, thresholds are specified in parameter list for the
  # original model run and mc.object$fishery$threshold.violations is a count of the number of such violations
  # per trial per SSMU (threshold.violations is a matrix with dimensions ntrials by nssmus)
  #
  # first find the possible times in which the threshold could be violated
  krill.Rage <- mc.object$R$maxRage$krill
  tt.times <- array(FALSE,dim=c(ntimes,nssmus,ntrials))
  tt.times[mc.object$setup$start.fishing:(mc.object$setup$stop.fishing-1),,]<-TRUE
  tt.times[mc.object$fishery$allocation==0]<-FALSE
  # now make a matrix of the count of TRUEs where the result is ntrials*nssmus so it is comparable to threshold.violations
  tt.times <- t(apply(tt.times,3,FUN=function(x){
    apply(x,2,FUN=function(x){
      ttt<-factor(x,levels=c("FALSE","TRUE"))
      table(ttt)[2]
    })
  }))
  #
  # first condition is to evaluate performance as recorded in the original simulations

#########################JM DISABLED####################################

#  if(is.null(threshold.density)){
#    nyearsfishing <- length(start.fishing:(stop.fishing -1))
#    ssmu.vector <- rep(1:nssmus,each=ntrials)
#    tt1<-as.vector(mc.object$fishery$threshold.violations)
#    tt2<-as.vector(tt.times)
#    if(is.null(mc.object$color.tags)){
#      pm5 <- tapply(tt1,list(ssmu.vector),sum,na.rm=TRUE)/tapply(tt2,list(ssmu.vector),sum,na.rm=TRUE)
#      names(pm5)<-ssmu.labels
#    } else {
#      tags.vector<-rep(mc.object$color.tags,nssmus)
#      pm5 <- tapply(tt1,list(tags.vector,ssmu.vector),sum,na.rm=TRUE)/tapply(tt2,list(tags.vector,ssmu.vector),sum,na.rm=TRUE)
#      dimnames(pm5)<-list(tag.labels,ssmu.labels)
#    }
#  } else {
#    # second condition is to evaluate performance relative to a post-hoc, user-supplied density threshold
#    if(length(threshold.density)==1) threshold.density<-rep(threshold.density,nssmus)
#    if(length(threshold.density)!=nssmus) stop("threshold.density should be a scalar or a vector of length nssmus")
#    if(is.call(mc.object$call)){
#      wbar.vector <- as.vector(unlist(lapply(eval(mc.object$call$param.list)$KRILL,FUN=function(x){x$wbar})))[1:nssmus]
#      AREAS <- eval(mc.object$call$param.list)$AREAS[1:nssmus]
#    } else {
#      wbar.vector <- as.vector(unlist(lapply(eval(mc.object$call[[1]]$param.list)$KRILL,FUN=function(x){x$wbar})))[1:nssmus]
#      AREAS <- eval(mc.object$call[[1]]$param.list)$AREAS[1:nssmus]
#    }
#    tt.density <- array(0,dim=c(ntimes,nssmus,ntrials))
#    tt.TF <- array(FALSE,dim=c(ntimes,nssmus,ntrials))
#    # I tried all sorts of stuff with apply etc. and couldn't get it to work right -- FRUSTRATION!
#    # frick, it wasn't my use of apply, it was my indexing -- note that krill density is now offset by 1 time period
#    # as per its use in ssmu.ss()
#    for(k in 1:ntrials){
#      for(j in 1:nssmus){
#        for(i in 1:ntimes){
#          tt.density[i,j,k]<-mc.object$N$krill[(krill.Rage+i),j,k]*wbar.vector[j]/AREAS[j]
#          tt.TF[i,j,k]<-(tt.density[i,j,k]<threshold.density[j])
#        }
#      }
#    }
#    tt.TF[mc.object$fishery$allocation==0]<-FALSE
#    # now reshape tt.TF so that it has dimensions ntrials x nssmus (as per the original threshold.violations)
#    tt.TF <- apply(tt.TF,c(3,2),sum) 
#    # now use the same logic as before to get the PM
#    nyearsfishing <- length(start.fishing:(stop.fishing -1))
#    ssmu.vector <- rep(1:nssmus,each=ntrials)
#    tt1<-as.vector(tt.TF)
#    tt2<-as.vector(tt.times)
#    if(is.null(mc.object$color.tags)){
#      pm5 <- tapply(tt1,list(ssmu.vector),sum,na.rm=TRUE)/tapply(tt2,list(ssmu.vector),sum,na.rm=TRUE)
#      names(pm5)<-ssmu.labels
#    } else {
#      tags.vector<-rep(mc.object$color.tags,nssmus)
#      pm5 <- tapply(tt1,list(tags.vector,ssmu.vector),sum,na.rm=TRUE)/tapply(tt2,list(tags.vector,ssmu.vector),sum,na.rm=TRUE)
#      dimnames(pm5)<-list(tag.labels,ssmu.labels)
#    }
#  }
#  out$fishery$P.threshold.violation <- pm5
#  #
#  #
############################ JM DISABLED ###################################

  # ******************  NEXT THE SUMMARIZED KRILL PERFORMANCE MEASURES  ************************************************
  #
  out$krill <- vector(mode="list",length=2)
  names(out$krill)<-c("rule1","rule2")
  #  
  start.fishing <- mc.object$setup$start.fishing
  stop.fishing <- mc.object$setup$stop.fishing
  maxRage <- mc.object$R$maxRage$krill
  #
  # drop off the bathtubs
  tt.krill <- mc.object$N$krill[,1:nssmus,]
  #
  # first get the appropriate matrices to work with depending on whether
  # user wants to consider performance for annualized (and how) or seasonal simulation output	
  if(annual){
    start.fishing <- start.fishing%/%nseasons+1
    stop.fishing <- stop.fishing%/%nseasons+1
    maxRage <- maxRage/nseasons
    if(is.character(connector)){
      # avg abundaces within a year
      year.vector <- rep(1:(dim(tt.krill)[1]/nseasons),each=nseasons)
      tt.krill <- array(apply(tt.krill, 3, FUN = function(x){
        apply(x, 2, FUN=function(x){
          tapply(x,list(year.vector), mean, na.rm=TRUE)
        })
      }), dim=c(length(unique(year.vector)), nssmus, ntrials))
      #
    }
    if(is.numeric(connector)){
      # just use the stuff in the season of choice
      # first check that the connector is a feasible season
      if(connector > mc.object$setup$nseasons){stop("FAULT: connector season > nseasons")}
      season.vector <- rep(1:nseasons,length.out=dim(tt.krill)[1])
      keepers <- (season.vector == connector)
      tt.krill <- tt.krill[keepers,,]
    }
  }
  #

  # OK do the summarization
  if(is.null(mc.object$color.tags)){
    # get median pre-exploitation abundance for each SSMU
    # note this should work BOTH for results that are relativized and are not relativized
    med.preX <- t(tt.krill[(maxRage+start.fishing-1),,])
    med.preX <- apply(med.preX,2,median)
    #
    # now compute probability of violating "rule 1" -- SSMU-specific
    # proportion of trials in which MINIMUM krill abundance is below 20% of
    # pre-exploitation median during the fishing period
    #
    # get SSMU- and trial-specific minima during the fishing period
    min.ssmu <- t(apply(tt.krill,3,FUN=function(x){
      apply(x,2,FUN=function(x){
        min(x[(maxRage+start.fishing):(maxRage+stop.fishing-1)])
      })
    }))
    # now have mini-trickyness and bind the medians onto the mins so that we can use a call to apply
    # rather than a loop
    min.ssmu <- rbind(med.preX,min.ssmu)
    rule1 <- apply(min.ssmu,2,FUN=function(x){
      thresh <- x[1]*0.2
      ttt <- x[2:length(x)]<thresh
      ttt <- table(factor(ttt,levels=c("FALSE","TRUE")))
      ttt[2]/sum(ttt)
    })
    names(rule1)<-ssmu.labels
    out$krill$rule1 <- rule1
    #
    # now compute probability of violating "rule 2" -- SSMU-specific
    # proportion of trials in which krill abundance at end of
    # fishing period is below 75% of pre-exploitation median
    #
    # get SSMU- and trial-specific end points
    end.ssmu <- t(tt.krill[(maxRage+stop.fishing-1),,])
    # now bind as before and use apply
    end.ssmu <- rbind(med.preX,end.ssmu)
    rule2 <- apply(end.ssmu,2,FUN=function(x){
      thresh <- x[1]*0.75
      ttt <- x[2:length(x)]<thresh
      ttt <- table(factor(ttt,levels=c("FALSE","TRUE")))
      ttt[2]/sum(ttt)
    })
    names(rule2)<-ssmu.labels
    out$krill$rule2 <- rule2
  } else {
    tt1 <- matrix(NA,nrow=length(unique(mc.object$color.tags)),ncol=nssmus)
    tt2 <- tt1
    for(i in unique(mc.object$color.tags)){
      #
      # get median pre-exploitation abundance for each SSMU
      med.preX <- t(tt.krill[(maxRage+start.fishing-1),,(mc.object$color.tags==i)])
      med.preX <- apply(med.preX,2,median)
      #
      min.ssmu <- t(apply(tt.krill[,,(mc.object$color.tags==i)],3,FUN=function(x){
        apply(x,2,FUN=function(x){
          min(x[(maxRage+start.fishing):(maxRage+stop.fishing-1)])
        })
      }))
      #
      min.ssmu <- rbind(med.preX,min.ssmu)
      rule1 <- apply(min.ssmu,2,FUN=function(x){
        thresh <- x[1]*0.2
        ttt <- x[2:length(x)]<thresh
        ttt <- table(factor(ttt,levels=c("FALSE","TRUE")))
        ttt[2]/sum(ttt)
      })
      tt1[i,]<-rule1
      #
      end.ssmu <- t(tt.krill[(maxRage+stop.fishing-1),,(mc.object$color.tags==i)])
      #
      end.ssmu <- rbind(med.preX,end.ssmu)
      rule2 <- apply(end.ssmu,2,FUN=function(x){
        thresh <- x[1]*0.75
        ttt <- x[2:length(x)]<thresh
        ttt <- table(factor(ttt,levels=c("FALSE","TRUE")))
        ttt[2]/sum(ttt)
      })
      tt2[i,]<-rule2
    }
    dimnames(tt1)<-list(object.labels,ssmu.labels)
    dimnames(tt2)<-list(object.labels,ssmu.labels)
    out$krill$rule1 <- tt1
    out$krill$rule2 <- tt2
  }
  #
  out
}  
	
		
	
