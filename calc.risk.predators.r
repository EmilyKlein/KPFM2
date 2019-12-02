calc.risk.predators <- function(mc.object, depletion.fraction = 0.75, recovery.fraction = 0.75)
{
  # auxiliary function to compute risk measures for predators
  #
  # George Watters
  # last edited 11 November 2007
  # 
  # orginally called calc.risk; renamed on 11/1/12 by JM
	
  # stop if the user didn't simulate fishing	
	if(is.null(mc.object$setup$start.fishing)){
	  stop("Hey dork -- none of the PMs can be calculated because you didn't fish!")
	}
  # set up part of the output list hierarchy
  out <- vector(mode="list",length=4)
  names(out) <- c("call","mc.calls","risk","color.tags")
  out$risk <- vector(mode="list",length=4)
  names(out$risk)<-c("seals","pengs","whales","fish")
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
    tag.labels <- paste("tag",unique(mc.object$color.tags),sep=".")
  }
  ssmu.labels <- paste("ssmu",1:nssmus,sep=".")
  #
  start.fishing <- mc.object$setup$start.fishing
  stop.fishing <- mc.object$setup$stop.fishing
  #
  # ******************  NOW THE RISK MEASURES  ************************************************
  #
  out$risk$seals <- vector(mode="list",length=2)
  names(out$risk$seals)<-c("P.depleted","P.notrecovered")
  out$risk$pengs <- out$risk$whales <- out$risk$fish <- out$risk$seals
  #  
  maxRage <- list(seals = mc.object$R$maxRage$seals, pengs = mc.object$R$maxRage$pengs, whales = mc.object$R$maxRage$whales, fish = mc.object$R$maxRage$fish)
  #
  for(i in names(out$risk)){
    #
    tt.N <- mc.object$N[[i]]
    #
    # first check to see if this mc.object is "relative"
    # if not relative then depletion and recovery defined relative to pre-exploitation abundance
    # if relative then depletion and recovery defined from the relative abundance itself
    if(is.null(mc.object$setup$relative)){
      tt.N <- apply(tt.N , c(2,3), FUN=function(x){
        x/x[(maxRage[[i]]+start.fishing-1)]
      })
    }
    #
    # now just snarf out the row we need and then build ssmu.vector for using tapply()
    tt.depleted <- as.vector(ifelse(tt.N[(maxRage[[i]]+stop.fishing-1),,] < depletion.fraction,1,0))
    # assess recovery either at ntimes or at 30 years after stop fishing, whichever is shorter
    recovery.time <- min(ntimes,(maxRage[[i]]+stop.fishing+(nseasons*30)-1))
    tt.notrecovered <- as.vector(ifelse(tt.N[recovery.time,,] < recovery.fraction,1,0))
    ssmu.vector<-rep(1:nssmus,ntrials)
    if(is.null(mc.object$color.tags)){
      P.depleted<-tapply(tt.depleted,ssmu.vector,FUN=function(x){sum(x)/ntrials})
      names(P.depleted)<-ssmu.labels
      P.notrecovered<-tapply(tt.notrecovered,ssmu.vector,FUN=function(x){sum(x)/ntrials})
      names(P.notrecovered)<-ssmu.labels
    } else {
      tags.vector<-rep(mc.object$color.tags,each=nssmus)
      P.depleted<-tapply(tt.depleted,list(tags.vector,ssmu.vector),FUN=function(x){sum(x)/(ntrials/length(unique(mc.object$color.tags)))})
      dimnames(P.depleted)<-list(tag.labels,ssmu.labels)
      P.notrecovered<-tapply(tt.notrecovered,list(tags.vector,ssmu.vector),FUN=function(x){sum(x)/(ntrials/length(unique(mc.object$color.tags)))})
      dimnames(P.notrecovered)<-list(tag.labels,ssmu.labels)
    }
    # TO DO ----- probably want to add depletion and not recovering risks for all SSMUs combined
    out$risk[[i]]$P.depleted<-P.depleted
    out$risk[[i]]$P.notrecovered<-P.notrecovered
  }
  
  #
  out
}  
	
		
	
