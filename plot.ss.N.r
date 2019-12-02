plot.ss.N<-function(ss.object, annual.plots=TRUE, connector="avg", page.layout = c(4, 4), Nylimits = c(0, 3))
{
	# auxiliary function to plot abundance from single run of predator-prey model
	# George Watters
	# code last edited 14 June 2006
	#
	# ss.object ("single sim object") should have the following components to work with this function:
	# krill, krill.Rage, seals, seals.maxRage, pengs, pengs.maxRage, whales, whales.maxRage, fish, fish.maxRage
  #
  if(annual.plots){
    season.vector <- rep(1:ss.object$setup$nseasons, length.out = ss.object$setup$ntimes)
    year.vector <- rep(1:(ss.object$setup$ntimes/ss.object$setup$nseasons),each=ss.object$setup$nseasons)
    time.label<-"year"
    if(is.character(connector)){
      plot.time <- c(0,unique(year.vector))
      krill.denom <- rep(0,ss.object$setup$nssmus)
      seals.denom <- pengs.denom <- whales.denom <- fish.denom <- krill.denom
      krill.y <- matrix(0,nrow=length(unique(year.vector)),ncol=ss.object$setup$nssmus)
      seals.y <- pengs.y <- whales.y <- fish.y <- krill.y
      for(i in 1:ss.object$setup$nssmus){
        krill.denom[i] <- mean(ss.object$N$krill[1:ss.object$setup$nseasons,i])
        seals.denom[i] <- mean(ss.object$N$seals[1:ss.object$setup$nseasons,i])
        pengs.denom[i] <- mean(ss.object$N$pengs[1:ss.object$setup$nseasons,i])
        whales.denom[i] <- mean(ss.object$N$whales[1:ss.object$setup$nseasons,i])
        fish.denom[i] <- mean(ss.object$N$fish[1:ss.object$setup$nseasons,i])        
        #
        krill.y[,i] <- as.vector(tapply(ss.object$N$krill[((ss.object$R$maxRage$krill+1):(ss.object$R$maxRage$krill + ss.object$setup$ntimes)),i],list(year.vector),mean))
        seals.y[,i] <- as.vector(tapply(ss.object$N$seals[((ss.object$R$maxRage$seals+1):(ss.object$R$maxRage$seals + ss.object$setup$ntimes)),i],list(year.vector),mean))
        pengs.y[,i] <- as.vector(tapply(ss.object$N$pengs[((ss.object$R$maxRage$pengs+1):(ss.object$R$maxRage$pengs + ss.object$setup$ntimes)),i],list(year.vector),mean))
        whales.y[,i] <- as.vector(tapply(ss.object$N$whales[((ss.object$R$maxRage$whales+1):(ss.object$R$maxRage$whales + ss.object$setup$ntimes)),i],list(year.vector),mean))
        fish.y[,i] <- as.vector(tapply(ss.object$N$fish[((ss.object$R$maxRage$fish+1):(ss.object$R$maxRage$fish + ss.object$setup$ntimes)),i],list(year.vector),mean))
      }
      title.prefix <- "avgN/avgN[yr1]"
    }
    if(is.numeric(connector)){
      # first check that the connector is a feasible season
      if(connector > ss.object$setup$nseasons){stop("FAULT: connector season > nseasons")}
      keepers <- (season.vector == connector)
      #
      plot.time <- c(0,year.vector[keepers])
      krill.denom <- ss.object$N$krill[connector, ]
      seals.denom <- ss.object$N$seals[connector, ]
      pengs.denom <- ss.object$N$pengs[connector, ]
      whales.denom <- ss.object$N$whales[connector, ]
      fish.denom <- ss.object$N$fish[connector, ]
      #
      krill.y <- ss.object$N$krill[((ss.object$R$maxRage$krill+1):(ss.object$R$maxRage$krill + ss.object$setup$ntimes))[keepers], ]
      seals.y <- ss.object$N$seals[((ss.object$R$maxRage$seals+1):(ss.object$R$maxRage$seals + ss.object$setup$ntimes))[keepers], ]
      pengs.y <- ss.object$N$pengs[((ss.object$R$maxRage$pengs+1):(ss.object$R$maxRage$pengs + ss.object$setup$ntimes))[keepers], ]
      whales.y <- ss.object$N$whales[((ss.object$R$maxRage$whales+1):(ss.object$R$maxRage$whales + ss.object$setup$ntimes))[keepers], ]
      fish.y <- ss.object$N$fish[((ss.object$R$maxRage$fish+1):(ss.object$R$maxRage$fish + ss.object$setup$ntimes))[keepers], ]
      #
      if(ss.object$setup$nssmus==1){
        krill.y <- as.matrix(krill.y)
        seals.y <- as.matrix(seals.y)
        pengs.y <- as.matrix(pengs.y)
        whales.y <- as.matrix(whales.y)
        fish.y <- as.matrix(fish.y)
      }
      #
      title.prefix <- paste("N",connector,"/N",connector,"[yr1]",sep="")
    }
    if(!is.null(ss.object$setup$reassessment.times)){
      vlines <- ss.object$setup$reassessment.times/ss.object$setup$nseasons
    }
  }
  else {
    plot.time <- 0:ss.object$setup$ntimes
    keepers <- rep(TRUE,ss.object$setup$ntimes)
    #
    krill.denom <- ss.object$N$krill[1, ]
    seals.denom <- ss.object$N$seals[1, ]
    pengs.denom <- ss.object$N$pengs[1, ]
    whales.denom <- ss.object$N$whales[1, ]
    fish.denom <- ss.object$N$fish[1, ]
    #
    krill.y <- ss.object$N$krill[((ss.object$R$maxRage$krill+1):(ss.object$R$maxRage$krill + ss.object$setup$ntimes))[keepers], ]
    seals.y <- ss.object$N$seals[((ss.object$R$maxRage$seals+1):(ss.object$R$maxRage$seals + ss.object$setup$ntimes))[keepers], ]
    pengs.y <- ss.object$N$pengs[((ss.object$R$maxRage$pengs+1):(ss.object$R$maxRage$pengs + ss.object$setup$ntimes))[keepers], ]
    whales.y <- ss.object$N$whales[((ss.object$R$maxRage$whales+1):(ss.object$R$maxRage$whales + ss.object$setup$ntimes))[keepers], ]
    fish.y <- ss.object$N$fish[((ss.object$R$maxRage$fish+1):(ss.object$R$maxRage$fish + ss.object$setup$ntimes))[keepers], ]
    #
    if(ss.object$setup$nssmus==1){
      krill.y <- as.matrix(krill.y)
      seals.y <- as.matrix(seals.y)
      pengs.y <- as.matrix(pengs.y)
      whales.y <- as.matrix(whales.y)
      fish.y <- as.matrix(fish.y)
    }
    #
    title.prefix <- "N/N[1]"
    time.label<-"season"
    if(!is.null(ss.object$setup$reassessment.times)){
      vlines <- ss.object$setup$reassessment.times
    }
  }
  #
  #
  windows()
  origpar <- par(no.readonly=TRUE)
	#par(oma = c(0, 0, 2, 0), mfrow = page.layout)
	par(oma = c(4, 2, 4, 3), mar=c(4,4,1,0)+0.1, mgp=c(2,0.75,0), xpd=FALSE, mfrow = page.layout)
	panel.count <- 1
	left.col.panels <- seq(from=1,to=page.layout[1]*page.layout[2],by=page.layout[2])
	bottom.panels <- (1:(page.layout[1]*page.layout[2]))[max(left.col.panels):((page.layout[1]*page.layout[2]))]
	for(i in 1:ss.object$setup$nssmus) {
		if(panel.count > (page.layout[1] * page.layout[2])) {
			panel.count <- 1
			par(origpar)
			windows()
			#par(oma = c(0, 0, 2, 0), mfrow = page.layout)
			par(oma = c(4, 2, 4, 3), mar=c(4,4,1,0)+0.1, mgp=c(2,0.75,0), xpd=FALSE, mfrow = page.layout)
		}
		if(is.element(panel.count,left.col.panels)){ylabel<-"relative abundance"}else{ylabel<-""}
  	if(is.element(panel.count,bottom.panels)){xlabel<-time.label}else{xlabel<-""}
		plot(plot.time, c(krill.denom[i],krill.y[,i])/krill.denom[i], type = "l", xlab = xlabel, ylab =
			ylabel, ylim = Nylimits,axes=FALSE)
    lines(plot.time, c(seals.denom[i],seals.y[,i])/seals.denom[i], col = "red")
		lines(plot.time, c(pengs.denom[i],pengs.y[,i])/pengs.denom[i], col = "blue")
		lines(plot.time, c(whales.denom[i],whales.y[,i])/whales.denom[i], col = "green")
		lines(plot.time, c(fish.denom[i],fish.y[,i])/fish.denom[i], lty = 4)
		box()
		axis(1,cex.axis=0.8)
		axis(2,cex.axis=0.8)
		if(!is.null(ss.object$setup$reassessment.times)){
      abline(v=vlines,lty=3)
		}
		
    title(main=paste("SSMU ", i, sep = ""), outer = FALSE, line = 0.5, cex.main = 0.9)
		panel.count <- panel.count + 1
		if(panel.count > (page.layout[1] * page.layout[2])) {
			mtext(paste(title.prefix, " -- krill (black), seals (red), pengs (blue), whales (green), fish (dash)",sep=""), outer
				 = TRUE, line = 1, cex = 0.75)
		}
	}
	mtext(paste(title.prefix," -- krill (black), seals (red), pengs (blue), whales (green), fish (dash)",sep=""), outer = TRUE, line = 1,
		cex = 0.75)
  par(origpar)
}