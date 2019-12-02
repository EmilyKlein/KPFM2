plot.ss.R<-function(ss.object, annual.plots=TRUE, connector="sum", page.layout = c(4, 4), Rylimits = c(0, 3))
{
	# auxiliary function to plot recruitment from single run of predator-prey model
	# George Watters
	# code last edited 14 June 2006
	#
  if(annual.plots){
    season.vector <- rep(1:ss.object$setup$nseasons, length.out = ss.object$setup$ntimes)
    year.vector <- rep(1:(ss.object$setup$ntimes/ss.object$setup$nseasons),each=ss.object$setup$nseasons)
    time.label<-"year"
    if(is.character(connector)){
      plot.time <- unique(year.vector)
      krill.denom <- rep(0,ss.object$setup$nssmus)
      seals.denom <- pengs.denom <- whales.denom <- fish.denom <- krill.denom
      krill.y <- matrix(0,nrow=length(unique(year.vector)),ncol=ss.object$setup$nssmus)
      seals.y <- pengs.y <- whales.y <- fish.y <- krill.y
      for(i in 1:ss.object$setup$nssmus){
        krill.denom[i] <- sum(ss.object$R$R$krill[1:ss.object$setup$nseasons,i])
        seals.denom[i] <- sum(ss.object$R$R$seals[1:ss.object$setup$nseasons,i])
        pengs.denom[i] <- sum(ss.object$R$R$pengs[1:ss.object$setup$nseasons,i])
        whales.denom[i] <- sum(ss.object$R$R$whales[1:ss.object$setup$nseasons,i])
        fish.denom[i] <- sum(ss.object$R$R$fish[1:ss.object$setup$nseasons,i])
        #
        krill.y[,i] <- as.vector(tapply(ss.object$R$R$krill[,i],list(year.vector),sum))
        seals.y[,i] <- as.vector(tapply(ss.object$R$R$seals[,i],list(year.vector),sum))
        pengs.y[,i] <- as.vector(tapply(ss.object$R$R$pengs[,i],list(year.vector),sum))
        whales.y[,i] <- as.vector(tapply(ss.object$R$R$whales[,i],list(year.vector),sum))
        fish.y[,i] <- as.vector(tapply(ss.object$R$R$fish[,i],list(year.vector),sum))
      }
      title.prefix <- "sumR/sumR[yr1]"
    }
    if(is.numeric(connector)){
      # first check that the connector is a feasible season
      if(connector > ss.object$setup$nseasons){stop("FAULT: connector season > nseasons")}
      keepers <- (season.vector == connector)
      #
      plot.time <- year.vector[keepers]
      krill.denom <- ss.object$R$R$krill[connector, ]
      seals.denom <- ss.object$R$R$seals[connector, ]
      pengs.denom <- ss.object$R$R$pengs[connector, ]
      whales.denom <- ss.object$R$R$whales[connector, ]
      fish.denom <- ss.object$R$R$fish[connector, ]
      #
      krill.y <- ss.object$R$R$krill[keepers, ]
      seals.y <- ss.object$R$R$seals[keepers, ]
      pengs.y <- ss.object$R$R$pengs[keepers, ]
      whales.y <- ss.object$R$R$whales[keepers, ]
      fish.y <- ss.object$R$R$fish[keepers, ]
      #
      if(ss.object$setup$nssmus==1){
        krill.y <- as.matrix(krill.y)
        seals.y <- as.matrix(seals.y)
        pengs.y <- as.matrix(pengs.y)
        whales.y <- as.matrix(whales.y)
        fish.y <- as.matrix(fish.y)
      }
      #
      title.prefix <- paste("R",connector,"/R",connector,"[yr1]",sep="")
    }
    if(!is.null(ss.object$setup$reassessment.times)){
      vlines <- ss.object$setup$reassessment.times/ss.object$setup$nseasons
    }
  }
  else {
    plot.time <- 1:ss.object$setup$ntimes
    keepers <- rep(TRUE,ss.object$setup$ntimes)
    #
    krill.denom <- ss.object$R$R$krill[1, ]
    seals.denom <- ss.object$R$R$seals[1, ]
    pengs.denom <- ss.object$R$R$pengs[1, ]
    whales.denom <- ss.object$R$R$whales[1, ]
    fish.denom <- ss.object$R$R$fish[1, ]
    #
    krill.y <- ss.object$R$R$krill[keepers, ]
    seals.y <- ss.object$R$R$seals[keepers, ]
    pengs.y <- ss.object$R$R$pengs[keepers, ]
    whales.y <- ss.object$R$R$whales[keepers, ]
    fish.y <- ss.object$R$R$fish[keepers, ]
    #
    if(ss.object$setup$nssmus==1){
      krill.y <- as.matrix(krill.y)
      seals.y <- as.matrix(seals.y)
      pengs.y <- as.matrix(pengs.y)
      whales.y <- as.matrix(whales.y)
      fish.y <- as.matrix(fish.y)
    }
    #
    title.prefix <- "R/R[1]"
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
		if(is.element(panel.count,left.col.panels)){ylabel<-"relative recruitment"}else{ylabel<-""}
  	if(is.element(panel.count,bottom.panels)){xlabel<-time.label}else{xlabel<-""}
		plot(plot.time, krill.y[,i]/krill.denom[i], type = "l", xlab = xlabel, ylab = ylabel, ylim = Rylimits,axes=FALSE)
		lines(plot.time, seals.y[,i]/seals.denom[i], col = "red")
		lines(plot.time, pengs.y[,i]/pengs.denom[i], col = "blue")
		lines(plot.time, whales.y[,i]/whales.denom[i], col = "green")
		lines(plot.time, fish.y[,i]/fish.denom[i], lty = 4)
		box()
		axis(1,cex.axis=0.8)
		axis(2,cex.axis=0.8)
		if(!is.null(ss.object$setup$reassessment.times)){
      abline(v=vlines,lty=3)
    }
		title(main=paste("SSMU ", i, sep = ""), outer = FALSE, cex.main = 0.9, line = 0.5)
		panel.count <- panel.count + 1
		if(panel.count > (page.layout[1] * page.layout[2])) {
			mtext(paste(title.prefix," -- krill (black), seals (red), pengs (blue), whales (green), fish (dash)",sep=""),
				outer = TRUE, cex = 0.75, line = 1)
		}
	}
	mtext(paste(title.prefix," -- krill (black), seals (red), pengs (blue), whales (green), fish (dash)",sep=""), outer = TRUE, cex =
		0.75, line = 1)
  par(origpar)
}
