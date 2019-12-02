plot.ss.C<-function(ss.object, annual.plots=TRUE, connector="sum", page.layout = c(4, 4), Cylimits = c(0, 3), point.size = 0.001)
{
	# auxiliary function to plot catch results from single simulation of predator-prey model
	# George Watters
	# code last edited 14 June 2006
	# 
	# fishery is a sublist in ss.object: fishery=list(allocation=earmarks, catch=catch)
	#
if(annual.plots){
    season.vector <- rep(1:ss.object$setup$nseasons, length.out = ss.object$setup$ntimes)
    year.vector <- rep(1:(ss.object$setup$ntimes/ss.object$setup$nseasons),each=ss.object$setup$nseasons)
    time.label<-"year"
    if(is.character(connector)){
      plot.time <- unique(year.vector)
      fishery.denom <- matrix(0,nrow=length(unique(year.vector)),ncol=ss.object$setup$nssmus)
      fishery.y <- fishery.denom
      for(i in 1:ss.object$setup$nssmus){
        fishery.denom[,i] <- as.vector(tapply(ss.object$fishery$allocation[,i], list(year.vector), sum))
        fishery.y[,i] <- as.vector(tapply(ss.object$fishery$catch[,i], list(year.vector), sum))
      }
      title.prefix <- "sumC/sumAC"
    }
    if(is.numeric(connector)){
      # first check that the connector is a feasible season
      if(connector > ss.object$setup$nseasons){stop("FAULT: connector season > nseasons")}
      keepers <- (season.vector == connector)
      #
      plot.time <- year.vector[keepers]
      fishery.denom <- ss.object$fishery$allocation[keepers, ]
      fishery.y <- ss.object$fishery$catch[keepers, ]
      #
      title.prefix <- paste("C",connector,"/AC",connector,sep="")
    }
    if(!is.null(ss.object$setup$reassessment.times)){
      vlines <- ss.object$setup$reassessment.times/ss.object$setup$nseasons
    }
  }
  else {
    plot.time <- 1:ss.object$setup$ntimes
    keepers <- rep(TRUE,ss.object$setup$ntimes)
    #
    fishery.denom <- ss.object$fishery$allocation
    fishery.y <- ss.object$fishery$catch
    #
    title.prefix <- "C/AC"
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
		if(is.element(panel.count,left.col.panels)){ylabel<-"relative catch"}else{ylabel<-""}
  	if(is.element(panel.count,bottom.panels)){xlabel<-time.label}else{xlabel<-""}
		if(!all(is.na(fishery.y[,i]/fishery.denom[,i]))) {
		  plot(plot.time, fishery.y[,i]/fishery.denom[,i], type = "l", ylim = Cylimits, ylab =
				ylabel, xlab = xlabel,axes=FALSE)
			points(plot.time, fishery.y[,i]/fishery.denom[,i], pch = 16, cex = point.size)
		}
		else {
			plot(c(min(plot.time),max(plot.time)), Cylimits, type = "n", ylab = ylabel, xlab = xlabel,axes=FALSE)
		}
		box()
		axis(1,cex.axis=0.8)
		axis(2,cex.axis=0.8)
		if(!is.null(ss.object$setup$reassessment.times)){
      abline(v=vlines,lty=3)
    }
		title(main=paste("SSMU ", i, sep = ""), outer = FALSE, line = 0.5, cex.main = 0.9)
		panel.count <- panel.count + 1
		if(panel.count > (page.layout[1] * page.layout[2])) {
			mtext(title.prefix, line = 1, cex = 0.75, outer = TRUE)
		}
	}
	mtext(title.prefix, line = 1, cex = 0.75, outer = TRUE)
}
