plot.mc.C<-function(mc.object, quants = c(0.2, 0.5, 0.8), plot.trials = TRUE, annual.plots = TRUE, connector = "sum", page.layout = c(4, 4), Cylimits = c(0, 3), color.tags = NULL)
{
	# auxiliary function to plot abundances from Monte Carlo trials
	# George Watters
	# code last edited 19 July 2006
	#
	if(!is.null(mc.object$color.tags)&&max(mc.object$color.tags>15)){
    stop("FAULT -- software not designed to deal with plotting more than 15 colors in a single panel.\nIf you're sure you want to do this we can easily edit the color table.")
  }
  if(!is.null(quants)&&length(quants) > 3){stop("FAULT: Sorry, you can only plot 3 quantiles.")}
  #
  OPTION6 <- FALSE
  if(!is.null(mc.object$call$fishing.option) && mc.object$call$fishing.option==6){OPTION6<-TRUE}
  #
	ntrials <- mc.object$setup$ntrials
  nssmus <- mc.object$setup$nssmus
  nyears <- mc.object$setup$nyears
  nseasons <- mc.object$setup$nseasons
  ntimes <- mc.object$setup$ntimes
  #
  # get the desired data as determined by the arguments annual.plots and connector
  # then standardize these data as appropriate
  #
  if(annual.plots){
    season.vector <- rep(1:nseasons, length.out = ntimes)
    year.vector <- rep(1:(ntimes/nseasons),each = nseasons)
    time.label <- "year"
    if(is.character(connector)){
      plot.time <- unique(year.vector)
      tt.data2 <- array(0,dim=c(length(unique(year.vector)),nssmus,ntrials))
      for(j in 1:ntrials){
        for(i in 1:nssmus){
          tt.denom <- as.vector(tapply(mc.object$fishery$allocation[,i,j],list(year.vector),sum))
          tt.y <- as.vector(tapply(mc.object$fishery$catch[,i,j],list(year.vector),sum))
          tt.data2[,i,j] <- tt.y/tt.denom
        }
      }
      title.prefix <- "sumC/sumAC"
    }
    if(is.numeric(connector)){
      # first check that the connector is a feasible season
      if(connector > nseasons){stop("FAULT: connector season > nseasons")}
      keepers <- (season.vector == connector)
      plot.time <- year.vector[keepers]
      tt.data2 <- array(0,dim=c(length(unique(year.vector)),nssmus,ntrials))
      for(j in 1:ntrials){
        for(i in 1:nssmus){
          tt.denom <- mc.object$fishery$allocation[(1:ntimes)[keepers],i,j]
          tt.y <- mc.object$fishery$catch[(1:ntimes)[keepers],i,j]
          tt.data2[,i,j] <- tt.y/tt.denom
        }
      }
      title.prefix <- paste("C",connector,"/AC",connector,sep="")
    }
  }
  else {
    plot.time <- 1:ntimes
    tt.data2 <- array(0,dim=c(ntimes,nssmus,ntrials))
    for(j in 1:ntrials){
      tt.denom <- mc.object$fishery$allocation[,,j]
      tt.y <- mc.object$fishery$catch[,,j]
      tt.data2[,,j] <- tt.y/tt.denom
    }
    title.prefix <- "C/AC"
    time.label<-"season"
  }
  tt.data2[is.nan(tt.data2)]<-NA
  #
  # now compute quantiles
  if(is.null(quants)){
    quants <- rep(NA, 3)
    plot.trials <- TRUE
  }
	if(!is.na(quants[2])) {
	  ttmed <- apply(tt.data2, 2, FUN = function(x, prob)
	  {
		  apply(x, 1, quantile, probs = prob, na.rm = T)
	  }
	  , prob = quants[2])
	}
	if(!is.na(quants[1])) {
		ttlow <- apply(tt.data2, 2, FUN = function(x, prob)
		{
			apply(x, 1, quantile, probs = prob, na.rm = T)
		}
		, prob = quants[1])
	}
	if(!is.na(quants[3])) {
		tthigh <- apply(tt.data2, 2, FUN = function(x, prob)
		{
			apply(x, 1, quantile, probs = prob, na.rm = T)
		}
		, prob = quants[3])
	}
	title.suffix <- paste("quantiles = ", deparse(quants), sep="")
	title.string <- paste(title.prefix, title.suffix, sep = " -- ")
	red.width <- ifelse(plot.trials,2,1)
	#
  # set up the color table
  # now actually turn the color.tags into colors that are interpretable by the plot functions
  # black, blue, green, yellow, magenta, orange, cyan, lightgoldenrod, blueviolet, springgreen, gray47, aquamarine3, orange4, purple, yellow4
  if(!is.null(mc.object$color.tags)){
    tt.colors <- colors()[c(24,26,254,652,450,498,68,410,31,610,200,11,502,547,656)]
	  tt.colors <- tt.colors[match(mc.object$color.tags,1:15)]
	}
	else{
    tt.colors <- rep("black", ntrials)
  }
  #
  # now do the plotting
  windows()
	origpar <- par(no.readonly=TRUE)
	#par(oma = c(0, 0, 2, 0), mfrow = page.layout)
	par(oma = c(4, 2, 4, 3), mar=c(4,4,1,0)+0.1, mgp=c(2,0.75,0), xpd=FALSE, mfrow = page.layout)
	panel.count <- 1
	left.col.panels <- seq(from=1,to=page.layout[1]*page.layout[2],by=page.layout[2])
	bottom.panels <- (1:(page.layout[1]*page.layout[2]))[max(left.col.panels):((page.layout[1]*page.layout[2]))]
	for(i in 1:nssmus) {
		if(panel.count > (page.layout[1] * page.layout[2])) {
			panel.count <- 1
			par(origpar)
			windows()
			#par(oma = c(0, 0, 2, 0), mfrow = page.layout)
			par(oma = c(4, 2, 4, 3), mar=c(4,4,1,0)+0.1, mgp=c(2,0.75,0), xpd=FALSE, mfrow = page.layout)
		}
    if(is.element(panel.count,left.col.panels)){ylabel<-"relative catch"}else{ylabel<-""}
  	if(is.element(panel.count,bottom.panels)){xlabel<-time.label}else{xlabel<-""}
    if(!all(is.na(tt.data2[, i, 1]))) {
			plot(plot.time, tt.data2[, i, 1], type = "n", ylim = Cylimits, ylab = ylabel,
				xlab = xlabel,axes=FALSE)
      box()
      axis(1, cex.axis=0.8)
      axis(2, cex.axis=0.8)
      if(plot.trials){
			  for(j in 1:ntrials) {
          lines(plot.time, tt.data2[, i, j], col = tt.colors[j])
          if(OPTION6){points(plot.time, tt.data2[, i, j], col = tt.colors[j],pch=16,cex=0.75)}
			  }
			}
			if(!is.na(quants[2])){
			  lines(plot.time, ttmed[, i], col = "red", lwd = red.width, lty = 1)
			  if(OPTION6){points(plot.time, ttmed[, i], col = "red", pch=16, cex=0.75)}
      }
      if(!is.na(quants[1])) {
				lines(plot.time, ttlow[, i], col = "red", lwd = red.width, lty = 2)
				if(OPTION6){points(plot.time, ttlow[, i], col = "red", pch=16, cex=0.75)}
			}
			if(!is.na(quants[3])) {
				lines(plot.time, tthigh[, i], col = "red", lwd = red.width, lty = 2)
				if(OPTION6){points(plot.time, tthigh[, i], col = "red", pch=16, cex=0.75)}
			}
		}
		else {
			plot(range(plot.time), Cylimits, type = "n", ylab = ylabel, xlab = xlabel,axes=FALSE)
			box()
      axis(1, cex.axis=0.8)
      axis(2, cex.axis=0.8)
		}
		title(main=paste("SSMU ", i, sep = ""), line = 0.5, outer = FALSE, cex.main = 0.9)
		panel.count <- panel.count + 1
		if(panel.count > (page.layout[1] * page.layout[2])) {
			mtext(title.string, line = 1, cex = 0.75, outer = TRUE)
		}
	}
	mtext(title.string, line = 1, cex = 0.75, outer = TRUE)
}
