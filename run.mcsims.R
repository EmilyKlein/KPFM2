# assumes a few variables have been set in the global environment by main()
###############################################################################################################
run.mcsims <- function(fishing.options, input.objects, ntrials, nyears, start.fishing.yr, stop.fishing.yr, 
		actual.gamma, sd.krill.Rdev, parameter.path, output.file.path) {
		
	for (fishing.option in fishing.options) {	
		
		print("===========================================")
		print(paste("FISHING OPTION: ", fishing.option))
		
		gamma.fractions <- get.gamma.fractions(fishing.option)
		
		for(b in seq(input.objects)){ 
			#loop over parameterizations
			
			## read in parameter set
			#load(path.to.saved.parameters[b])
			eval(parse(text=
				paste("load(file='",parameter.path, input.objects[b],".rdata')",sep="")
			))

			p1 <- eval(parse(text=input.objects[b]))
			if(!is.null(p1$new.params)) p1 <- p1$new.params
			
			# Additon to update the threshold densities - ESK 14 Feb 2018
			p1$THRESHOLD.DENSITY <- rep(15, 15)
			
			##
			## generate simulation that is used for the saved state
			## nyears=38 here given timeline of calendar (1970-2007)
			
			tt.ss <- ssmu.ss(p1,nyears=38,random.Rkrill=FALSE,single.sim=FALSE)
			
			##
			## generate the parameter set for the projections
			## assume that the last environmental index will apply for the projection period
			## assume that the last bathtub abundances will apply for the projection period
			p2 <- p1
			p2$ENV.INDEX <- p1$ENV.INDEX[length(p1$ENV.INDEX)]
			tt.tubs <- numeric(p2$NTUBS)
			for(i in 1:length(tt.tubs)){
				tt.tubs[i]<-p1$BATHTUB.ABUNDANCE[[i]][length(p1$BATHTUB.ABUNDANCE[[i]])]
			}
			tt.tubs <- data.frame(t(tt.tubs))
			p2$BATHTUB.ABUNDANCE<-bt.abund.var(nyears=nyears,ntrials=ntrials,btmeans=tt.tubs)
			
			# p2$HISTORICAL.CATCH<-hstry
			
############ For CM 51-07 re-analysis, needed to update a bunch of stuff - ESK, Sept 2016  ############
			
			# Update catch setup given new seasonality of fishery - ESK 09/21/2016	
			# s1 <- c(0.7852,0.4421,0.4466,0.7591,0.3015,0.3706,0.8346,0.7322,0.8575,0.4819,0.9631,0.5266,0.7429,0.0713,0.0114)
			# s2 <- c(0.2148,0.5579,0.5534,0.2409,0.6985,0.6294,0.1654,0.2678,0.1425,0.5181,0.0369,0.4734,0.2571,0.9287,0.9886)
			# catch.setup <- matrix(0, nrow = 2, ncol = 15)
			# catch.setup[1,] <- s1
			# catch.setup[2,] <- s2
			# 
			# p2$CATCH.SETUP <- catch.setup
	

			#################################################
			####### RUN THE MONTE-CARLO SIMULATIONS #########
			
			if (fishing.option==0) {
				## sim with no fishing
				tmpfile <- paste(output.file.path,input.objects[b],".0.0.mc",sep="")

				if (OVERWRITE==TRUE | file.exists(tmpfile)==FALSE) {
					cat("Running gamma = 0",fill=TRUE)
					tmp <- paste(input.objects[b],".",0,".",0,".","mc",sep="")	
					assign(tmp, ssmu.mc(p2, saved.state=tt.ss, safe.mode=FALSE, ntrials=ntrials, nyears=nyears, fishing.option=NULL, sd.krill.Rdev=sd.krill.Rdev, plot.now=FALSE))
					save(list=tmp, file=tmpfile)
					rm(tmp)
				}

				
			} else {
				## fishing occurring over the agreed range of fractions of gamma
				
				#now set up the vectors of object and file names for saving the results of each simulation
				#naming convention: (path/)paramprefix.fishingoption.ggammafractions.mc
				mc.objects <- paste(input.objects[b],".",fishing.option,".g",gamma.fractions,".","mc",sep="")
				mc.files <- paste(output.file.path,mc.objects,sep="")
				
				if (OVERWRITE==FALSE) {
					aa <- which(file.exists(mc.files))	
					if (any(aa)) {
						mc.objects <- mc.objects[-aa]
						mc.files <- mc.files[-aa]
					}
					if (length(mc.files)==0) print("All files already processed for these fishing options>0")
				}			
				#remove objects and files that already exist if they don't need processing

				print(paste(length(mc.files),"FILES TO PROCESS: "))
				print("==========================================")
				print(mc.files)

				if (!length(mc.files)==0) { #if there's anything to process ...
					for(k in seq(mc.objects)) {#seq(gamma.fractions)){
						#gamma.fraction <- gamma.fractions[k]
						pos <- regexpr("g.*.mc", mc.objects[k])
						gamma.fraction <- as.numeric(substr(mc.objects[k],pos+1,pos+attr(pos,"match.length")-4))
						#get gamma.fraction from the filename ... replacement for when not processing all files
								
						cat("Running gamma = ",paste(actual.gamma,"*", gamma.fraction),fill=TRUE)
						
						########## this conditional relating to option 7 inserted by JM 12/1/12	
						# if (fishing.option == 7) {		
						# 	#For fishing option CM 57-02 also need other catch proportions conditional on ym:
						# 	if (gamma.fraction<0.1) {
						# 		p2$HISTORICAL.CATCH <- hstry
						# 	} 
						# 	if (gamma.fraction==0.1) {
						# 		p2$HISTORICAL.CATCH <- c(0.001,0.001,0.162,0.029,0.027,0.040,0.017,0.000,0.005,0.342,0.010,0.000,0.002,0.105,0.259)
						# 	} 
						# 	if (gamma.fraction==0.11) {
						# 		p2$HISTORICAL.CATCH <- c(0.001,0.001,0.147,0.026,0.025,0.037,0.015,0.000,0.005,0.354,0.011,0.000,0.002,0.109,0.268)
						# 	}		
						# 	# see SIH's excel document for CM 51-02
						# 	assign(mc.objects[k], ssmu.mc(p2, saved.state=tt.ss, safe.mode=FALSE, ntrials=ntrials, nyears=nyears, fishing.option=1, start.fishing.yr=start.fishing.yr, stop.fishing.yr=stop.fishing.yr, actual.gamma=actual.gamma*gamma.fraction, sd.krill.Rdev=sd.krill.Rdev,plot.now=FALSE))
						# 	
						# } else {
							assign(mc.objects[k], ssmu.mc(p2, saved.state=tt.ss, safe.mode=FALSE, ntrials=ntrials, nyears=nyears, fishing.option=fishing.option, start.fishing.yr=start.fishing.yr, stop.fishing.yr=stop.fishing.yr, actual.gamma=actual.gamma*gamma.fraction, sd.krill.Rdev=sd.krill.Rdev,plot.now=FALSE))
						# }
						
						save(list=mc.objects[k],file=mc.files[k])
					}	
				} else {
					print("nothing to process")
				}	
				
			}

			####### END OF MONTE-CARLO SIMS ##############
			# rm(mc.objects, mc.files)
			
		} # parameter loop
		gc()
	} # end of fishing options loop
}	

