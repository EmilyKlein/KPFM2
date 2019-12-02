import.all.parameters <- function(file.string="C:/Users/emily/Desktop/SWFSC/FOOSA/SWFSC R/Foosa/data/nlt_new.txt", foraging.prefix="p")
 {
 # Last edited 21 June 2006
 # Jefferson Hinke
 #
 # Function to scan whole text file that contains all model parameters (.txt files only!)
 # Functionality depends of a fixed formating of the parameter file. 
 # DO NOT ALTER RELATIVE POSITIONS OF THE PARAMETER NAMES OR DATA IN THE TEXT FILE
 #
 # First, make sure the file.string is not a .csv file
 #
 SEP<-ifelse(regexpr("csv", file.string)[1] > 0, ",", "")
 if(SEP==","){
  stop(".csv files are not supported by parameter import function. Save your parameter set as a tab-delimited text file (.txt).")
 }
 #
 # Next, determine the dimensions of the parameter file
 # parameter order is assumed to be NO. of SSMUs, Bathtubs, Seasons, Timeseries Years, krill R age, initial season, and base fishing option
 #
 # SKIP is the number of lines to skip over to get to input data
 SKIP <- 2
 setup.params <- scan(file=file.string, nlines=1, skip=SKIP, sep=SEP, quiet=T)
 #
 # for later use, define the setup parameters and the names of the parameter groups here
 nssmu <- setup.params[1]
 ntub <- setup.params[2]
 nseason <- setup.params[3]
 timeseries.yrs <- setup.params[4]
 krill.Rage <- setup.params[5]
 init.season <- setup.params[6]
 base.option <- setup.params[7]
 tt.parameter.list<-list()
 # now read in the data for historical catch and SSMU areas
 SKIP<-SKIP+4 #UPDATE SKIP
 param.values <- matrix(scan(file = file.string, what="character", skip=SKIP, sep=SEP, nlines=nssmu+ntub, quiet=T), byrow=TRUE, nrow=nssmu+ntub)
 param.values<-param.values[,3:4]
 if(nssmu==1){
    ssmu.areas <- as.numeric(param.values[,1])
    historical.catch <- as.numeric(param.values[1,2])
  } else {
    ssmu.areas <- as.vector(as.numeric(param.values[, 1]))
    historical.catch <- as.vector(as.numeric(param.values[1:nssmu,2]))
  }
  tt.parameter.list[[1]] <- ssmu.areas
  tt.parameter.list[[2]] <- historical.catch
  names(tt.parameter.list)[[1]]<-"AREAS"
  names(tt.parameter.list)[[2]]<-"HISTORICAL.CATCH"
 # now read in the predator parameters using the setup parameters as dimensional guides
 # determine which predator is being read in
 # loop over predators and krill to import parameter names and values into lists
 for(k in 1:5){
    # set up skipping to go to line above group name, i.e. above WHALES, SEALS, PENGS, etc..
    SKIP <- 3+ntub+(k*(nssmu+4)) # funky indexing depends on fixed format of input text file
    Param.name <- scan(file=file.string, what="character", nlines=1, skip=SKIP, sep=SEP, quiet=T)
    param.names <- scan(file=file.string, what="character", nlines=1, skip=SKIP+1, sep=SEP, quiet=T)
    season.vector <- scan(file=file.string, what="character", nlines=1, skip=SKIP+2, sep=SEP, quiet=T)
    season.vector <- as.numeric(season.vector[-(1:2)]) # remove first two columns
    # Read in the parameter values
    # The following code was modified by JTH from original GMW code
    # The addition of bathtubs to the krill parameters (k=5) requires special indexing
    if(k!=5){
      param.values <- matrix(scan(file = file.string, what="character", skip=SKIP+3, sep=SEP, nlines=nssmu, quiet=T), byrow=TRUE, nrow=nssmu)
    } else {
      # required because of krill information in the bathtub spots
      param.values <- matrix(scan(file = file.string, what="character", skip=SKIP+3, sep=SEP, nlines=nssmu+ntub, quiet=T), byrow=TRUE, nrow=nssmu+ntub)
    }
    # Drop the first two columns
    param.values <- param.values[,3:(length(season.vector)+2)]
    # now simplify param.names to so that the foraging matrix is a single thing
    if(Param.name!="KRILL"){
       foraging.nomatch <- match(substring(param.names,first=1,last=nchar(foraging.prefix)),foraging.prefix)
       foraging.columns <- !is.na(foraging.nomatch)
       component.names <- c(unique(param.names[is.na(foraging.nomatch)]),"foraging.matrix")
    } else {
       component.names <- unique(param.names)
    }
    # now get some necessary dimensions and fill up the list
    # probably could do this without looping, but it's late (very late)
    # besides, speed isn't important here -- you don't need to do it very often
    ncomponents <- length(component.names)
    J <- ncomponents
    if(Param.name!="KRILL"){
      nareas <- sum(foraging.columns)/nseason
      J <- ncomponents-1
    }
    # Setup list names for KRILL parameters and PREDATOR parameters
    if(k==5){
 	  tt.list <- vector("list",nssmu+ntub)
 	  names(tt.list) <- c(paste("ssmu",1:nssmu,sep="."), paste("bt",1:ntub, sep="."))
     } else {
 	  tt.list <- vector("list", nssmu)
 	  names(tt.list) <- paste("ssmu",1:nssmu,sep=".")
    }
    #
    # FILL IN PARAMETER LISTS FOR MODELS WITH MORE THAN ONE SSMU 
    if(nssmu>1){
      AREAS<-ifelse(k==5, nssmu+ntub, nssmu)
      for(i in 1:AREAS) {
        tt.list[[i]] <- vector("list",ncomponents)
        names(tt.list[[i]]) <- component.names
        for(j in 1:J){
          if(Param.name!="KRILL"){
            if(j==2){
              tt.list[[i]][[j]] <- param.values[i,(param.names==component.names[j])]
              attributes(tt.list[[i]][[j]]) <- NULL
            } else {
              tt.list[[i]][[j]] <- as.numeric(param.values[i,(param.names==component.names[j])])
              attributes(tt.list[[i]][[j]]) <- NULL
            }
          } else {
            tt.list[[i]][[j]] <- as.numeric(param.values[i,(param.names==component.names[j])])
            attributes(tt.list[[i]][[j]]) <- NULL
          }
          if(Param.name!="KRILL"){
            tt.list[[i]][[ncomponents]] <- matrix(as.numeric(param.values[i,foraging.columns]),nrow=nseason,ncol=nareas,byrow=TRUE)
          }
        }
      } 
    } else {
      # IF ONLY ONE SSMU IS MODELED, USE THIS SECTION OT FILL PARAMETER LISTS
      tt.list[[1]] <- vector("list",ncomponents)
      names(tt.list[[1]]) <- component.names
      if(Param.name!="KRILL"){
        # LOOP FOR PREDATORS
        for(j in 1:J){
          if(j==2){
            tt.list[[1]][[j]] <- param.values[param.names==component.names[j]]
            attributes(tt.list[[1]][[j]]) <- NULL
          } else {
            tt.list[[1]][[j]] <- as.numeric(param.values[param.names==component.names[j]])
            attributes(tt.list[[1]][[j]]) <- NULL
          }
        }
      } else {
        # LOOP FOR KRILL BECAUSE OF ADDITIONAL BATHTUBS
        for(i in 1:(nssmu+ntub)){
          tt.list[[i]] <- vector("list", ncomponents)
          names(tt.list[[i]]) <- component.names
          for(j in 1:J){
            tt.list[[i]][[j]] <- as.numeric(param.values[i,][param.names==component.names[j]])
          }
          attributes(tt.list[[i]][[j]]) <- NULL
          }
       }
        if(Param.name!="KRILL"){
          tt.list[[1]][[ncomponents]] <- matrix(as.numeric(param.values[foraging.columns]),nrow=nseason,ncol=nareas,byrow=TRUE)
        }
    } 
    tt.parameter.list[[k+2]] <- tt.list
    names(tt.parameter.list)[[k+2]] <- Param.name
  }
  # Now read in the catch setup 
  # Due to flexible dimensions of the catch matrix, this code will scan for missing data, which will end the import.
  # UPDATE SKIP TO JUMP TO LINE WITH NAME "CATCH.SETUP"
  SKIP<-SKIP+nssmu+ntub+5
  param.values <- matrix(NA, ncol=nssmu, nrow=nssmu*nseason)
  tt.lines <- numeric(0)
  for(m in 1:(nssmu*nseason)){
    data <- scan(file=file.string, skip=SKIP+m, sep=SEP, quiet=T, nlines=1)
    # look for a blank data point - this signifies the end of the catch.setup parameter chuck
    if(is.na(data[1])){
      param.values <- param.values[1:(m-1),]
      break
    } else {
    param.values[m,] <- data[3:(nssmu+2)]
    # KEEP TRACK OF HOW MANY LINES ARE READ TO UPDATE SKIP FOR NEXT GRUOP OF PARAMETERS
    tt.lines <- m
    }
  }
  tt.parameter.list[[8]] <- param.values
  # now read in the FISHING OPTION 5 setup parameters
  # again, flexible size demands looking for break.
  SKIP<-SKIP+tt.lines+3
  param.values <- matrix(NA, ncol=7, nrow=nssmu+ntub) # make the matrix large enough hold all the info
  tt.lines <- numeric(0)
  for(m in 1:(nssmu+ntub)){ # should not have more areas to monitor than the number of areas! 
    data <- scan(file=file.string, what="character", skip=SKIP+m, sep=SEP, quiet=T, nlines=1)
    if(is.na(data[1])){
      param.values <- param.values[1:(m-1),]
      break
    } else {
    param.values[m,] <- data
    }
    # KEEP TRACK OF HOW MANY LINES ARE READ TO UPDATE SKIP FOR NEXT GROUP OF PARAMETERS
    tt.lines <- m
  }
  tt.option5.list<-list(8)
  names<-c("option5.areas", "monitored.spp", "monitoring.seasons", "npoints", "obs.density.dists", "obs.mult", "obs.sd")
  param.type <- c("n", "char", rep("n", 2), "char", rep("n", 2)) # need to ID the character and numeric data.
    if(tt.lines > 1){
      for(i in 1:length(names)){
        if(param.type[i]=="n"){
          tt.option5.list[[i]]<-as.numeric(param.values[,i]) # numeric paramters in all other positions
        } else {
          tt.option5.list[[i]]<-param.values[,i]  # character paramters
        }  
        names(tt.option5.list)[[i]]<-names[i]
      }
    } else {
      for(i in 1:length(names)){
        if(param.type[i]=="n"){
          tt.option5.list[[i]]<-as.numeric(param.values[i]) # numeric paramters in all other positions
      } else {
         tt.option5.list[[i]]<-param.values[i]  # character paramters
      }  
      names(tt.option5.list)[[i]]<-names[i]
    }
  }
  tt.option5.list[[8]]<-base.option
  names(tt.option5.list)[[8]] <- "base.option"
  tt.parameter.list[[9]]<- tt.option5.list
  # now read in the vector of threshold krill densities below which fishing will cease
  SKIP <- SKIP+tt.lines+3 # accounting based on skipping to THESHOLD.DENSITY name
  param.values <- scan(file=file.string, skip=SKIP+1, sep=SEP, nlines=1, quiet=T)
  tt.parameter.list[[10]] <- param.values
  # now read in the vector of available.fraction
  SKIP <- SKIP+4
  param.values <- scan(file=file.string, skip=SKIP+1, sep=SEP, nlines=1, quiet=T)
  tt.parameter.list[[11]] <- param.values
  # now read in the movement array! 
  SKIP <- SKIP+4
  data <- matrix(scan(file=file.string, what="character", nlines=nssmu+ntub, skip=SKIP+2, quiet=TRUE), byrow=TRUE, nrow=nssmu+ntub)
  param.values <- as.numeric(data[,-1]) # remove first column 
  v.array <- array(param.values, dim=c(nssmu+ntub, nssmu+ntub, nseason))
  tt.parameter.list[[12]] <- v.array
  #NOW READ IN THE COMPETITION MATRIX
  SKIP <- SKIP+nssmu+ntub+4
  param.values <- matrix(scan(file = file.string, what="character", skip=SKIP+1, sep=SEP, nlines=nssmu+ntub, quiet=T), byrow=TRUE, nrow=nssmu+ntub)
  param.values<-param.values[,-1] # remove first column
  param.values<-matrix(as.numeric(param.values), nrow=nssmu+ntub, ncol=5, byrow=F)  
  tt.parameter.list[[13]] <- param.values
  # now read in the time series of bathtub abundances and environmental forcing
  # this gets tricky due to the option of inputting a single value or a time series for the batchtub abundances
  SKIP <- SKIP+nssmu+ntub+3 # skip to line above "TIMESERIES" 
  param.names <- scan(file=file.string, what="character", sep=SEP, skip=SKIP, nlines=1, quiet=T)
  param.names <- param.names[-(1:2)] # remove first two fields
  n.keepers <- ntub+1 # this should only have data for the bathtubs and 1 environmental anomoly
  NLINE<-ifelse(timeseries.yrs==1, 1, timeseries.yrs*nseason)
  if(NLINE==1){
    param.values <- scan(file=file.string, sep=SEP, skip=SKIP+1, nlines=NLINE, quiet=T)
    param.values <- param.values[3:(3+n.keepers-1)]
  } else {
    param.values <- matrix(scan(file=file.string, sep=SEP, skip=SKIP+1, nlines=NLINE, quiet=T), byrow=TRUE, nrow=NLINE)
    param.values <- param.values[, 3:(3+n.keepers-1)]
  }
  bathtub.abundance <- list(ntub)
  if(NLINE==1){
    for(i in 1:ntub){
      bathtub.abundance[[i]] <- param.values[i]
      names(bathtub.abundance)[[i]] <- paste("tub", i, sep="")
    }
  } else {
    for(i in 1:ntub){
      bathtub.abundance[[i]] <- param.values[,i]
      names(bathtub.abundance)[[i]] <- paste("tub", i, sep="")
    }
  }
  if(NLINE==1){
     env.index <- param.values[ntub+1]
  } else {
    env.index <- param.values[,ntub+1] # env.index should be last column
  }
  # now add the rest of the data
  tt.parameter.list[[14]] <- bathtub.abundance
  tt.parameter.list[[15]] <- env.index
  tt.parameter.list[[16]] <- ntub
  tt.parameter.list[[17]] <- nseason
  tt.parameter.list[[18]] <- krill.Rage
  tt.parameter.list[[19]] <- init.season
  names(tt.parameter.list)[[8]] <- "CATCH.SETUP"
  names(tt.parameter.list)[[9]] <- "OPTION5.LIST"
  names(tt.parameter.list)[[10]] <- "THRESHOLD.DENSITY"
  names(tt.parameter.list)[[11]] <- "AVAILABLE.FRACTION"
  names(tt.parameter.list)[[12]] <- "V.ARRAY" 
  names(tt.parameter.list)[[13]] <- "COMPETITION.MATRIX"
  names(tt.parameter.list)[[14]] <- "BATHTUB.ABUNDANCE"
  names(tt.parameter.list)[[15]] <- "ENV.INDEX"
  names(tt.parameter.list)[[16]] <- "NTUBS"
  names(tt.parameter.list)[[17]] <- "NSEASONS"
  names(tt.parameter.list)[[18]] <- "KRILL.RAGE"
  names(tt.parameter.list)[[19]] <- "INIT.SEASON"
  print(paste("The setup says you have ", setup.params[1], " SSMU(s), ", setup.params[2], " BATHTUB(s), ", setup.params[4], " YEAR(s) of time series data, and ", setup.params[3], " SEASON(s) specified", sep=""))
  tt.parameter.list       
}
