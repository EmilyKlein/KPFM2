assess.krill.fishery <- function(fishing.options, input.objects, dens.thresholds, parameter.path, output.file.path) {

# compute krill and fishery performance measures
#
# George Watters
# last edited 16 April 2009

# (1) merges *.mc objects and saves into .merge.mc file
# (2) further analysis creates *merge.rel.*.pms, *merge.mc.*.pms files
	
# assumes you've already run main() to run simulations and store files
# NB *.pms objects means 'performance measures'
	
# JM changes up to 31/1/12:
#    - remove some used objects as we go along to try and avoid memory probs 
#    - changed objects from (mc.objects.sm, mc.objects) to (mc.0.object, mc.objects)
#    - seperated .mc merging process from subsequent analysis
#	- code to prevent reanalysis where files exist, if OVERWRITE==FALSE

print("==============================")
print("RUNNING SSMU RISK ASSESSMENT FOR KRILL AND FISHERY ...")	
	
fishing.options <- setdiff(fishing.options,0)	
	
print("Merging .mc objects ...")
print(")------------------------------------")

for(b in seq(input.objects)){
    ## make the merged mc objects (didn't do this in earlier scripts but need it now)
	print(paste("Input object:", input.objects[b]))
    for(fishing.option in fishing.options){

		print(paste("Fishing option:", fishing.option))

		gamma.fractions <- get.gamma.fractions(fishing.option)


		#==========================================================

		mc.objects <- paste(input.objects[b],".",fishing.option,".g",gamma.fractions,".","mc",sep="")
		mc.0.object <- paste(input.objects[b],".",0,".",0,".","mc",sep="")
		mc.files <- paste(output.file.path,mc.objects,sep="")
		## naming convention: (path/)inputobject.fishingoption.ggammafractions.mc

		## naming convention: (path/)inputobject.fishingoption.merge.mc
		merge.object <- paste(input.objects[b],".",fishing.option,".merge.","mc",sep="")
		merge.file <- paste(output.file.path,merge.object,sep="")
		##

		#===============================================================

		if (OVERWRITE==TRUE | file.exists(merge.file)==FALSE) {
			eval(parse(text=paste("load('",output.file.path,mc.0.object,"')",sep="")))

			for(i in 1:length(mc.objects)){
				load(mc.files[i])
				if(i==2) {
					assign(merge.object, merge.mc(eval(parse(text=mc.objects[1])),eval(parse(text=mc.objects[2])),ssds=FALSE))
					eval(parse(text=paste("rm(",mc.objects[1],",",mc.objects[2],")",sep="")))
				}
				if(i>2) {
					print(paste("Merging :", mc.objects[i]))
					assign(merge.object, merge.mc(eval(parse(text=merge.object)),eval(parse(text=mc.objects[i])),ssds=FALSE))
					eval(parse(text=paste("rm(",mc.objects[i],")",sep="")))
				}
			}
			save(list=merge.object, file=paste(merge.file,sep=""))
			rm(list=merge.object)
			rm(list=mc.0.object)
		}

		#if necessary merge .mc objects together and save as merge.mc file

	} #end of fishing options
} #end of input.objects

########################################################################################
print("================================================")
print("Now other analysis ...")
print("================================================")
		## now do what is necessary to compute PMs etc	
		
	for(b in seq(input.objects)){

		print(paste("Input object:", input.objects[b]))	
		
		## read in parameter set
		#load(path.to.saved.parameters[b])
		eval(parse(text=
				paste("load('",parameter.path, input.objects[b],".rdata')",sep="")
				))
		
		p1 <- eval(parse(text=input.objects[b]))
		if(!is.null(p1$new.params)) p1 <- p1$new.params
		##
		## now load the files (*.merge.mc and *.merge.rel) and get the parameter set into the call
		## object so that calc.pms.nopred() can get the necessary setup information from it
		## NOTE -- these parameter lists do NOT include the bathtub setup and ENV.INDEX setup
#		tt.mc<-vector(mode="list",length=length(fishing.options))
#		tt.rel<-vector(mode="list",length=length(fishing.options))

		for (j in seq_along(fishing.options)){

			print(paste("Fishing option:", fishing.options[j]))	
			
			load(paste(output.file.path,input.objects[b],".",fishing.options[j],".merge.mc",sep=""))
			tt.mc <- eval(parse(text=paste(input.objects[b],".",fishing.options[j],".merge.mc",sep="")))
			tt.mc$call$param.list<-p1
			rm(list=paste(input.objects[b],".",fishing.options[j],".merge.mc",sep=""))
			load(paste(output.file.path,input.objects[b],".",fishing.options[j],".merge.rel",sep=""))
			tt.rel <- eval(parse(text=paste(input.objects[b],".",fishing.options[j],".merge.rel",sep="")))
			tt.rel$call$param.list<-p1
			rm(list=paste(input.objects[b],".",fishing.options[j],".merge.rel",sep=""))

		## now compute the PMs -- note the "merge" was originally over levels of gamma

			## now set up the vectors of object and file names for saving the PMs
			## naming convention: (path/)paramprefix.fishingoption.merge.mc.densitythreshold.pms
			mc.pm.objects <- paste(input.objects[b],".",fishing.options[j],".merge.mc.",dens.thresholds,".pms",sep="")
			mc.pm.files <- paste(output.file.path,mc.pm.objects,sep="")
			## naming convention: (path/)paramprefix.fishingoption.merge.rel.densitythreshold.pms
			rel.pm.objects <- paste(input.objects[b],".",fishing.options[j],".merge.rel.",dens.thresholds,".pms",sep="")
			rel.pm.files <- paste(output.file.path,rel.pm.objects,sep="")

			for(i in seq_along(dens.thresholds)){
				
				cat("Running fishing option =",fishing.options[j],"& density threshold =",paste(dens.thresholds[i], "g/m^2", sep=" "),fill=TRUE)
				if (OVERWRITE==TRUE | file.exists(mc.pm.files[i])==FALSE) {
				  assign(mc.pm.objects[i], calc.pms.krill.fishery(tt.mc,threshold.density=dens.thresholds[i]))
				  # assign(mc.pm.objects[i], calc.pms.krill.fishery.esk(tt.mc,threshold.density=NULL))
					save(list=mc.pm.objects[i],file=mc.pm.files[i])
				}

				if (OVERWRITE==TRUE | file.exists(rel.pm.files[i])==FALSE) {
				  assign(rel.pm.objects[i], calc.pms.krill.fishery(tt.rel,threshold.density=dens.thresholds[i]))
				  # assign(rel.pm.objects[i], calc.pms.krill.fishery.esk(tt.rel,threshold.density=NULL))
					save(list=rel.pm.objects[i],file=rel.pm.files[i])
				}
			}

			#rm(list=mc.pm.objects)
		  #rm(list=rel.pm.objects)
		}
		
	} #end parameter input loop

} #end function
