assess.risk.predators <- function(fishing.options, input.objects, risk.reference.levels, parameter.path, output.file.path){
	
# assess risks to predators and save results to file
# assumes you've already run main() to run simulations and store files
	
#(1) creates .mc objects relative to one another and saves as .rel files
#(2) merges the .rel objects and saves as .merge.rel file
#(3) 'calculate risk metrics for predators' and saves as .risk files
	
#JM changes:
#29/1/12 remove some used objects as we go along to try and avoid memory probs 
#31/1/12 changed objects from (mc.objects.sm, mc.objects) to (mc.ref.object, mc.objects) 
	
	print("==============================")
	print("RUNNING RISK ASSESSMENT FOR PREDATORS ...")
	
	fishing.options <- setdiff(fishing.options,0)
	#getting results relative to fishing.option=0 so won't be processing that
	
	for(b in seq(input.objects)){
		
		print(paste("Running : ",input.objects[b],sep=""))
		print("---------------------------------------")
		for (fishing.option in fishing.options) {	
			gamma.fractions <- get.gamma.fractions(fishing.option)
			
#==========================================================
			
			mc.rel.objects <- paste(input.objects[b],".",fishing.option,".g",gamma.fractions,".","mc",sep="")
			mc.ref.object <- paste(input.objects[b],".",0,".",0,".","mc",sep="")
			mc.rel.files <- paste(output.file.path,mc.rel.objects,sep="")
			
			## naming convention: (path/)inputobject.fishingoption.ggammafractions.rel		
			rel.objects <- gsub("mc","rel",mc.rel.objects)
			rel.files <- paste(output.file.path,rel.objects,sep="")
			
			## naming convention: (path/)inputobject.fishingoption.merge.rel
			merge.object <- paste(input.objects[b],".",fishing.option,".merge.","rel",sep="")
			merge.file <- paste(output.file.path,merge.object,sep="")
			##
			## now do the scaling using the function relative.mc() and merging using merge.mc()
			## note that the scaling is relative to time-specific abundances from the "no fishing" simulation
			## note that the merge is across gamma.fractions
			
#==========================================================
			
			eval(parse(text=paste("load('",output.file.path,mc.ref.object,"')",sep="")))
			for(i in 1:length(rel.objects)){
				if (OVERWRITE==TRUE | file.exists(rel.files[i])==FALSE) {			
					
					load(mc.rel.files[i])
					assign(rel.objects[i], relative.mc(eval(parse(text=mc.rel.objects[i])),eval(parse(text=mc.ref.object))))
					eval(parse(text=paste("rm(",mc.rel.objects[i],")",sep="")))
					save(list=rel.objects[i],file=rel.files[i])
					
				} 
			}
			
			#load or create .rel objects from .mc objects
#===========================================================

			if (OVERWRITE==TRUE | file.exists(merge.file)==FALSE) {	
				for(i in 1:length(rel.objects)){
					load(rel.files[i])
					if(i==2) {
						assign(merge.object, merge.mc(eval(parse(text=rel.objects[1])),eval(parse(text=rel.objects[2])),ssds=FALSE))
						eval(parse(text=paste("rm(",rel.objects[1],",",rel.objects[2],")",sep="")))
					}
					if(i>2) {
						print(paste("Merging :", rel.objects[i]))
						assign(merge.object, merge.mc(eval(parse(text=merge.object)),eval(parse(text=rel.objects[i])),ssds=FALSE))
						eval(parse(text=paste("rm(",rel.objects[i],")",sep="")))
					}
				}
				save(list=merge.object, file=paste(merge.file,sep=""))
			}
			
			#merge .rel objects together and save
#====================================================

			if (exists(merge.object)==FALSE) {
				load(paste(merge.file,sep=""))
			}
			## calculate risk metrics for predators
			## naming convention: (path/)paramprefix.fishingoption.merge.rel.riskreferencelevels*100.risk
			risk.objects <- paste(merge.object,".",risk.reference.levels*100,".risk",sep="")
			risk.files <- paste(output.file.path,risk.objects,sep="")
			for(i in 1:length(risk.reference.levels)){
				assign(risk.objects[i], calc.risk.predators(eval(parse(text=merge.object)),depletion.fraction=risk.reference.levels[i],recovery.fraction=risk.reference.levels[i]))
				save(list=risk.objects[i],file=risk.files[i])
			}
#====================================================
			
			rm(list=merge.object)
			rm(list=risk.objects)
			rm(list=mc.ref.object)
			rm(merge.file, rel.files, risk.files)
			gc()
			
		} #end fishing.options loop
		print(paste("Completed : ",input.objects[b],sep=""))
	} #end input.objects loop
} #end function
