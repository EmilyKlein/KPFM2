assess.regional.krill <- function(fishing.options, input.objects, parameter.path, output.file.path) {

# Script to compute REGIONAL krill performance measures
# assumes you've already run main() to run simulations and store files
#
# George Watters last edited 15 April 2009

# WHAT IT DID:
#(1) aggregate krill abundances by region, merge and save as .merge.regkrill.mc file
#(2) 'scale to 2007 abundances' and save as .merge.regkrill.mc.pms file 
#(3) 'scale to no-fishing' and save as .merge.regkrill.rel file
#(4) then 'calculate performance measures at the regional scale' and save as .merge.regkrill.rel.pms
	
#JM changes to 1/2/12
#   - disabled (2) - don't need it?
#   - changed objects from (mc.objects.sm, mc.objects) to (mc.0.object, mc.objects)
	
print("==============================")
print("RUNNING REGIONAL RISK ASSESSMENT FOR KRILL ...")
	
fishing.options <- setdiff(fishing.options,0)
	
for(b in seq(input.objects)){
    for(fishing.option in fishing.options){
		gamma.fractions <- get.gamma.fractions(fishing.option)
        ##
        ## now set up the vectors of object and file names for saving the results of each simulation
        ## naming convention: (path/)paramprefix.fishingoption.ggammafractions.mc
        mc.objects <- paste(input.objects[b],".",fishing.option,".g",gamma.fractions,".","mc",sep="")
        mc.0.object <- paste(input.objects[b],".",0,".",0,".","mc",sep="")
        mc.files <- paste(output.file.path,mc.objects,sep="")
		
        ##
        ##
#        #################################################
#        ####### SIMS SCALED TO 2007 ABUNDANCES ##########
#        ## read mc output and set up new object and file names
#        for(i in mc.files){
#            load(i)
#        }
#        ## now take the mc objects and chop out stuff that is not used (to save space)
#        ## also sum up krill abundances by region (4 regions)
#        ## region 1 = Subarea 48.1 = SSMUs 1-8
#        ## region 2 = Subarea 48.2 = SSMUs 9-12
#        ## region 3 = Subarea 48.3 = SSMUs 13-15
#        for(i in mc.objects){
#            assign(i, calc.regional.krill.mc(eval(parse(text=i))))
#        }
#        ##
#        ## naming convention: (path/)paramprefix.fishingoption.merge.regkrill.mc
#        merge.object <- paste(input.objects[b],".",fishing.option,".merge.regkrill.mc",sep="")
#        merge.file <- paste(output.file.path,merge.object,sep="")
#        ##
#        ## now merge using merge.mc()
#        ## note that merge is across gamma.fractions
#        for(i in 2:length(mc.objects.sm)){
#            if(i==2) assign(merge.object, merge.regional.krill.mc(eval(parse(text=mc.objects.sm[1])),eval(parse(text=mc.objects.sm[2])),ssds=FALSE))
#            if(i>2) assign(merge.object, merge.regional.krill.mc(eval(parse(text=merge.object)),eval(parse(text=mc.objects.sm[i])),ssds=FALSE))
#        }
#        save(list=merge.object,file=merge.file)
#        ##
#        ## now compute krill PMs at the regional scale
#        pm.object <- paste(input.objects[b],".",fishing.option,".merge.regkrill.mc.pms",sep="")
#        pm.file <- paste(output.file.path,pm.object,sep="")
#        assign(pm.object, calc.pms.regional.krill(eval(parse(text=merge.object))))
#        save(list=pm.object,file=pm.file)
#        rm(list=pm.object)
#        rm(list=merge.object)
#        ##
#        ##
#        ##
        #################################################
        ####### SIMULATIONS SCALED TO NO FISHING ########
        ##
        ## naming convention: (path/)paramprefix.fishingoption.ggammafractions.regkrill.rel
		

		
        rel.objects <- gsub(".mc",".regkrill.rel",mc.objects)
        rel.files <- paste(output.file.path,rel.objects,sep="")
        ## naming convention: (path/)paramprefix.fishingoption.merge.regkrill.rel
        merge.object <- paste(input.objects[b],".",fishing.option,".merge.regkrill.rel",sep="")
        merge.file <- paste(output.file.path,merge.object,sep="")
        ##
        ## now do the scaling using the function relative.mc() and merging using merge.mc()
        ## note that the scaling is relative to time-specific abundances from the "no fishing" simulation
        ## note that the merge is across gamma.fractions
        		
		if (OVERWRITE==TRUE | file.exists(merge.file)==FALSE) {
			
			eval(parse(text=paste("load('",output.file.path,mc.0.object,"')",sep="")))	
			assign(mc.0.object, calc.regional.krill.mc(eval(parse(text=mc.0.object))))
			#load and regionalise the reference object
			
			for(i in 1:length(rel.objects)){
				
				load(mc.files[i])			
				assign(mc.objects[i], calc.regional.krill.mc(eval(parse(text=mc.objects[i]))))
				#make regional mc.objects from mc.objects
				
				assign(rel.objects[i], relative.regional.krill.mc(eval(parse(text=mc.objects[i])),eval(parse(text=mc.0.object))))
				##save(list=rel.objects[i],file=rel.files[i])
				rm(list=mc.objects[i])
				if(i==2) {	
					assign(merge.object, merge.regional.krill.mc(eval(parse(text=rel.objects[1])),eval(parse(text=rel.objects[2])),ssds=FALSE))
					rm(list=c(rel.objects[1],rel.objects[2]))
				}
				if(i>2) {
					assign(merge.object, merge.regional.krill.mc(eval(parse(text=merge.object)),eval(parse(text=rel.objects[i])),ssds=FALSE))
					rm(list=rel.objects[i])  
				}
				
			}
			save(list=merge.object,file=merge.file)
		} else {
			load(merge.file)
		}

        ## now compute krill PMs at the regional scale	
        pm.object <- paste(input.objects[b],".",fishing.option,".merge.regkrill.rel.pms",sep="")
        pm.file <- paste(output.file.path,pm.object,sep="")
		if ((OVERWRITE==TRUE) | file.exists(pm.file)==FALSE) {
			assign(pm.object, calc.pms.regional.krill(eval(parse(text=merge.object))))
			save(list=pm.object,file=pm.file)
		}
  }
}

}
