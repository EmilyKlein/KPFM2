# KPFM2 ~ READ ME AND ANALYSIS GUIDE:

R model code for the Krill-Predator-Fishery Model (KPFM2), used to assess trade-offs between predators and a fishery both focused on a forage species (currently focused on krill in the Southern Ocean). 

This file contains information on the (1) data input files, (2) code for analysis, and (3) steps in analysis for the *basic* use of KPFM2. The analysis uses functions coded in [R] programming language, which is available for free download at https://www.r-project.org/ (NB: this research used RStudio, https://www.rstudio.com/, to access the R software). For further background and reasoning regarding the model and its use, the reader is referred to: 

> Watters GM et al. (2013) Decision-making for ecosystem-based management: evaluating options for a krill fishery with an ecosystem dynamics model. Ecol Appl 23 (4):710-725. doi:10.1890/12-137. 
<br>

*Further README files, code, and input data are available to run the analyses for impacts of climate change (https://github.com/EmilyKlein/KPFM2_Climate_change) and for running marine protected area and feedback management scenarios (https://github.com/EmilyKlein/KPFM2_MPA_FBM). Please refer there for that additoinal information.*

<br>

## (1) DATA INPUT FILES
**mlt, mst, nlt, nst**: The four main .txt input for the KPFM2 ecosystem model, one for each parameterization across krill movement (no movement, *n*, and full movement as passive drifters, *m*) and predator sensitivity to krill availability (hyperstable, *s*, and linear, *l*). The parameterizations are run individually in KPFM2. 

<br>

## (2) CODE FOR ANALYSIS
The analysis utilizes several functions, some of which call the ecosystem model, KPFM2, as described in Watters et al. (2013). Detail on KPFM2 is available in the documentation for that manuscript.
  
### 2.1 The base KPFM2 code
`load.funcs.r`: Function to load and source all functions in a folder (use to easily get all necessary functions into the R workspace)<br>
`import.all.parameters.r`: Function to load the KPFM2 input files (e.g., mlt, mst, nlt, nst). <br>
`ssmu.ss.r`: The KPFM2 model, run in stochastic mode. Default plots are by SSMU and predator group for abundance and recruitment. This calls the additional functions: <br>
&emsp;&emsp;`allocate.catch.r`: Generalized function to allocate catch among areas <br>
&emsp;&emsp;`bt.abund,var.r`: Generates the variable bathtub krill abundances <br>
&emsp;&emsp;`m2f.root.r`: Estimates root of catch equation 	<br>
&emsp;&emsp;`plot.ss.C.r`: Plots changes in catch when fishing is implemented.<br>
&emsp;&emsp;`plot.ss.N.r`: Plots changes in abundance of krill and predators)<br>
&emsp;&emsp;`plot.ss.R.r`: Plots changes in recruitment of krill and predators)<br>
 
`ssmu.mc.r`: Function to run Monte Carlo simulations of KPFM2. Default plots are by SSMU and predator group for abundance and recruitment, and this calls the following additional functions:<br>
&emsp;&emsp;`plot.mc.C.r`: Plots changes in catch when fishing is implemented.<br>
&emsp;&emsp;`plot.mc.N.r`: Plots changes in abundance of krill and predators)<br>
&emsp;&emsp;`plot.mc.R.r`: Plots changes in recruitment of krill and predators)<br>


### 2.2 Running across multiple scenarios and risk assessment 
`main.r`: Function to run either or both Monte Carlo simulations across multiple scenarios and risk assessment. If designated by the user via (`if runmcsims=TRUE`), this can call the following functions:<br>
&emsp;&emsp;`run.mcsims.r`<br>
&emsp;&emsp;`assess.risk.predators.r` <br>
&emsp;&emsp;`assess.krill.fishery.r`<br>
&emsp;&emsp;`assess.regional.krill.r` <br>

`run.mcsims.r`: Runs Monte Carlo simulations over multiple fishing scenarios and parameterizations. Calls the following functions: <br>
&emsp;&emsp;`get.gamma.fractions.r` <br>
&emsp;&emsp;`ssmu.ss.r` <br> 
&emsp;&emsp;`ssmu.ss.mc.r` <br> 

`assess.risk.predators.r`: Risk assessment for the predator groups across multiple fishing options and parameterizations; averages output across parameterizations. Calls the following functions:<br>
&emsp;&emsp;`calc.risk.predators.r`: Calculates depletion risks for predator groups. This code calls the script <br>
&emsp;&emsp;&emsp;&emsp;`get.gamma.fractions.r` <br>
&emsp;&emsp;`merge.mc.r`: Aggregates output across Monte Carlo trials into a single output<br>
&emsp;&emsp;`relative.mc.r`: Makes one Monte Carlo trial output object relative to others. 
<br>

`assess.krill.fishery.r`: Performance assessment of the fishery in terms of krill catch over multiple fishing scenarios and parameterizations; averages output across parameterizations. Calls the functions: <br>
&emsp;&emsp;`calc.pms.krill.fishery.r`: Calculates performance measures for krill and the fishery.<br>
&emsp;&emsp;`get.gamma.fractions.r` <br>
&emsp;&emsp;`merge.mc.r`
<br>

`assess.regional.krill.r`: Computes regional krill performance measures; averages output across parameterizations. Calls the following functions:<br>
&emsp;&emsp;`calc.pms.regional.krill.r` <br>
&emsp;&emsp;`calc.regional.krill.mc.r`<br>
&emsp;&emsp;`get.gamma.fractions.r`<br>
&emsp;&emsp;`merge.regional.krill.mc.r`<br>
&emsp;&emsp;`relative.regional.krill.mc.r`<br>
<br>

**Plotting functions**<br>
`plot.risk.predators.all.r`: Plots potential for predator groups to be 75% of a “base case” scenario by SSMU.  <br>
`pot.risk.predators.sa.r` – plots across the different fishing options called by main() – aggregated by Subareas.<br>
`plot.recovery.predators.all.r` – plots the probability of recovery after 20 years of no fishing (e.g. Fig 2 in CM 51-07 report) <br>

<br>

### 2.3. Convention for naming output
For output from the initial Monte Carlo trials:`[parameterization].[fishing option].g[yield multiplier].mc`. For example: <br>
>`mst.0.0.mc`: Parameterization = movement + stable, fishing option = 0, yield multiplier = 0 <br>
>`nlt.1.g0.1.mc`: Parameterization = no movement + linear, fishing option = 1, yield multiplier = 0.1 <br>

For output from predator assessment, the merged files are: `[parameterization].[fishing option].g[yield multiplier].rel`. For example:<br> 
>`mst.0.0.rel`: Parameterization = movement + stable, fishing option = 0, yield multiplier = 0.<br>
>`nlt.1.g0.1.rel`: Parameterization = no movement + linear, fishing option=1, yield multiplier = 0.1. 
<br>

For output from predator assessment, the risk files are: `[parameterization].[fishing option].merge.rel.[proportion depleted].risk`. For example: <br>
>`mst.1.merge.rel.25.risk`: Parameterization = movement + stable, fishing option = 1, depletion  below 25% of non-MPA scenario.

For output from fishery and krill assessment, the convention naming of merged files is: `[parameterization].[fishing option].merge.mc`. For example: <br>
>`nlt.1.merge.mc`: Parameterization = no movement + linear, fishing option = 1 
<br>

For output from fishery and krill assessment, the convention naming of krill and fishery performance files  is: `[parameterization].[fishing option].merge.mc.[krill density threshold].pms`, e.g.: 
>`nlt.1.merge.mc.15.pms`: Parameterization = no movement + linear, fishing option = 1 , krill density for a threshold violation is 15g/m2.

<br>

## (3) STEPS FOR ANALYSIS

### 3.1 Using KPFM2 in stochastic mode: 
 1. Make sure all code for analysis denoted above is sourced. 
 2. Create folders for the code to call data from and to save output to. 
 3. Develop input data file and save in “data” folder: 
    1. Data is stored and uploaded into the R code for KPFM2 via a text file (“.txt”)
       1. *This can be easily updated in Excel. A guide to this input is forthcoming.* 
    1. KPFM2 uses four parameterizations – these are separate files usually saved in a “data” folder: mlt, mst, nlt, nst for movement-linear, movement-stable, no movement-linear, no movement-stable.
 4. Import each of the parameterizations using “import.all.parameters()” – these files must be named “mlt”, “mst”, “nlt”, and “nst” as appropriate or later aspects of the code cannot find them: 
    1. Make the file string reflects location of the data (.txt file), e.g.: <br>
    `mlt <- import.all.parameters(file.string ="C:/Users/Desktop/KPFM2/data/mlt.txt")`
    1. Response reports the SSMUs, bathtuns, number of years and seasons – check that these are correct, e.g.: <br>
    `[1] "The setup says you have 15 SSMU(s), 3 BATHTUB(s), 38 YEAR(s) of time series data, and 2 SEASON(s) specified"`
 5. Implement a stochastic model run 
    1. At minimum, designate the param.list (imported in #3 above), e.g.: <br>
    `> mlt.ss <- ssmu.ss(mlt)`
    1. Model run will produce the plots for abundance, recruitment, and catch by initial spatial unit, SSMU, with all predator groups plotted together. Note plots are relative to initial input, and catch plots will be blank if no fishing occurred. 
       1. *You can turn off plotting by commenting out code starting line 1252.* 
    1. To run with fishing, include based on fishing option, e.g.: <br>
    `> mlt.ss <- ssmu.ss(mlt, fishing.option=1)`
 6.	Output includes the initial model input and setup, abundance and recruitment for krill and predators, and consumption by predators, fishing, and krill growth (see code for details). 
 
 <br>

### 3.2	Running MC simulations of KPFM2

 1.	Follow steps 1-4 for the stochastic runs of KPFM2 above. 
 2.	Implement Monte Carlo simulations with ssmu 
    1. At minimum, designate the param.list, e.g.: 
    `> mst.mc <- ssmu.mc(mst)`
    1. Model run will produce the plots for abundance, recruitment, and catch by species and initial spatial unit, SSMU (note that catch plots will be blank if no fishing occurred). Plots are relative to initial input. To turn off plotting, run with plot.now = FALSE,, e.g.: <br> 
    `> mst.mc <- ssmu.mc(mst, plot.now=FALSE)`
    1. To run with fishing, include based on fishing option, e.g.: <br>
    `> mlt.ss <- ssmu.ss(mlt, fishing.option=1)`
 3.	Output includes the initial model input and setup, abundance and recruitment for krill and predators, and consumption by predators, fishing, and krill growth (*see code for details*). 
<br>

### 3.3. Running risk assessment
 1.	Make sure all needed codes are sourced. 
 2.	Set up input and folders for output
    1. Make sure all parameterizations are available as .rdata files
       1. NB: if parameterizations only exist as text files, first import them via import.all.parameters() (see 3.1-4 above), then save as an .rdata file via save():<br>
       `save(mst,file="C:/Users/emily/Desktop/SWFSC/FOOSA/Pixelation/data/mst.rdata")`
    1. Create folder for model output, specify output path for model run.
 3.	Inside the `main.r` script, update the following as needed: 
    1. Thus far in running risk assessments, the assessment has been based on a scenario with no fishing. Therefore, update this with which fishing options to run, but keep the no-fishing, i.e. fishing.options=0, included: <br>
    `fishing.options=c(0,1)` runs no fishing (0) against fishing option 1<br>
    `fishing.options=c(0,1,2)` runs no fishing (0) against fishing option 1 and against fishing option 2 (but there is no comparison between 1 and 2).
    1. Update “parameter.path” to reference location of parameterization data files in (1a above), and “output.file.path” to reference where output should go (1b above). 
    1. Update number of Monte Carlo trials to run (“ntrials”), number of years for the model to run (“nyears”), when fishing should start and stop, and the parameters actual.gamma and sd.krill.rdev
       1. NB: If the goal is to look at predator recovery, there must be a period at the end of the model run when no fishing occurs, i.e. the year when fishing stops is not the end year of analysis (e.g. fishing stops at year 30 in a 50-year model run)
    1. Update risk reference levels and density thresholds.
4. The main.r code has two options: running Monte Carlo trials for KPFM2 (calls the run.mcsims() code), or running assessments (calls three different codes), which can be run together (default) or separately: 
    1. main(runmcsims = TRUE) calls the MC trials
    1. main(runassessments = TRUE) calls the assessments, and this requires that the MC sims have already been run previously. 
    1. Output from the assessment scripts is necessarily for plotting.
    1. It has an area where fishery is updated, use this only if the fishery is already updated in input files. Note also that there is an Option 7 update included in this function (“JM 12/1/12”)
<br>

### 3.4	Plotting 
Plots are generated automatically in the stochastic (ssmu.ss.r) and Monte Carlo (ssmu.mc.r) scripts, with `plot.now=TRUE` (default).<br>

Plotting output of risk assessments plots require results from running `main.r` above, with both run.mcsims and run.assessments as TRUE (which is the default). These output are contingent on the fishing options called in main() – that is, which fishing options are called - although which aspects of those comparisons can be altered in the plotting section of the functions.<br>

 1.	Update the script for plotting: 
    1. Update file.path as necessary to reference location of  `main.r` output 
    1. The script is written for plotting across four fishing options (i.e. 1, 2, 3, 7); comment out the ones not used throughout the script
    1. Names of categories and titles for plots at the end of the script.
 2.	Plot risk assessments for predators with:
    1. `plot.risk.predators.r` plots each predator group by SSMUs and fishing options
    1. `plot.risk.predators.all.r` plots species groups and all SSMUs across several scenarios
    1. `plot.risk.predators.sa.r` plots each predator group by fishing option, with SSMUs averaged over the subarea
    1. `plot.recovery.predators.all.r` plots probability that predators will recover by SSMU and fishing option, across several scenarios
       1. requires a period of no-fishing included in the analysis
 3.	Plot assessment of krill fishery with  `plot.perf.fishery.catch` and `plot.perf.fishery.relcatch`. Look to plotting aspect of script to change what is plotted (e.g. by SSMUs or by fishing options). 
 4.	Plot outcomes for `krill with plot.risk.regkrill`. Look to plotting aspect of script to change what is plotted (e.g. by SSMUs or by fishing options).
 5.	*NB*: Can zoom in on the trigger by changing xlim on plot to xlim=c(0.0, 0.13) 


