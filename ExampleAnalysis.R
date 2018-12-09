## Usage example
rm(list=ls())
source("DAMFunctions.R")
dam.list<-ImportDAMData()
exp.design<-ImportExpDesign()
results<-ComputeStarvationResults(dam.list,exp.design)
plot.counts.dam.list(dam.list,results)

## Check that the data look good!




SurvPlots(new.result)












