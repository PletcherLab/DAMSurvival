## Usage example
rm(list=ls())
source("DAMFunctions.R")
dam.list<-ImportDAMData()
exp.design<-ImportExpDesign()
results<-ComputeStarvationResults(dam.list,exp.design)
plot.counts.dam.list(dam.list,results)

## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
## Make sure to check each plot to ensure the
## calculations are working for your data!!!!
## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SurvPlots(results)












