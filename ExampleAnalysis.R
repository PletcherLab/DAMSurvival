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

## If you want to limit the output to include only
## a subset of treatments, create a string vector
## (case-sensive) and pass it to the function.

trt<-c("TrtA","TrtB")
SurvPlots(results, trt.list=trt)

## To see some fancy plots and output them to a file
SurvPlotsFancy(results) ## Saves to SurvPlot.png
## or 
SurvPlotsFancy(results,filename="MySurveCurves.png")
## Set conf.int=FALSE if you don't want the shaded regions
SurvPlotsFancy(results,conf.int=FALSE) 

## You can also filter by treatments
trt<-c("TrtA","TrtB")
SurvPlotsFancy(results,trt.list=trt,conf.int=FALSE) 

## To output the survival values at event times
## so that you can plot using your own software.
OutputSurvData(results)  ## Saves to SurvData.csv
## or
OutputSurvData(results,filename="MyResults.csv")  



## Do you want to see raster plots of the activity per fly?
source("RasterFunctions.R")
binsize.minutes<-30
processedData<-GetRasterProcessedData.DAMList(dam.list,results,exp.design,binsize.minutes)
gps<-MakeRasterPlots(processedData)


## To fix the xaxis range for direct comparison
gps<-MakeRasterPlots(processedData,x.limits=c(0,50))



