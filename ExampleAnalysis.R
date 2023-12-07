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

## If you want to delete certain flies based on the count plots
## you can do so here by uncommenting the following code and
## adjusting the dam and channel lists as needed.
#dam.list.to.remove<-c(24,24)
#channel.list.to.remove<-c(25,29)
#results<-RemoveChannelsFromResults(results,dam.list.to.remove,channel.list.to.remove)


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
binsize.minutes<-10
processedData<-GetRasterProcessedData.DAMList(dam.list,results,exp.design,binsize.minutes)
gps<-MakeRasterPlots(processedData)


## To fix the xaxis range for direct comparison
gps<-MakeRasterPlots(processedData,x.limits=c(0,50))


## Do you want to get summary activity stats for each treatment?
## The following function will calculate mean and SEM of activity counts
## across individual flies, for the first part of the experiment.  If an hours
## argument is given, e.g., hours=18, then for the first 18 hours of the experiment.
## if no hours argument is given, then the first 24 hours of the experiment is used.
## remove.zeros=TRUE will ignore tubes in which there was zero activity.  
results<-SummarizeHoursActivity(dam.list,exp.design,hours=24,remove.zeros=TRUE)

ggplot(results, aes(x=Trt, y=Mean, fill=Trt)) + 
  geom_errorbar( aes(x=Trt, ymin=Mean-SEM, ymax=Mean+SEM), width=0.4, colour="black", alpha=0.9, size=1) +
  geom_bar(stat = "identity") 
  

