## Usage example
rm(list=ls())
source("DAMFunctions.R")
dam.list<-ImportDAMData()
result<-GetHoursAtDeathForDAMList(dam.list)
ed<-GetExpDesign()
new.result<-AssignTrt(result,ed)
SurvPlots(new.result)
