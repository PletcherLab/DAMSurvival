## Usage example
rm(list=ls())
source("DAMFunctions.R")
dam.list<-ImportDAMData()
results<-GetHoursAtDeathForDAMList(dam.list)