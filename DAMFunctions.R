## SurvivalDAMFunctions
require(data.table)
library(data.table)
require(survival)
library(survival)


#--------------------------------------------------------#
## Functions for importing DAM data, adding new columns 
## and isolating data of interest.
#--------------------------------------------------------#

#Need to specify monitor files
#Function to import and name all .txt files in the directory.
ImportDAMData <- function(){
  dam.files.list <- list.files(path = getwd(), pattern = "M{1,}.*txt", all.files = FALSE,  full.names = FALSE, recursive = FALSE,
                               ignore.case = FALSE, include.dirs = FALSE)
  dam <- lapply(dam.files.list, GetDAMFile)
  #returns list of unnamed data frames. Need to name with proper number and DAM prefix to pass to the next functions.
  
  pattern <- "Monitor"
  replacement <- "DAM"
  dam.files.list <- gsub(pattern, replacement,  dam.files.list)
  names(dam)<-dam.files.list
  
  dam
  #perhaps it's best to leave the dataframes as part of the list then I can pass the list to the next function. 
}

GetDAMFile <- function(z) {
  dam <- fread(z, stringsAsFactors = FALSE)
  colnames(dam) <- c("Num",	"Date",	"Time",	"Status",	"Blank1",	"Blank2",	"Blank3",	"Blank4",	"Blank5",	
                     "Light",	"Channel1",	"Channel2",	"Channel3",	"Channel4",	"Channel5",	"Channel6",	"Channel7",	"Channel8",
                     "Channel9",	"Channel10",	"Channel11",	"Channel12",	"Channel13",	"Channel14",	"Channel15",	
                     "Channel16",	"Channel17",	"Channel18",	"Channel19",	"Channel20",	"Channel21",	"Channel22",
                     "Channel23",	"Channel24",	"Channel25",	"Channel26",	"Channel27",	"Channel28",	"Channel29",	
                     "Channel30",	"Channel31",	"Channel32")
  
  
  dam<-as.data.frame(dam)
  dam<-IsolateLastExperiment(dam)
  dam<-AddCalcDateTime(dam)
  
  monitor.number<-GetMonitorNumberFromFileName(z)
  dam<-list(Number=monitor.number,Data=dam)
  dam
}

IsolateLastExperiment<-function(dam){
  status.runs<-rle(dam$Status)
  runs.1<-which(status.runs$values==1)
  last.1.run<-runs.1[length(runs.1)]
  start.index<-sum(status.runs$lengths[1:last.1.run-1])+1
  end.index<-sum(status.runs$lengths[1:last.1.run])
  
  new.dam<-dam[start.index:end.index,]
  new.dam
}

AddCalcDateTime<-function(dam){
  #Identify start time based on status changing from 51 to 1. Save all 1s as vector RecordingTime. 1st element is StartTime
  dam<-subset(dam,dam$Status==1)
  
  dam$CalDateTime <- as.POSIXct (paste(dam$Date, dam$Time), format = "%d %b %y %H:%M:%S")
  
  if(sum(is.na(dam$CalDateTime))>5)
    dam$CalDateTime <- as.POSIXct (paste(dam$Date, dam$Time), format = "%d-%b-%y %H:%M:%S") 
  dam
}

GetMonitorNumberFromFileName<-function(s){
  s<-substr(s,8,nchar(s))
  tmp<-regexpr("\\.",s)[1]
  s<-substr(s,1,tmp-1)
  as.numeric(s)
}


#--------------------------------------------------------#
### Functions extracting death times and organizing into data frame.
#--------------------------------------------------------#

#function to find the index number for the last activity count
GetSingleDeathTime<-function(data,times){
  tmp<-which(data>0)
  if(length(tmp)==0){
    result<-NA
  }
  else {
    time.index<-tmp[length(tmp)]
    result<-times[time.index]  
  }
  result
}

GetHoursAtDeathVector<-function(dam){
  dam.data<-dam$Data
  #Extract last element from time based on a positive activity value in an activity column
  #First index so that only channel data greater than 0 is included
  death.times<-(lapply( dam.data[,11:42],GetSingleDeathTime, dam.data$CalDateTime))
  
  tmp<-rep(-1,32)
  for(i in 1:32){
    if(is.na(tmp[i])){
      tmp[i]<-NA
    }
    else {
      tmp[i]<-difftime(death.times[[i]], dam.data$CalDateTime[1],units="hours")
    }
  }
  hours.at.death<-tmp
  hours.at.death
}
GetHoursatDeathForDAM<-function(dam){
  had<-GetHoursAtDeathVector(dam)
  damnumber<-rep(dam$Number,32)
  pos<-1:32
  Trt<-rep(NA,32)
  
  result<-data.frame(damnumber,pos,Trt,had)
  names(result)<-c("DAM","Channel","Trt","HrsAtDeath")
  result
}

#need function to pass list, with name, one dataframe at a time to GetHoursDeath which doesn't work on a list. 
#can use names from dam.files.list
GetHoursAtDeathForDAMList <- function(dam.list){
  for(i in 1:length(dam.list)){
    #pull out dataframe with it's name. Pass it to GetHoursDeath. Name comes from dam.files.list[i]
    #deathhours<-lapply(dam[i],GetHoursDeath) 
    deathhours<-GetHoursatDeathForDAM(dam.list[[i]]) 
    if(exists("results",inherits=FALSE)){
      results<-rbind(results,deathhours)
    }
    else {
      results<-deathhours
    }
  }
  results
}


#--------------------------------------------------------#
### Functions to import  design and sort tubes into treatments.
#--------------------------------------------------------#
#function to read in ExpDesign.txt file and assign a treatment group to the trt column of result. 
GetExpDesign <- function(){
  ed <- read.csv("ExpDesign.csv", row.names=NULL)
  ed
}

AssignTrt <- function(result, ed){
  for(i in 1:nrow(result)){
    result[i,]$Trt<-GetTreatment(ed,result[i,]$DAM,result[i,]$Channel)
  }
  result
}


GetTreatment<-function(ed,dam,channel){
  tmp<-subset(ed,ed$Channel==channel & ed$DAM==dam)
  tmp<-as.character(tmp$Trt)
  if(length(tmp)==0)
    tmp<-"NA"
  tmp
}


#--------------------------------------------------------#
### Functions for survival analysis.
#--------------------------------------------------------#
#Survival plotting code
SurvPlots <- function(result){
  individual.trt <- DetermineTreatments(result)
  surv.object<-Surv(result$HrsAtDeath,rep(1,length(result$HrsAtDeath)))
  SurvCurve <- survfit(surv.object~result$Trt)
  SurvComp <- survdiff(surv.object~result$Trt)
  plot(SurvCurve, col=1:length(individual.trt), lty = 1)
  print(individual.trt)
  legend("bottomleft",  legend = individual.trt, col = 1:length(individual.trt), lty = 1)
  print(SurvComp)
}

#function to determine the treatments included in the analysis. Sorts alphabetically. 
DetermineTreatments  <- function(new.result){
  treatments <- new.result$Trt
  individual.trt <- unique(treatments)
  as.character(individual.trt)
  sort(individual.trt)
  individual.trt
}










