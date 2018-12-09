## SurvivalDAMFunctions
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
  dam<-read.table(z,sep="\t")
  colnames(dam) <- c("Num",	"Date",	"Time",	"Status",	"Blank1",	"Blank2",	"Blank3",	"Blank4",	"Blank5",	
                     "Light",	"Channel1",	"Channel2",	"Channel3",	"Channel4",	"Channel5",	"Channel6",	"Channel7",	"Channel8",
                     "Channel9",	"Channel10",	"Channel11",	"Channel12",	"Channel13",	"Channel14",	"Channel15",	
                     "Channel16",	"Channel17",	"Channel18",	"Channel19",	"Channel20",	"Channel21",	"Channel22",
                     "Channel23",	"Channel24",	"Channel25",	"Channel26",	"Channel27",	"Channel28",	"Channel29",	
                     "Channel30",	"Channel31",	"Channel32")
  
  dam<-IsolateLastExperiment(dam)
  dam<-AddCalcDateTime(dam)
  dam<-AddElapsedHours(dam)
  
  
  monitor.number<-GetMonitorNumberFromFileName(z)
  dam<-list(Number=monitor.number,Data=dam)
  dam
}

AddElapsedHours<-function(dam){
  ElapsedHours<-difftime(dam$CalDateTime, dam$CalDateTime[1],units="hours")
  dam$ElapsedHours<-as.numeric(ElapsedHours)
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
  hours.at.death<-(lapply( dam.data[,11:42],GetSingleDeathTime, dam.data$ElapsedHours))
  unlist(hours.at.death)
}
GetHoursatDeathForDAM<-function(dam){
  had<-GetHoursAtDeathVector(dam)
  lastMeasure<-dam$Data$ElapsedHours[nrow(dam$Data)]
  status<-rep(1,length(had))
  status[had==lastMeasure]<-0
  damnumber<-rep(dam$Number,32)
  pos<-1:32
  Trt<-rep(NA,32)
  
  result<-data.frame(damnumber,pos,Trt,had,status)
  names(result)<-c("DAM","Channel","Trt","HrsAtDeath","Status")
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
  row.names(results)<-1:nrow(results)
  results
}


ComputeStarvationResults<-function(dam.list,expDesign){
  tmp<-GetHoursAtDeathForDAMList(dam.list)
  tmp2<-AssignTrt(tmp,expDesign)
  tmp2
}

#--------------------------------------------------------#
### Functions to import  design and sort tubes into treatments.
#--------------------------------------------------------#
#function to read in ExpDesign.txt file and assign a treatment group to the trt column of result. 
ImportExpDesign <- function(){
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
  surv.object<-Surv(result$HrsAtDeath,result$Status)
  SurvCurve <- survfit(surv.object~Trt,data=result)
  SurvComp <- survdiff(surv.object~Trt,data=result)
  lLab <- gsub("Trt=","",names(SurvCurve$strata))
  plot(SurvCurve, col=1:length(lLab), lty = c(1,1))
  legend("bottomleft",legend = lLab,  col=1:length(lLab), lty = c(1,1),cex = 1)
  print(SurvComp)
}


DoItAll<-function(){
  dam.list<-ImportDAMData()
  exp.design<-ImportExpDesign()
  results<-ComputeStarvationResults(dam.list,exp.design)
  plot.counts.dam.list(dam.list,results)
  SurvPlots(results)
}

#--------------------------------------------------------#
### Functions for plotting.
#--------------------------------------------------------#

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

plot.counts.channel<-function(dam,channel,dam.number=0, result.vector=NULL){
  elapsedhours<-dam$ElapsedHours
  tmp<-paste("Channel",channel,sep="")
  channel.data<-dam[,tmp]
  if(dam.number>0)
    ttl<-paste("DAM: ",dam.number," - Channel: ",channel, sep="")
  else
    ttl<-paste("Channel: ",channel, sep="")
  p<-ggplot(data=dam,aes(x=elapsedhours,y=channel.data)) + geom_point(size=1) + ggtitle(ttl) +xlab("Hours") + ylab("Counts")
  if(!is.null(result.vector)) {
    tmp.y<-channel.data[elapsedhours==result.vector$HrsAtDeath]
    if(result.vector$Status==1)
      colorstring="red"
    else
      colorstring="green"
    p<- p + geom_point(aes(x=result.vector$HrsAtDeath, y=tmp.y), colour=colorstring, size=2)
  }
  p
}

plot.counts.dam<-function(dam, results.frame){
  dam.number<-dam$Number
  dam<-dam$Data
  results<-subset(results.frame,results.frame$DAM==dam.number)
 
  plot.list<-list()
  if(!dir.exists("Plots"))
    dir.create("Plots")
  tmp.dir<-paste(paste("Plots\\DAM_",dam.number,sep=""))
  if(dir.exists(tmp.dir))
    unlink(tmp.dir,recursive=TRUE)
  dir.create(tmp.dir)
  fn<-paste("Plots\\DAM_",dam.number,"\\",sep="")
  for(i in 1:16){
    fn.long<-paste(fn,"Channel_",i,".png",sep="")
    png(fn.long,width=960,height=480)
    result.vector<-results[results$Channel==i,]
    p<-plot.counts.channel(dam,i,dam.number,result.vector)
    plot(p)
    graphics.off() 
  }
}

plot.counts.dam.list<-function(dam.list,results){
  for(i in 1:length(dam.list)){
    tmp<-paste("Plotting ",i," of ", length(dam.list),".",sep="")
    print(tmp)
    plot.counts.dam(dam.list[[i]],results)
  }
}
