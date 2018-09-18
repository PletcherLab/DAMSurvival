
#Strategy to create a program to find and plot lifespan of flies in activity monitors:

#Steps
#read in DAM txt files. Prompt to assign treatment group to each monitor file, or add headder with all columns labeled.
#merge data into single frame for similar treatments using time and date columns to align data
#Activity data is in column 11-33
#find start of experiment based on when DAM output file shows status change from 51 to 1. Record time in same row.
#delete all prior data containing 51 readings in column 4 except the 1 flanking the start of the experiment
#
#determine time last alive by searching through the data from the bottom to find the first nonzero entry
#determine lifespan by subtracting the time of death from the start time
#populate a new vector create new vectors of time lived for each treatment
#stack survival data in format for SurvComp analysis. Add second column of equal length with 1 as value (to represent deaths)
#
#
#
#Read DAM file into R and add header with column names
DAM13 <- read.table("Monitor13.txt", header = FALSE)
colnames(DAM13)<-c("Num",	"Date",	"Time",	"Status",	"Blank1",	"Blank2",	"Blank3",	"Blank4",	"Blank5",	
"Light",	"Channel1",	"Channel2",	"Channel3",	"Channel4",	"Channel5",	"Channel6",	"Channel7",	"Channel8",
"Channel9",	"Channel10",	"Channel11",	"Channel12",	"Channel13",	"Channel14",	"Channel15",	
"Channel16",	"Channel17",	"Channel18",	"Channel19",	"Channel20",	"Channel21",	"Channel22",
"Channel23",	"Channel24",	"Channel25",	"Channel26",	"Channel27",	"Channel28",	"Channel29",	
"Channel30",	"Channel31",	"Channel32")

#Merge date and time columns into one and identify time format. Some issue with time formating, 
#guessing problems are with the month abbreviation.Tried POSIXct and POSIXlt.
DAM13$CalDateTime <- as.POSIXct (paste(DAM13$Date, DAM13$Time), format = "%d-%b-%y %H:%M:%S")

#Identify start time based on status changing from 51 to 1. Save all 1s as vector RecordingTime. 1st element is StartTime
DAM13<-subset(DAM13,DAM13$Status==1)




GetDeathTime<-function(data,times){
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


#Extract last element from time based on a positive activity value in an activity column
#First index so that only channel data greater than 0 is included
death.times<-(lapply(DAM13[,11:42],GetDeathTime,DAM13$CalDateTime))

tmp<-rep(-1,32)
for(i in 1:32){
  if(is.na(tmp[i])){
    tmp[i]<-NA
  }
  else {
    tmp[i]<-difftime(death.times[[i]],DAM13$CalDateTime[1],units="hours")
  }
}

hours.at.death<-tmp
rm(tmp)




#Trying to figure out a way to not type all channel + number repetedly
AllChannels <-paste0("Channel", 1:32)
DAM13[,colnames(DAM13(AllChannels))]

DAM13[length("CalDateTime"),"CalDateTime"]



#trying to import all txt files into dataframes at one time.
filenames <- list.files(path="C:/DAMIsolationStarvationMale",
                        pattern="xyz+.*txt")


#Survival plotting code
library(survival)
Survdata <- read.csv (file = "SurvivalDataR.csv", header = TRUE, sep = ",")

surv.object<-Surv(Survdata$Age,rep(1,length(Survdata$Age)))
SurvCurve <- survfit(surv.object~Survdata$Trt)
SurvComp <- survdiff(surv.object~Survdata$Trt)
plot(SurvCurve, col=c(1,2))
print(SurvComp)
plot(SurvComp)



