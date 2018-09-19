## SurvivalDAMFunctions

#Function to import and name all .txt files in the directory
ImportDAMData <- function(){
install.packages("data.table")
  GetDAMFiles <- function(z) {
      dam <- fread(z, stringsAsFactors = FALSE)
      colnames(dam) <- c("Num",	"Date",	"Time",	"Status",	"Blank1",	"Blank2",	"Blank3",	"Blank4",	"Blank5",	
                       "Light",	"Channel1",	"Channel2",	"Channel3",	"Channel4",	"Channel5",	"Channel6",	"Channel7",	"Channel8",
                      "Channel9",	"Channel10",	"Channel11",	"Channel12",	"Channel13",	"Channel14",	"Channel15",	
                      "Channel16",	"Channel17",	"Channel18",	"Channel19",	"Channel20",	"Channel21",	"Channel22",
                      "Channel23",	"Channel24",	"Channel25",	"Channel26",	"Channel27",	"Channel28",	"Channel29",	
                      "Channel30",	"Channel31",	"Channel32")
      return(as.data.frame(dam))
   }

  dam.files.list <- list.files(path = getwd(), pattern = "*.txt", all.files = FALSE,  full.names = FALSE, recursive = FALSE,
                               ignore.case = FALSE, include.dirs = FALSE)
  dam <- lapply(dam.files.list, GetDAMFiles)
#returns list of unnamed data frames. Need to name with proper number and DAM prefix to pass to the next functions.

  pattern <- "Monitor"
  replacement <- "DAM"
  dam.files.list <- gsub(pattern, replacement,  dam.files.list)

#applies the names to the first columns of all dataframes. Want it to apply the names to the elements of the list (dataframes)
  dam <- lapply(dam, setNames, dam.files.list)

#perhaps it's best to leave the dataframes as part of the list then I can pass the list to the next function. 
}

#function to find the index number for the last activity count
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

GetHoursDeath<-function(dam){
  
  dam$CalDateTime <- as.POSIXct (paste(dam$Date, dam$Time), format = "%d-%b-%y %H:%M:%S")
  
  #Identify start time based on status changing from 51 to 1. Save all 1s as vector RecordingTime. 1st element is StartTime
  dam<-subset(dam,dam$Status==1)
  
  #Extract last element from time based on a positive activity value in an activity column
  #First index so that only channel data greater than 0 is included
  death.times<-(lapply(dam[,11:42],GetDeathTime,dam$CalDateTime))
  
  tmp<-rep(-1,32)
  for(i in 1:32){
    if(is.na(tmp[i])){
      tmp[i]<-NA
    }
    else {
      tmp[i]<-difftime(death.times[[i]],dam$CalDateTime[1],units="hours")
    }
  }
  
  hours.at.death<-tmp
  hours.at.death
}
