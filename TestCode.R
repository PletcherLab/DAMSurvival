GetHoursActivity<-function(dam.list,expDesign, hours=24,remove.zeros=TRUE){
  for(i in 1:length(dam.list)){
    dam<-dam.list[[i]]
    acounts<-GetHoursActivityForDAM(dam.list[[i]],hours) 
    if(exists("results",inherits=FALSE)){
      results<-rbind(results,acounts)
    }
    else {
      results<-acounts
    }
  }
  row.names(results)<-1:nrow(results)
  results<-AssignTrt(results,expDesign)
  if(remove.zeros==TRUE){
    results<-subset(results,results$Counts>0)
  }
  results
}


GetHoursActivityForDAM<-function(dam,hours=24){
  dam.data<-dam$Data
  fdata<-subset(dam.data,dam.data$ElapsedHours<=hours)
  acounts<-(lapply(fdata[,11:42],sum))
  acounts<-unlist(acounts)
  damnumber<-rep(dam$Number,32)
  pos<-1:32
  Trt<-rep(NA,32)
  hrs<-rep(hours,32)
  result<-data.frame(damnumber,pos,Trt,acounts,hrs)
  names(result)<-c("DAM","Channel","Trt","Counts","Hours")
  result
}

SummarizeHoursActivity<-function(dam.list,expDesign,hours=24,remove.zeros=TRUE){
  tmp<-GetHoursActivity(dam.list,expDesign,hours,remove.zeros)
  tmp2<-aggregate(tmp$Counts,list(tmp$Trt),mean)
  tmp3<-aggregate(tmp$Counts,list(tmp$Trt),sd)
  tmp4<-aggregate(tmp$Counts,list(tmp$Trt),length)
  tmp5<-tmp3[,2]/sqrt(tmp4[,2])
  results<-data.frame(tmp2,tmp5,tmp4[,2])
  names(results)<-c("Trt","Mean","SEM","N")
  
  a<-aov(tmp$Counts~tmp$Trt)
  print(summary(a))
  
  results
  
}