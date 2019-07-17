require(reshape2)


BinActivityData.DataFrame<-function(damForMelt,binsize.min){
  
  h.min<-min(damForMelt$ElapsedHours)
  h.max<-max(damForMelt$ElapsedHours)
  
  binsize.hours<-binsize.min/60.0
  
  y<-seq(h.min,h.max,by=binsize.hours)
  if(y[length(y)]<h.max)
    y<-c(y,h.max)
  
  z<-cut(damForMelt$ElapsedHours,y,include.lowest=TRUE)
  r.min<-aggregate(damForMelt$ElapsedHours~z,FUN=mean)
  results<-data.frame(r.min,rep(damForMelt$DAM[1],nrow(r.min)))
  for(i in 1:32){
    r.A<-aggregate(damForMelt[,i+2]~z,FUN=sum)  
    results<-data.frame(results,r.A[,2])
  }
  
  names(results)<-c("Interval","ElapsedHours","DAM",1:32)
  results
  
}

BinActivityData.Chamber<-function(dam,channel,binsize.min){
  tcol<-paste("Channel",channel,sep="")
  tmp<-dam$Data[,c("ElapsedHours",tcol)]
  
  h.min<-min(tmp$ElapsedHours)
  h.max<-max(tmp$ElapsedHours)
  
  binsize.hours<-binsize.min/60.0
  
  y<-seq(h.min,h.max,by=binsize.hours)
  if(y[length(y)]<h.max)
    y<-c(y,h.max)
  
  z<-cut(tmp$ElapsedHours,y,include.lowest=TRUE)
  
  r.min<-aggregate(tmp$ElapsedHours~z,FUN=mean)
  r.A<-aggregate(tmp[,tcol]~z,FUN=sum)
  
  results<-data.frame(r.min,r.A[,2])
  names(results)<-c("Interval","ElapsedHours",tcol)
  results
}
GetRasterProcessedData.DAM<-function(dam,starvation.results,binsize.min=30){
  starvation.results<-subset(starvation.results,DAM==dam$Number)
  data.for.melt<-data.frame(dam$Data$ElapsedHours,rep(dam$Number,nrow(dam$Data)),dam$Data[,11:42])
  names(data.for.melt)<-c("ElapsedHours","DAM",1:32)
  data.for.melt.binned<-BinActivityData.DataFrame(data.for.melt,binsize.min)
  data.melted<-melt(data.for.melt.binned,id.vars=c("Interval","ElapsedHours","DAM"),variable.name="Channel",value.name="Counts")

  for(i in 1:nrow(starvation.results)){
    ch<-starvation.results[i,"Channel"]
    death<-starvation.results[i,"HrsAtDeath"]
    if(is.na(death))
      death<-0
    rev.index<-data.melted$Channel == ch & data.melted$ElapsedHours>(death-binsize.min/60)
    index<-!rev.index
    data.melted<-data.melted[index,]
  }

  Trt<-rep(NA,nrow(data.melted))
  data.melted<-data.frame(data.melted,Trt)
  data.melted
}

GetRasterProcessedData.DAMList<-function(dam.list,starvation.results,exp.design,binsize.min=30){
  results<-GetRasterProcessedData.DAM(dam.list[[1]],starvation.results,binsize.min)
  for(i in 2:length(dam.list)){
    tmp<-GetRasterProcessedData.DAM(dam.list[[i]],starvation.results,binsize.min)
    results<-rbind(results,tmp)
  }
  results<-AssignTrt(results,exp.design)
  results
}

AddYCoords<-function(melted.data){
  tmp<-aggregate(melted.data$ElapsedHours,by=list(Channel=melted.data$Channel, DAM=melted.data$DAM),max)
  ycoords<-rank(-1*tmp$x,ties.method = "first")
  from.to<-data.frame(tmp$DAM,tmp$Channel,ycoords)
  names(from.to)<-c("DAM","Channel","Y")
  Y<-rep(NA,nrow(melted.data))
  for(i in 1:nrow(melted.data)){
    ch<-melted.data$Channel[i]
    dm<-melted.data$DAM[i]
    index<-from.to$Channel==ch & from.to$DAM==dm
    if(sum(index)>0)
      Y[i]<-from.to[index,3]
    else
      Y[i]<-NA
  }
  results<-data.frame(melted.data,Y)
  results
}

MakeRasterPlots<-function(processedData,x.limits=NA){
  levels<-unique(processedData$Trt)
  max.count<-quantile(processedData$Counts,0.99)
  glist<-list()
  for(i in levels){
    tmp<-subset(processedData,Trt==i)
    tmp<-AddYCoords(tmp)
    if(is.na(x.limits)){
      p<-ggplot(tmp,aes(ElapsedHours,Y,width=0.5)) +geom_tile(aes(fill=Counts))+ scale_fill_viridis(limits=c(0,max.count))+ ylab("Fly") + ggtitle(paste("TRT:",i))
    }
    else {
      p<-ggplot(tmp,aes(ElapsedHours,Y,width=0.5)) +geom_tile(aes(fill=Counts))+ scale_fill_viridis(limits=c(0,max.count))+ ylab("Fly") + ggtitle(paste("TRT:",i)) +
        xlim(x.limits[1],x.limits[2])  
    }
    
    tt<-paste("Name",i,sep="")
    glist[[tt]]<-p
    
  }
  multiplot(plotlist=glist,cols=2)
  glist
}
