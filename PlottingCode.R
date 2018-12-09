##########################
# Sample script for binning raw DFM data and making comparison plots
# K. Hoffman
# Last update Dec 20 2017
##########################

### Step 0: set working directory and install/load required packages
#can install packages using install.packages("readxl"), etc.
setwd("/Users/khoffman/Downloads/SampleFlyCode/")
source("TidyFunctions.R")
library("readxl")
library("reshape")
library("ggplot2")

### Step 1: Use this binning code to create binned
# excel files of your raw data
source("SingleWellChamber.R")
source("SaveBinnedData.R")
p<-ParametersClass.SingleWell()
p<-SetParameter(p, Feeding.Threshold.Value = 40, Feeding.Interval.Minimum = 40,
                Feeding.Minevents=5, Tasting.Threshold.Interval=c(10,40))
binInterval = 30 #Set to desired interval in minutes. 
# Note: Anytime this interval changes, the 'Bin Data' section needs to be re-run 
#      before saving or plotting data.

#--------Bin Data-------------
#Change numeric values to correspond to data
dfm1 <- DFMClass(1,p)
dfm2 <- DFMClass(2,p)
dfm3 <- DFMClass(3,p)
dfm4 <- DFMClass(4,p)
dfm5 <- DFMClass(5,p)
dfm6 <- DFMClass(6,p)
dfm7 <- DFMClass(7,p)
dfm8 <- DFMClass(8,p)
dfm9 <- DFMClass(9,p)
dfm10 <- DFMClass(10,p)
dfm11 <- DFMClass(11,p)
dfm12 <- DFMClass(12,p)
dfm13 <- DFMClass(13,p)

# Bin by 30 min
bindfm1 <- BinFeedingData.Licks(dfm1, binInterval)
bindfm2 <- BinFeedingData.Licks(dfm2, binInterval)
bindfm3 <- BinFeedingData.Licks(dfm3, binInterval)
bindfm4 <- BinFeedingData.Licks(dfm4, binInterval)
bindfm5 <- BinFeedingData.Licks(dfm5, binInterval)
bindfm6 <- BinFeedingData.Licks(dfm6, binInterval)
bindfm7 <- BinFeedingData.Licks(dfm7, binInterval)
bindfm8 <- BinFeedingData.Licks(dfm8, binInterval)
bindfm9 <- BinFeedingData.Licks(dfm9, binInterval)
bindfm10 <- BinFeedingData.Licks(dfm10, binInterval)
bindfm11 <- BinFeedingData.Licks(dfm11, binInterval)
bindfm12 <- BinFeedingData.Licks(dfm12, binInterval)
bindfm13 <- BinFeedingData.Licks(dfm13, binInterval)
bindfm14 <- BinFeedingData.Licks(dfm14, binInterval)
bindfm15 <- BinFeedingData.Licks(dfm15, binInterval)
bindfm16 <- BinFeedingData.Licks(dfm16, binInterval)
bindfm17 <- BinFeedingData.Licks(dfm17, binInterval)
#--------Bin Data-------------

#------Save to Excel----------
# Change numeric values and file names below to correspond to data
BinFeedingData.Licks.SaveResults(dfm1, bindfm1, "1_19Sept2017_30minbin.xlsx", binInterval)
BinFeedingData.Licks.SaveResults(dfm2, bindfm2, "2_19Sept2017_30minbin.xlsx", binInterval)
BinFeedingData.Licks.SaveResults(dfm3, bindfm3, "3_19Sept2017_30minbin.xlsx", binInterval)
BinFeedingData.Licks.SaveResults(dfm4, bindfm4, "4_19Sept2017_30minbin.xlsx", binInterval)
BinFeedingData.Licks.SaveResults(dfm5, bindfm5, "5_19Sept2017_30minbin.xlsx", binInterval)
BinFeedingData.Licks.SaveResults(dfm6, bindfm6, "6_19Sept2017_30minbin.xlsx", binInterval)
BinFeedingData.Licks.SaveResults(dfm7, bindfm7, "7_19Sept2017_30minbin.xlsx", binInterval)
BinFeedingData.Licks.SaveResults(dfm8, bindfm8, "8_19Sept2017_30minbin.xlsx", binInterval)
BinFeedingData.Licks.SaveResults(dfm9, bindfm9, "9_19Sept2017_30minbin.xlsx", binInterval)
BinFeedingData.Licks.SaveResults(dfm10, bindfm10, "10_19Sept2017_30minbin.xlsx", binInterval)
BinFeedingData.Licks.SaveResults(dfm11, bindfm11, "11_19Sept2017_30minbin.xlsx", binInterval)
BinFeedingData.Licks.SaveResults(dfm12, bindfm12, "12_19Sept2017_30minbin.xlsx", binInterval)
BinFeedingData.Licks.SaveResults(dfm13, bindfm13, "13_19Sept2017_30minbin.xlsx", binInterval)
BinFeedingData.Licks.SaveResults(dfm14, bindfm14, "14_19Sept2017_30minbin.xlsx", binInterval)
BinFeedingData.Licks.SaveResults(dfm15, bindfm15, "15_19Sept2017_30minbin.xlsx", binInterval)
BinFeedingData.Licks.SaveResults(dfm16, bindfm16, "16_19Sept2017_30minbin.xlsx", binInterval)
BinFeedingData.Licks.SaveResults(dfm17, bindfm17, "17_19Sept2017_30minbin.xlsx", binInterval)
#-------Save to Excel----------

#------------Events------------
#calculate average event duration per bin
# binevents1<-BinFeedingData.Events(dfm1,binInterval)
# write.xlsx(binevents1,"EventExample1.xlsx")
# 
# binevents2<-BinFeedingData.Events(dfm2,binInterval)
# write.xlsx(binevents2,"EventExample2.xlsx")
# 
# binevents3<-BinFeedingData.Events(dfm3,binInterval)
# write.xlsx(binevents3,"EventExample3.xlsx")
#------------Events------------

### Step 2: Upload your files from excel
oct_dfm1 <- as.data.frame(read_excel("1_19Sept2017_30minbin.xlsx"))
oct_dfm2 <- as.data.frame(read_excel("2_19Sept2017_30minbin.xlsx"))
oct_dfm3 <- as.data.frame(read_excel("3_19Sept2017_30minbin.xlsx"))
oct_dfm4 <- as.data.frame(read_excel("4_19Sept2017_30minbin.xlsx"))
oct_dfm5 <- as.data.frame(read_excel("5_19Sept2017_30minbin.xlsx"))
oct_dfm6 <- as.data.frame(read_excel("6_19Sept2017_30minbin.xlsx"))
oct_dfm7 <- as.data.frame(read_excel("7_19Sept2017_30minbin.xlsx"))
oct_dfm8 <- as.data.frame(read_excel("8_19Sept2017_30minbin.xlsx"))
oct_dfm9 <- as.data.frame(read_excel("9_19Sept2017_30minbin.xlsx"))
oct_dfm10 <- as.data.frame(read_excel("10_19Sept2017_30minbin.xlsx"))
oct_dfm11 <- as.data.frame(read_excel("11_19Sept2017_30minbin.xlsx"))
oct_dfm12 <- as.data.frame(read_excel("12_19Sept2017_30minbin.xlsx"))
oct_dfm13 <- as.data.frame(read_excel("13_19Sept2017_30minbin.xlsx"))
oct_dfm14 <- as.data.frame(read_excel("14_19Sept2017_30minbin.xlsx"))
oct_dfm15 <- as.data.frame(read_excel("15_19Sept2017_30minbin.xlsx"))
oct_dfm16 <- as.data.frame(read_excel("16_19Sept2017_30minbin.xlsx"))
oct_dfm17 <- as.data.frame(read_excel("17_19Sept2017_30minbin.xlsx"))

### Step 3: Extract the fly feeding columns from each DFM
# (there are a few extra columns of data in the excel file;
# we are only interested in the W1-W12 columns which are 4:15)
# Depending on how you've assigned your flies you may have to play with this a bit
# Christina usually has odds on one diet and even on another, so an example of
# doing it like this is shown. If all your flies from one DFM are on a certain
# diet, you'll simply extract columns 4:15 (that's the W1-W12 columns)

#these variables just help me extract the columns I want quickly and easily
all<-c(4:15)
odds<-c(4,6,8,10,12,14)
evens<-c(5,7,9,11,13,15)
dfm11_exclusions <- c(4:14)
dfm13_exclusion <- c(4:11, 13:15)
dfm17_exclusion <- c(4, 6:15)
dfm7_exclusions <- c(4, 6:15)
dfm2_exclusions <- c(4:13)
dfm4_exclusions <- c(6:8, 10:15)
dfm6_exclusions <- c(4:12, 14, 15)
dfm14_exclusions <- c(5:15)

#for this example we'll pretend dfm1's even flies were on a 5% diet and odds 20%
oct_dfm1_evens <- oct_dfm1[evens]
oct_dfm1_odds <- oct_dfm1[odds]
#dfm2, all flies were on 5%
oct_dfm2_all <- oct_dfm2[all]
#dfm3, all flies were on 20%
oct_dfm3_all <- oct_dfm3[all]

#ND sated
dfm6_all <- oct_dfm6[dfm6_exclusions]
#ND Starved
dfm17_all <- oct_dfm17[dfm17_exclusion]
#HSD sated
dfm1_all <- oct_dfm1[all]
dfm2_all <- oct_dfm2[dfm2_exclusions]
#HSD starved
dfm3_all <- oct_dfm3[all]
dfm4_all <- oct_dfm4[dfm4_exclusions]
#ND 50 mM D-glucose
dfm14_all <- oct_dfm14[dfm14_exclusions]
#HSD 50 mM D-glucose
dfm10_all <- oct_dfm10[all]
dfm11_all <- oct_dfm11[dfm11_exclusions]
#ND 400 mM D-glucose
dfm8_all <- oct_dfm8[all]
#HSD 400 mM D-glucose
dfm12_all <- oct_dfm12[all]
dfm13_all <- oct_dfm13[dfm13_exclusion]
#ND 1 M D-glucose
dfm7_all <- oct_dfm7[dfm7_exclusions]
#HSD 1 M D-glucose
dfm15_all <- oct_dfm15[all]
dfm16_all <- oct_dfm16[all]


### Step 4: "glue" together your flies that are on the same diet and/or genetic path
#remember this is our 5% flies
oct_5p <- cbind(oct_dfm1_evens, oct_dfm2_all)
#20% flies
oct_20p <- cbind(oct_dfm1_odds, oct_dfm3_all)

#HSD Sated
HSD_sated <- cbind(dfm1_all, dfm2_all)
#HSD Starved
HSD_starved <- cbind(dfm3_all, dfm4_all)
#HSD 50 mM D-glucose
HSD_50mMG <- cbind(dfm10_all, dfm11_all)
#HSD 400 mM D-glucose
HSD_400mMG <- cbind(dfm12_all, dfm13_all)
#HSD 1 M D-glucose
HSD_1MG <- cbind(dfm15_all, oct_dfm16_all)


### Step 4: MELT your data
# These functions all "melt" your data down into long format
# Check out melt() online if you haven't used it before, but basically
    # we want columns for ggplot to be able to easily see what it needs to plot
# So you'll have all of the values for dfm1, fly 1 in one column for all time points
    # Then followed by all of dfm1, fly 2 then dfm1, fly3...... and so on
# This will allow us to tell ggplot to put all the flies on the Y axis,
# Time on the X axis (another column), and then group by diet, day, etc
    # whatever the thing is should get its own column

###################
# IMPORTANT!!!
###################
# Make sure you go into your excel file and find the closest time to midnight of day 1
# and use it as your row start parameter. In this data you can see that row 17
# is time 23:54, so we want to run our data through this function starting
# at row 16 (this corresponds to row 17 in excel)

# 4A - calling this function will keep all of your flies and just prep your data for ggplot
# Unless you have a genetically modified fly just ignore the 3rd parameter
oct_5p_melt <- tidyFlies(oct_5p, 5, NA, 16)
oct_20p_melt <- tidyFlies(oct_20p, 20, NA, 16)

oct_50mMG_melt <- tidyFlies(oct_50mMG, "50 mM D-Glucose", NA, 30)
oct_400mMG_melt <- tidyFlies(oct_400mMG, "400 mM D-Glucose", NA, 30)
oct_1MG_melt <- tidyFlies(oct_1MG, "1 M D-Glucose", NA, 30)
oct_5p_sated_melt <- tidyFlies(oct_5p_sated, "5% Sucrose Sated", NA, 30)
oct_5p_starved_melt <- tidyFlies(oct_5p_starved, "5% Sucrose Starved", NA, 30)

# 4B - use if you want to throw out flies that have value of 0 the entire experiment
oct_5p_rm_melt <- tidyDeadFlies(oct_5p, 5, NA, 16)
oct_20p_rm_melt <- tidyDeadFlies(oct_20p, 20, NA, 16)

# 4C - use if you want to throw out flies that have value of 0 the entire experiment
# AND make flies that register 0 values for the rest of the experiement NA
# This helps so that dead flies don't drag down your means
oct_5p_rm2_melt <- removeDeadEscaped(oct_5p, 5, NA, 16)
oct_20p_rm2_melt <- removeDeadEscaped(oct_20p, 20, NA, 16)


ND_Sated_rm2 <- removeDeadEscaped(dfm6_all, "Sated ND", NA, 22)
ND_Starved_rm2 <- removeDeadEscaped(dfm17_all, "Starved ND", NA, 22)
ND_50mMG_rm2 <- removeDeadEscaped(dfm14_all, "50 mM D-Glucose ND", NA, 22)
ND_400mMG_rm2 <- removeDeadEscaped(dfm8_all, "400 mM D-Glucose ND", NA, 22)
ND_1MG_rm2 <- removeDeadEscaped(dfm7_all, "1 M D-Glucose ND", NA, 22)
HSD_sated_rm2 <- removeDeadEscaped(HSD_sated, "Sated HSD", NA, 22)
HSD_starved_rm2 <- removeDeadEscaped(HSD_starved, "Starved HSD", NA, 22)
HSD_50mMG_rm2 <- removeDeadEscaped(HSD_50mMG, "50 mM D-Glucose HSD", NA, 22)
HSD_400mMG_rm2 <- removeDeadEscaped(HSD_400mMG, "400 mM D-Glucose HSD", NA, 22)
HSD_1MG_rm2 <- removeDeadEscaped(HSD_1MG, "1 M D-Glucose HSD", NA, 22)


### Step 5: Combine your nice clean (long) data frames into one big (long) one that you
# can call easily for your various plots

#Using results from 4A
oct_all <- rbind(oct_5p_melt, oct_20p_melt)
#4B
oct_rm_all <- rbind(oct_5p_rm_melt, oct_20p_rm_melt)
#4C
rm2_all <- rbind(ND_Sated_rm2, ND_Starved_rm2, ND_50mMG_rm2, ND_400mMG_rm2, ND_1MG_rm2, HSD_sated_rm2, HSD_starved_rm2, HSD_50mMG_rm2, HSD_400mMG_rm2, HSD_1MG_rm2)
rm2_glucose <- rbind(ND_50mMG_rm2, ND_400mMG_rm2, ND_1MG_rm2, HSD_50mMG_rm2, HSD_400mMG_rm2, HSD_1MG_rm2)
rm2_sated <- rbind(ND_Sated_rm2, HSD_sated_rm2)
rm2_starved <- rbind(ND_Starved_rm2, HSD_starved_rm2)



#you have to add this to make your time continuous (ultimately makes plots nicer)
oct_all$variable<-as.numeric(as.character(oct_all$variable))
oct_rm_all$variable<-as.numeric(as.character(oct_rm_all$variable))

rm2_all$variable<-as.numeric(as.character(rm2_all$variable))
rm2_glucose$variable<-as.numeric(as.character(rm2_glucose$variable))
rm2_sated$variable <- as.numeric(as.character(rm2_sated$variable))
rm2_starved$variable <- as.numeric(as.character(rm2_starved$variable))

### Step 6: Make your plots!
# You've got a few different options here as far as filtering out dead/escaped flies goes
# Try switching out your oct_all data frame for oct_rm2_all and oct_rm_all to see differences

# Set a new directory where you want the plots to go
dir.create("./Plots")
setwd("./Plots")

# To look at all your data for all your days:
##Monica doesn't like this one
pdf("All Diets Days.pdf",width=7,height=7)
ggplot(rm2_all, aes(Time,value,col=factor(Diet)))+
  stat_summary(fun.y = mean,geom="line",size=0.5)+
  stat_summary(fun.y = mean,geom="point",size=0.5)+
  #change fun.data = mean_se to fun.data=mean_sdl to get standard dev
  stat_summary(fun.data = mean_se, geom="errorbar", alpha= 0.5)+
  ggtitle("August Feeding +/- SEM")+
  ylab("Mean Licks")+
  guides(col=guide_legend(title="Diet"),fill=F)+
  facet_wrap(~ factor(Day),scales="free")+
  theme_bw()
dev.off()

#To get individual graphs:
for (i in unique(oct_rm2_all$Day)) {
  ggsave(filename=paste("Day",i,"AugustFeeding_SEM.pdf"), plot=ggplot(data=oct_rm2_all[oct_rm2_all$Day == i,], mapping = aes(Time,value,col=factor(Diet)))+
    stat_summary(fun.y = mean,geom="line",size=0.5)+
    stat_summary(fun.y = mean,geom="point",size=0.5)+
    #change fun.data = mean_se to fun.data=mean_sdl to get standard dev
    stat_summary(fun.data = mean_se, geom="errorbar", alpha= 0.5)+
    ggtitle("August Feeding +/- SEM")+
    ylab("Mean Licks")+
    #Changes the legend
    guides(col=guide_legend(title="Diet"),fill=F)+
    theme_bw(),
    #change your size of your saved graphs
    units="in", width=5, height=4)
}

### Purple comparison plot code ####
pdf("Glucose Diets 2 PM Purple Plot.pdf", height=3,width=10) # height/weight in inches
#Using CD_full_samp2 because that contains all the flies, just specific days
ggplot(rm2_all, aes(Time,value,col=factor(Diet)))+
  # if you want the lines connecting the points, uncomment this
  stat_summary(fun.y = mean,geom="line",size=0.5)+
  #stat_summary(fun.y = mean,geom="point",size=0.5)+ #can change the size of the points
  scale_color_manual(values=c("#E4ABFA","#8A08BE","#aad3f9","#174c7c","#7c1636","#382229","#97e5ad","#468758","#2a00ff","#36258c"))+ 
  #light purple, dark purple, light blue, dark blue, maroon, dark maroon, pastel green, green, light royal blue, royal blue
  #stat_summary(fun.data = mean_se, geom="errorbar", alpha= 0.5)+ #adds the standard error mean
  # if you want standard deviation run this instead:
  # stat_summary(fun.data = mean_sdl, geom="errorbar", alpha= 0.5) +
  ggtitle("Diets Midnight ± SEM")+
  labs(y="Mean Licks", x="Time (Hours)", title="Glucose Diets Starting at 2 PM ± SEM", col = "Diet")+
  facet_grid(~factor(Day),scales="free",space="free")+
  theme_classic() #change to theme_bw() or something else if you want the grid lines back
dev.off() #ends the creation of the pdf

### RASTER code ###
pdf("All Diets Starting at Midnight Raster1.pdf",width=11,height=9)
ggplot(rm2_all, aes(Time, variable, fill = value)) +
  geom_tile() +
  facet_grid(Diet ~ Day, scales = "free", labeller = labeller(Diet = label_wrap_gen(8))) +  #Add space = "free" if you want y scales to automatically adjust to n
  ylab("Fly") +
  xlab("Time (Hours)") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradientn(
    colours = grad,
    values = rescale / 1200,
    limits = c(0, 1200),
    name = "Licks"
  ) +
  ggtitle("All Diets Starting at Midnight Raster") +
  guides(fill = guide_colorbar(
    barwidth = 1,
    barheight = 10,
    nbin = 100
  )) +
  theme(strip.text.y = element_text(size=9))
dev.off()


