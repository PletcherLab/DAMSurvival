####################
# Tidying functions for raster plots
# K. Hoffman
# Last update January 21 2017
#R version 3.4.2 (2017-09-28) -- "Short Summer"
#Copyright (C) 2017 The R Foundation for Statistical Computing
#Platform: x86_64-w64-mingw32/x64 (64-bit)
####################


#Simplest function - just tidies up your data for ggplot
#Does not remove any flies
tidyFlies <- function(df, diet, pathway, midnight_row) {
  df1 <- df[1:(midnight_row-1), ]
  df2 <- df[midnight_row:nrow(df), ]
  colnames(df1) <- c(1:ncol(df))
  colnames(df2) <- c(1:ncol(df))
  df1$Time <- rev(seq(23.5, by = -0.5, length.out = midnight_row-1))
  df1$Day <- 0
  df2$Time <- rep(seq(0, 23.5, by = 0.5), len = nrow(df2))
  df2$Day <- rep(1:15, each = 48, len = nrow(df2))
  newdf <- rbind(df1, df2)
  newdf$Diet <- rep(diet, nrow(df))
  newdf$Pathway <- rep(paste(pathway))
  df_melt <- melt(newdf, id = c("Time", "Day", "Diet", "Pathway"))
  return(df_melt)
}


#Slightly more complex function - throws out any flies that have 0 values from the start
# assume they were escaped flies
tidyDeadFlies <- function(df, diet, pathway, midnight_row) {
  df1 <- df[1:(midnight_row-1), ]
  df2 <- df[midnight_row:nrow(df), ]
  colnames(df1) <- c(1:ncol(df))
  colnames(df2) <- c(1:ncol(df))
  df1$Time <- rev(seq(23.5, by = -0.5, length.out = midnight_row-1))
  df1$Day <- 0
  df2$Time <- rep(seq(0, 23.5, by = 0.5), len = nrow(df2))
  df2$Day <- rep(1:15, each = 48, len = nrow(df2))
  newdf <- rbind(df1, df2)
  #get rid of flies that are dead the whole time
  newdf <- newdf[colSums(newdf, na.rm = T) != 0]
  newdf$Diet <- rep(diet, nrow(newdf))
  newdf$Pathway <- rep(paste(pathway))
  df_melt <- melt(newdf, id = c("Time", "Day", "Diet", "Pathway"))
  return(df_melt)
}



#REMOVE DEAD AND ESCAPED FLIES
#Make flies that are dead NA once they die (register as 0 for rest of data)....
removeDeadEscaped <- function(df, diet, pathway, midnight_row) {
  df1 <- df[1:(midnight_row-1), ]
  df2 <- df[midnight_row:nrow(df), ]
  colnames(df1) <- c(1:ncol(df))
  colnames(df2) <- c(1:ncol(df))
  df1$Time <- rev(seq(23.5, by = -0.5, length.out = midnight_row-1))
  df1$Day <- 0
  df2$Time <- rep(seq(0, 23.5, by = 0.5), len = nrow(df2))
  df2$Day <- rep(1:15, each = 48, len = nrow(df2))
  newdf <- rbind(df1, df2)
  #removes flies that are 0 value from start-finish (escapees)
  newdf <- newdf[colSums(newdf, na.rm = T) != 0]
  #makes flies NA as soon as they have all 0 values
  for (j in 1:ncol(newdf)) {
    for (i in 1:nrow(newdf)) {
      #print(paste(i, j))
      #print(sum(df[i:nrow(df), j]))
      if (sum(newdf[i:nrow(newdf), j], na.rm = T) == 0) {
        newdf[i:nrow(newdf), j] <- NA
      }
    }
  }
  newdf$Diet <- rep(diet, nrow(df))
  newdf$Pathway <- rep(paste(pathway))
  df_melt <- melt(newdf, id = c("Time", "Day", "Diet", "Pathway"))
  return(df_melt)
}




#Code for raster colors

grad <- c(
  "white",
  "#9A00B8",
  #purple
  "#5C00BB",
  #purple-blue
  "#1B00BE",
  #dark blue
  "#0026C1",
  #med blue
  "#006BC4",
  #light blue
  "#00B2C8",
  #turquoise
  "#00CB9B",
  #seagreen
  "#00D10F",
  #bright green
  "#85D800",
  #yellowgreen
  "#F0FF0F",
  #yellow
  "#FFE147",
  "#FCC509",
  "#FCA909",
  #orange yellow
  "#F68C07",
  #"#F07106",
  #"#EA5604",
  "#E43C03",
  "#DE2301",
  "#D60000",
  #red
  #"#980000", #darker reds
  "#890000",
  #"#6B0000",
  "#5B0000",
  "black"
)

breaks <-
  c(
    0,
    0.01,
    0.02,
    0.03,
    0.04,
    0.05,
    0.06,
    0.08,
    0.1,
    0.12,
    0.14,
    0.16,
    0.22,
    0.3,
    0.4,
    0.5,
    0.6,
    0.7,
    0.8,
    0.9,
    1
  )
#Creates vector in order to eliminate excess labels from axes
rescale <- breaks * 2000
rescale[20] <- 2000
rescale[21] <- 3000

