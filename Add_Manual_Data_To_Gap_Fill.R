# Updating the BVR data when there are gaps in the streaming data. Use the manual data and take out repeats
# Not necessary to use for forecasting BVR just for ABP to fix gaps and then push fixed file to GitHub
# BY ABP 30 MAr 22
# Manual files are stored on the ManualDownloadsSCCData and collated there

library(tidyverse)
library(lubridate)
library(dplyr)

#to download the files needed
download.file("https://raw.githubusercontent.com/CareyLabVT/ManualDownloadsSCCData/master/BVRPlatform/BVR_manual_2022.csv", "BVRplatform_manual_2022.csv")

# Read in files
bvrd<-read.csv("BVRplatform_manual_2022.csv") #read in manual data

#current BVR data
bvrheader5<-read.csv("bvre-waterquality.csv", skip=0, as.is=T)


bvr= bvrheader5%>%
  rbind(.,bvrd)%>%
  filter(TIMESTAMP!="")%>%#take out the rows with blank timestamp and Record
  distinct(TIMESTAMP, .keep_all= TRUE)

#reorder
bvr=bvr[order(bvr$TIMESTAMP),]

bvrdata_new=bvr

#change the columns from character to numeric and gets rid of quotes
#change the columns from as.character to as.numeric after the merge
bvrdata_new[, c(2:46)] <- sapply(bvrdata_new[, c(2:46)], as.numeric)

# Change NAs to "NAN" in notepad so compatiable with the format on the data logger

#write as a new file
write.csv(bvrdata_new, "bvre-waterquality.csv", row.names = F)
