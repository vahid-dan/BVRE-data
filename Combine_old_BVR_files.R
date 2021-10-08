# This script is to combine the old BVR dataframe before the turbidity sensors was added and then after it was added 
# And add the current BVR file
# By Adrienne Breef-Pilz
# 08 OCT 21

library(tidyverse)
library(lubridate)
library(dplyr)

#to download the files needed
download.file("https://github.com/CareyLabVT/ManualDownloadsSCCData/master/BVRplatform_manual_2020.csv", "BVRplatform_manual_2020.csv")
download.file('https://github.com/FLARE-forecast/BVRE-data/raw/bvre-platform-data/bvre-waterquality.csv', "bvre-waterquality.csv") 
download.file('https://github.com/FLARE-forecast/BVRE-data/raw/bvre-platform-data/BVRplatform.csv', "BVRplatform.csv.csv")
download.file("https://raw.githubusercontent.com/CareyLabVT/ManualDownloadsSCCData/master/BVRPlatform/CR6Series_BVRplatform_bvre-waterquality_20210531.dat", "CR6Series_BVRplatform_bvre-waterquality_20210531.dat" )
download.file("https://raw.githubusercontent.com/CareyLabVT/ManualDownloadsSCCData/master/BVRPlatform/CR6Series_BVRplatform_bvre-waterquality_20210726.dat", "CR6Series_BVRplatform_bvre-waterquality_20210726.dat" )
download.file("https://raw.githubusercontent.com/CareyLabVT/ManualDownloadsSCCData/master/BVRPlatform/CR6Series_BVRplatform_BVRplatform_20210416.dat", "CR6Series_BVRplatform_BVRplatform_20210416.dat")
download.file("https://raw.githubusercontent.com/CareyLabVT/ManualDownloadsSCCData/master/BVRPlatform/CR6Series_BVRplatform_BVRplatform_20210610.dat", "CR6Series_BVRplatform_BVRplatform_20210610.dat" )

bvrheader1<-read.csv("BVRplatform.csv", skip=1, as.is=T) #get header minus wonky Campbell rows
bvrdata1<-read.csv("BVRplatform.csv", skip=4, header=F) #get data minus wonky Campbell rows
names(bvrdata1)<-names(bvrheader1) 

bvrdata1=bvrdata1%>%
  filter(TIMESTAMP!="")%>%
  filter(TIMESTAMP< "2021-04-05 13:21:00") #filter for dates when we added a new Turbidity but didn't add a new column


bvrd<-read.csv("BVRplatform_manual_2020.csv") #read in manual data before gateway set up

bvrdata= bvrd%>%
  filter(TIMESTAMP!="")%>%#take out the rows with blank timestamp and Record
  select(!X)%>%#delete column that was added when uploaded
  rbind(.,bvrdata1)%>%
  distinct(TIMESTAMP, .keep_all= TRUE) #taking out the duplicate values 
   

mydir = c("CR6Series_BVRplatform_bvre-waterquality_20210531.dat",
          "CR6Series_BVRplatform_bvre-waterquality_20210726.dat",
          "CR6Series_BVRplatform_BVRplatform_20210416.dat",
          "CR6Series_BVRplatform_BVRplatform_20210610.dat")

myfiles = list.files(path=mydir, pattern="", full.names=TRUE)#list the files from BVR platform

#taking out the the Temp Test files
#myfilesBVR <- myfiles[ !grepl("CR6Series_BVRplatform_bvre-waterquality_20210416*", myfiles) ]#exclude the Temp test data

#create dataframe for the for loop
bvrdata3<-""

#combine all of the files into one data sheet, have to come back and fix this loop
for(k in 1:length(myfiles)){
  bvrheader2<-read.csv(myfiles[k], skip=1, as.is=T) #get header minus wonky Campbell rows
  bvrdata2<-read.csv(myfiles[k], skip=4, header=F) #get data minus wonky Campbell rows
  names(bvrdata2)<-names(bvrheader2) #combine the names to deal with Campbell logger formatting
  bvrdata3=rbind(bvrdata2, bvrdata3)
}

#current BVR data
bvrheader5<-read.csv("bvre-waterquality.csv", skip=1, as.is=T) #get header minus wonky Campbell rows
bvrdata5<-read.csv("bvre-waterquality.csv", skip=4, header=F) #get data minus wonky Campbell rows
names(bvrdata5)<-names(bvrheader5) 


#bind all of the files with turbidity together
bvrdata5=rbind(bvrdata3,bvrdata5)

bvrdata5= bvrdata5%>%
  rbind(.,bvrdata3)%>%
  filter(TIMESTAMP!="")%>%#take out the rows with blank timestamp and Record
  distinct(TIMESTAMP, .keep_all= TRUE)


bvrdata5$TIMESTAMP<-as.POSIXct(strptime(bvrdata5$TIMESTAMP, "%Y-%m-%d %H:%M:%S"), tz = "Etc/GMT+4")
bvrdata$TIMESTAMP<-as.POSIXct(strptime(bvrdata$TIMESTAMP, "%Y-%m-%d %H:%M:%S"), tz = "Etc/GMT+4")

bvrdata_new=merge(bvrdata5,bvrdata, all=T)



#change the columns from character to numeric and gets rid of quotes
bvrdata_new$RECORD=as.numeric(as.character(bvrdata_new$RECORD))
bvrdata_new$BattV=as.numeric(as.character(bvrdata_new$BattV))
bvrdata_new$PTemp_C=as.numeric(as.character(bvrdata_new$PTemp_C))
bvrdata_new$wtr_1=as.numeric(as.character(bvrdata_new$wtr_1))
bvrdata_new$wtr_2=as.numeric(as.character(bvrdata_new$wtr_2))
bvrdata_new$wtr_3=as.numeric(as.character(bvrdata_new$wtr_3))
bvrdata_new$wtr_4=as.numeric(as.character(bvrdata_new$wtr_4))
bvrdata_new$wtr_5=as.numeric(as.character(bvrdata_new$wtr_5))
bvrdata_new$wtr_6=as.numeric(as.character(bvrdata_new$wtr_6))
bvrdata_new$wtr_7=as.numeric(as.character(bvrdata_new$wtr_7))
bvrdata_new$wtr_8=as.numeric(as.character(bvrdata_new$wtr_8))
bvrdata_new$wtr_9=as.numeric(as.character(bvrdata_new$wtr_9))
bvrdata_new$wtr_10=as.numeric(as.character(bvrdata_new$wtr_10))
bvrdata_new$wtr_11=as.numeric(as.character(bvrdata_new$wtr_11))
bvrdata_new$wtr_12=as.numeric(as.character(bvrdata_new$wtr_12))
bvrdata_new$wtr_13=as.numeric(as.character(bvrdata_new$wtr_13))
bvrdata_new$doobs_6=as.numeric(as.character(bvrdata_new$doobs_6))
bvrdata_new$dosat_6=as.numeric(as.character(bvrdata_new$dosat_6))
bvrdata_new$dotemp_6=as.numeric(as.character(bvrdata_new$dotemp_6))
bvrdata_new$doobs_13=as.numeric(as.character(bvrdata_new$doobs_13))
bvrdata_new$dosat_13=as.numeric(as.character(bvrdata_new$dosat_13))
bvrdata_new$dotemp_13=as.numeric(as.character(bvrdata_new$dotemp_13))
bvrdata_new$EXO_Date=as.numeric(as.character(bvrdata_new$EXO_Date))
bvrdata_new$EXO_Time=as.numeric(as.character(bvrdata_new$EXO_Time))
bvrdata_new$EXO_wtr_1=as.numeric(as.character(bvrdata_new$EXO_wtr_1))
bvrdata_new$Cond_1=as.numeric(as.character(bvrdata_new$Cond_1))
bvrdata_new$SpCond_1=as.numeric(as.character(bvrdata_new$SpCond_1))
bvrdata_new$TDS_1=as.numeric(as.character(bvrdata_new$TDS_1))
bvrdata_new$dosat_1=as.numeric(as.character(bvrdata_new$dosat_1))
bvrdata_new$doobs_1=as.numeric(as.character(bvrdata_new$doobs_1))
bvrdata_new$Chla_RFU_1=as.numeric(as.character(bvrdata_new$Chla_RFU_1))
bvrdata_new$Chla_1=as.numeric(as.character(bvrdata_new$Chla_1))
bvrdata_new$BGAPC_RFU_1=as.numeric(as.character(bvrdata_new$BGAPC_RFU_1))
bvrdata_new$BGAPC_1=as.numeric(as.character(bvrdata_new$BGAPC_1))
bvrdata_new$fDOM_RFU_1=as.numeric(as.character(bvrdata_new$fDOM_RFU_1))
bvrdata_new$fDOM_QSU_1=as.numeric(as.character(bvrdata_new$fDOM_QSU_1))
bvrdata_new$Turbidity_FNU_1=as.numeric(as.character(bvrdata_new$Turbidity_FNU_1))
bvrdata_new$TSS_mgL_1=as.numeric(as.character(bvrdata_new$TSS_mgL_1))
bvrdata_new$EXO_pressure=as.numeric(as.character(bvrdata_new$EXO_pressure))
bvrdata_new$EXO_depth=as.numeric(as.character(bvrdata_new$EXO_depth))
bvrdata_new$EXO_battery=as.numeric(as.character(bvrdata_new$EXO_battery))
bvrdata_new$EXO_cablepower=as.numeric(as.character(bvrdata_new$EXO_cablepower))
bvrdata_new$EXO_wiper=as.numeric(as.character(bvrdata_new$EXO_wiper))
bvrdata_new$Lvl_psi=as.numeric(as.character(bvrdata_new$Lvl_psi))
bvrdata_new$wtr_pt_13=as.numeric(as.character(bvrdata_new$wtr_pt_13))


bvrdata_new$TIMESTAMP=as.character(bvrdata_new$TIMESTAMP)

#Reorder the columns so turbidity is after fdom and before EXO pressure
bvrdata_new=bvrdata_new%>%
  relocate(c(Turbidity_FNU_1,TSS_mgL_1), .after=fDOM_QSU_1)%>%
  filter(TIMESTAMP!="")

#write as a new file 
write.csv(bvrdata_new, "bvrewaterquality.csv", row.names = F)
