

#script written to create daily figures that are sent to lucky SCC team members :)
#written by CCC, BB, Vahid-Dan, ABP

if (!"lubridate" %in% installed.packages()) install.packages("lubridate")
library(lubridate)


download.file('https://github.com/FLARE-forecast/BVRE-data/blob/bvre-platform-data/BVRplatform.csv','BVRplatform.csv')
download.file('https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data/bvre-waterquality.csv', "bvre-waterquality.csv") 
download.file("https://raw.githubusercontent.com/CareyLabVT/ManualDownloadsSCCData/master/BVRPlatform/CR6Series_BVRplatform_BVRplatform_20210416.csv", "CR6Series_BVRplatform_BVRplatform_20210416.csv")



#time to now play with BVR data!
bvrheader<-read.csv("BVRplatform.csv", skip=1, as.is=T) #get header minus wonky Campbell rows
bvrdata<-read.csv("BVRplatform.csv", skip=4, header=F) #get data minus wonky Campbell rows
names(bvrdata)<-names(bvrheader) #combine the names to deal with Campbell logger formatting

bvrheader1<-read.csv("bvre-waterquality.csv", skip=1, as.is=T) #get header minus wonky Campbell rows
bvrdata1<-read.csv("bvre-waterquality.csv", skip=4, header=F) #get data minus wonky Campbell rows
names(bvrdata1)<-names(bvrheader1) #combine the names to deal with Campbell logger formatting

bvrheader2<-read.csv("Missing_BVR_2021.csv", skip=1, as.is=T) #get header minus wonky Campbell rows
bvrdata2<-read.csv("Missing_BVR_2021.csv", skip=4, header=F) #get data minus wonky Campbell rows
names(bvrdata2)<-names(bvrheader2) #combine the names to deal with Campbell logger formatting

bvr3=merge(bvrdata2,bvrdata1, all=T)

#Removes row if the TIMESTAMP column is blank
bvrdata <- bvrdata[complete.cases(bvrdata$TIMESTAMP),]

#Removes row if the RECORD column has an NA or blank
bvrdata=bvrdata[!(is.na(bvrdata$RECORD) | bvrdata$RECORD==""), ]

end.time1 <- with_tz(as.POSIXct(strptime(Sys.time(), format = "%Y-%m-%d %H")), tzone = "Etc/GMT+5") #gives us current time with rounded hours in EDT
start.time1 <- end.time1 - days(7) #to give us seven days of data for looking at changes
full_time1 <- as.data.frame(seq(start.time1, end.time1, by = "10 min")) #create sequence of dates from past 7 days to fill in data for a data frame
colnames(full_time1)=c("TIMESTAMP") #make it a data frame to merge to make obs1 later

#obs3 <- array(NA,dim=c(length(full_time1),44)) #create array that will be filled in with 44 columns (the entire size of the array)
bvr3$TIMESTAMP<-as.POSIXct(strptime(bvr3$TIMESTAMP, "%Y-%m-%d %H:%M"), tz = "Etc/GMT+5") #get dates aligned


if (length(na.omit(bvrdata$TIMESTAMP[bvrdata$TIMESTAMP>start.time1]))==0) { #if there is no data after start time, then a pdf will be made explaining this
  pdf(paste0("BVRDataFigures_", Sys.Date(), ".pdf"), width=8.5, height=11) #call PDF file
  plot(NA, xlim=c(0,5), ylim=c(0,5), bty='n',xaxt='n', yaxt='n', xlab='', ylab='') #creates empty plot
  mtext(paste("No data found between", start.time1, "and", end.time1, sep = " ")) #fills in text in top margin of plot
  dev.off() #file made!
} else {
  #for(i in 1:length(full_time3)){ #this loop looks for matching dates and extracts data from metdata file to obs array
  #index = which(bvrdata$TIMESTAMP==full_time3[i])
  #if(length(index)>0){
  #obs3[i,] <- unlist(bvrdata[index,c(1:44)])
  # }
  #}
  obs3=merge(full_time1,bvrdata, all.x=TRUE)#merge the data frame to get the last 7 days
  #obs3<-as.data.frame(obs3) #make into DF
  #obs3[,1] <- full_time3 #now have your array with a proper timedate stamp!
  #colnames(obs3)<-names(bvrdata[index,c(1:44)]) #get column names
  
  
  
  pdf(paste0("BVRDataFigures_", Sys.Date(), ".pdf"), width=8.5, height=11) #call PDF file
  par(mfrow=c(3,2))
  
  plot(obs3$TIMESTAMP,obs3$RECORD, main="Campbell Logger Record", xlab="Time", ylab="Number", type='l')
  plot(obs3$TIMESTAMP,obs3$BattV, main="Campbell Logger Battery", xlab="Time", ylab="Volts", type='l')
  if(min(tail(na.omit(obs3$BattV)))<11.5){
    mtext("Battery Charge Low", side = 3, col="red")
  }
  plot(obs3$TIMESTAMP,obs3$EXO_battery, main="EXO Battery", xlab="Time", ylab="Volts", type='l')
  plot(obs3$TIMESTAMP,obs3$EXO_cablepower, main="EXO Cable Power", xlab="Time", ylab="Volts", type='l')
  plot(obs3$TIMESTAMP,obs3$EXO_depth, main="EXO Depth", xlab="Time", ylab="Meters", type='l')
  
  plot(obs3$TIMESTAMP,obs3$EXO_pressure, main="Sonde Pressure", xlab="Time", ylab="psi", type='l', ylim=c(-1,22))
  points(obs3$TIMESTAMP, obs3$Lvl_psi, col="blue4", type='l')
  legend("topleft", c("1.5m EXO", "11m PT"), text.col=c("black", "blue4"), x.intersp=0.001)
  
  par(mar=c(5.1, 4.1, 4.1, 2.1), mgp=c(3, 1, 0), las=0)
  plot(obs3$TIMESTAMP,obs3$dotemp_13, main="Water temp of sondes", xlab="Time", ylab="degrees C", type='l', col="medium sea green", lwd=1.5, ylim=c(0,45))
  points(obs3$TIMESTAMP, obs3$dotemp_6, col="black", type='l', lwd=1.5)
  points(obs3$TIMESTAMP, obs3$EXO_wtr_1, col="magenta", type='l', lwd=1.5)
  points(obs3$TIMESTAMP, obs3$wtr_pt_13, col="blue4", type='l', lwd=1.5)
  legend("topleft", c("1.5m EXO", "4m DO", "11m DO", "11m PT"), text.col=c("magenta", "black", "medium sea green", "blue4"), x.intersp=0.001)
  
  plot(obs3$TIMESTAMP,obs3$doobs_13, main="DO", xlab="Time", ylab="mg/L", type='l', col="medium sea green", lwd=1.5, ylim=c(-0.5,15))
  points(obs3$TIMESTAMP, obs3$doobs_6, col="black", type='l', lwd=1.5)
  points(obs3$TIMESTAMP, obs3$doobs_1, col="magenta", type='l', lwd=1.5)
  legend("topleft", c("1.5m", "4m", "11m"), text.col=c("magenta", "black", "medium sea green"), x.intersp=0.001)
  
  plot(obs3$TIMESTAMP,obs3$dosat_13, main="DO % saturation", xlab="Time", ylab="% saturation", type='l', col="medium sea green", lwd=1.5, ylim=c(-0.5,200))
  points(obs3$TIMESTAMP, obs3$dosat_6, col="black", type='l', lwd=1.5)
  points(obs3$TIMESTAMP, obs3$dosat_1, col="magenta", type='l', lwd=1.5)
  legend("topleft", c("1.5m", "4m", "11m"), text.col=c("magenta", "black", "medium sea green"), x.intersp=0.001)
  
  plot(obs3$TIMESTAMP,obs3$Cond_1, main="Cond, SpCond, TDS @ 1.5m", xlab="Time", ylab="uS/cm or mg/L", type='l', col="red", lwd=1.5, ylim=c(-0.5,45))
  points(obs3$TIMESTAMP, obs3$SpCond_1, col="black", type='l', lwd=1.5)
  points(obs3$TIMESTAMP, obs3$TDS_1, col="orange", type='l', lwd=1.5)
  points(obs3$TIMESTAMP, obs3$Turbidity_FNU_1, col="brown", type='l', lwd=1.5)
  legend("topleft", c("TDS", "SpCond", "Cond", "Turbidity"), text.col=c("orange", "black","red", "brown"), x.intersp=0.001)
  
  plot(obs3$TIMESTAMP,obs3$Chla_1, main="Chla, Phyco, fDOM", xlab="Time", ylab="ug/L or QSU", type='l', col="green", lwd=1.5, ylim=c(-0.5,30))
  points(obs3$TIMESTAMP, obs3$BGAPC_1, col="blue", type='l', lwd=1.5)
  points(obs3$TIMESTAMP, obs3$fDOM_QSU_1, col="firebrick4", type='l', lwd=1.5)
  legend("topleft", c("Chla", "Phyco", "fDOM"), text.col=c("green", "blue", "firebrick4"), x.intersp=0.001)
  
  par(mfrow=c(1,1))
  par(oma=c(1,1,1,4))
  plot(bvr3$TIMESTAMP,bvr3$wtr_3, main="Water Temp", xlab="Time", ylab="degrees C", type='l', col="DarkOrange1", lwd=1.5, ylim=c(0,40))
  points(bvr3$TIMESTAMP, bvr3$wtr_4, col="gold", type='l', lwd=1.5)
  points(bvr3$TIMESTAMP, bvr3$wtr_5, col="greenyellow", type='l', lwd=1.5)
  points(bvr3$TIMESTAMP, bvr3$wtr_6, col="medium sea green", type='l', lwd=1.5)
  points(bvr3$TIMESTAMP, bvr3$wtr_7, col="sea green", type='l', lwd=1.5)
  points(bvr3$TIMESTAMP, bvr3$wtr_8, col="DeepSkyBlue4", type='l', lwd=1.5)
  points(bvr3$TIMESTAMP, bvr3$wtr_9, col="blue2", type='l', lwd=1.5)
  points(bvr3$TIMESTAMP, bvr3$wtr_10, col="blue4", type='l', lwd=1.5)
  points(bvr3$TIMESTAMP, bvr3$wtr_11, col="darkslateblue", type='l', lwd=1.5)
  points(bvr3$TIMESTAMP, bvr3$wtr_12, col="magenta2", type='l', lwd=1.5)
  points(bvr3$TIMESTAMP, bvr3$wtr_13, col="darkmagenta", type='l', lwd=1.5)
  
  #subset if there are missing pressure values
  obs3_sub=obs3[!(is.na(obs3$Lvl_psi) | obs3$Lvl_psi==""), ]
  
  par(fig=c(0,1,0,1), oma=c(0,0,0,0), mar=c(0,0,0,0), new=T)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  #using the first psi reading from LVI_psi which is at the bottom of the string  to determine the depth of the sensors
  if(as.numeric(obs3_sub[1,45])<14.5 & as.numeric(obs3_sub[1,45])>14){
    legend("right",c("Air", "Air", "Air", "0.1m", "1m", "2m", "3m", "4m","5m", "6m", "7m", "8m", "9m", "10m"),
           text.col=c("black","firebrick4", "firebrick1", "DarkOrange1", "gold", "greenyellow", "medium sea green", "sea green",
                      "DeepSkyBlue4", "blue2", "blue4", "darkslateblue", "magenta2", "darkmagenta"),
           cex=1, y.intersp=1, x.intersp=0.001, inset=c(0,0), xpd=T, bty='n')
    #using the first psi reading from LVI_psi which is at the bottom of the string  to determine the depth of the sensors
  }else if(as.numeric(obs3_sub[1,45])<15.3 & as.numeric(obs3_sub[1,45])>14.5){
    legend("right",c("Air", "Air", "Air", "0.5m", "1.5m", "2.5m", "3.5m", "4.5m","5.5m", "6.5m", "7.5m", "8.5m", "9.5m", "10.5m"),
           text.col=c("black","firebrick4", "firebrick1", "DarkOrange1", "gold", "greenyellow", "medium sea green", "sea green",
                      "DeepSkyBlue4", "blue2", "blue4", "darkslateblue", "magenta2", "darkmagenta"),
           cex=1, y.intersp=1, x.intersp=0.001, inset=c(0,0), xpd=T, bty='n')
    #using the first psi reading from LVI_psi which is at the bottom of the string  to determine the depth of the sensors
  } else if (as.numeric(obs3_sub[1,45])<16 & as.numeric(obs3_sub[1,45])>15.3){
    legend("right",c("Air", "Air", "Air", "1m", "2m", "3m", "4m", "5m","6m", "7m", "8m", "9m", "10m", "11m"),
           text.col=c("black","firebrick4", "firebrick1", "DarkOrange1", "gold", "greenyellow", "medium sea green", "sea green",
                      "DeepSkyBlue4", "blue2", "blue4", "darkslateblue", "magenta2", "darkmagenta"),
           cex=1, y.intersp=1, x.intersp=0.001, inset=c(0,0), xpd=T, bty='n') 
    #using the first psi reading from LVI_psi which is at the bottom of the string  to determine the depth of the sensors
  } else if (as.numeric(obs3_sub[1,45])<16.7 & as.numeric(obs3_sub[1,45])>16){
    legend("right",c("Air", "Air", "0.5", "1.5m", "2.5m", "3.5m", "4.5m", "5.5m","6.5m", "7.5m", "8.5m", "9.5m", "10.5m", "11.5m"),
           text.col=c("black","firebrick4", "firebrick1", "DarkOrange1", "gold", "greenyellow", "medium sea green", "sea green",
                      "DeepSkyBlue4", "blue2", "blue4", "darkslateblue", "magenta2", "darkmagenta"),
           cex=1, y.intersp=1, x.intersp=0.001, inset=c(0,0), xpd=T, bty='n') 
    #using the first psi reading from LVI_psi which is at the bottom of the string  to determine the depth of the sensors
  } else if (as.numeric(obs3_sub[1,45])<17.4 & as.numeric(obs3_sub[1,45])>16.7){ 
    legend("right",c("Air", "Air", "1m", "2m", "3m", "4m", "5m", "6m","7m", "8m", "9m", "10m", "11m", "12m"),
           text.col=c("black","firebrick4", "firebrick1", "DarkOrange1", "gold", "greenyellow", "medium sea green", "sea green",
                      "DeepSkyBlue4", "blue2", "blue4", "darkslateblue", "magenta2", "darkmagenta"),
           cex=1, y.intersp=1, x.intersp=0.001, inset=c(0,0), xpd=T, bty='n')
    #using the first psi reading from LVI_psi which is at the bottom of the string  to determine the depth of the sensors
  } else if (as.numeric(obs3_sub[1,45])<18.1 & as.numeric(obs3_sub[1,45])>17.4) { 
    legend("right",c("Air", "0.5m", "1.5m", "2.5m", "3.5m", "4.5m", "5.5m", "6.5m","7.5m", "8.5m", "9.5m", "10.5m", "11.5m", "12.5m"),
           text.col=c("black","firebrick4", "firebrick1", "DarkOrange1", "gold", "greenyellow", "medium sea green", "sea green",
                      "DeepSkyBlue4", "blue2", "blue4", "darkslateblue", "magenta2", "darkmagenta"),
           cex=1, y.intersp=1, x.intersp=0.001, inset=c(0,0), xpd=T, bty='n')
    #using the first psi reading from LVI__psi which is at the bottom of the string  to determine the depth of the sensors
  } else if (as.numeric(obs3_sub[1,45])<14 & as.numeric(obs3_sub[1,45])>13.2){ 
    legend("right",c("Air", "Air", "Air", "Air","0.5m", "1.5m", "2.5m", "3.5m", "4.5m", "5.5m", "6.5m","7.5m", "8.5m", "9.5m"),
           text.col=c("black","firebrick4", "firebrick1", "DarkOrange1", "gold", "greenyellow", "medium sea green", "sea green",
                      "DeepSkyBlue4", "blue2", "blue4", "darkslateblue", "magenta2", "darkmagenta"),
           cex=1, y.intersp=1, x.intersp=0.001, inset=c(0,0), xpd=T, bty='n')
    #using the first psi reading from LVI_psi which is at the bottom of the string  to determine the depth of the sensors
  } else  { 
    legend("right",c("Out of Range"),
           text.col=c("black"),
           cex=1, y.intersp=1, x.intersp=0.001, inset=c(0,0), xpd=T, bty='n')
    
  }
  dev.off() #file made!
}  

#CCR met data

ccrmetheader<-read.csv("ccre-met.csv", skip=1, as.is=T) #get header minus wonky Campbell rows
ccrmetdata<-read.csv("ccre-met.csv", skip=4, header=F) #get data minus wonky Campbell rows
names(ccrmetdata)<-names(ccrmetheader) #combine the names to deal with Campbell logger formatting

#Removes row if the TIMESTAMP column is blank
ccrmetdata <- ccrmetdata[complete.cases(ccrmetdata$TIMESTAMP),]

#Removes row if the RECORD column has an NA or blank
ccrmetdata=ccrmetdata[!(is.na(ccrmetdata$RECORD) | ccrmetdata$RECORD==""), ]

#for the time sequence we can use the same as from the FCR met staion 
end.time <- with_tz(as.POSIXct(strptime(Sys.time(), format = "%Y-%m-%d %H:%M")), tzone = "Etc/GMT+5") #gives us current time with rounded minutes in EDT
start.time <- end.time - days(7) #to give us seven days of data for looking at changes
full_time <- seq(start.time, end.time, by = "min") #create sequence of dates from past 5 days to fill in data

obs4 <- array(NA,dim=c(length(full_time),13)) #create array that will be filled in with 10 columns
#commented all lines that are irrelevant for 2020 data, per change in data downloads
#met_timechange=max(which(ccrmetdata$TIMESTAMP=="2019-04-15 10:19:00")) #shows time point when met station was switched from GMT -4 to GMT -5
ccrmetdata$TIMESTAMP<-as.POSIXct(strptime(ccrmetdata$TIMESTAMP, "%Y-%m-%d %H:%M"), tz = "Etc/GMT+5") #get dates aligned
#ccrmetdata$TIMESTAMP[c(1:met_timechange-1)]<-with_tz(force_tz(ccrmetdata$TIMESTAMP[c(1:met_timechange-1)],"Etc/GMT+4"), "Etc/GMT+5") #pre time change data gets assigned proper timezone then corrected to GMT -5 to match the rest of the data set
#ccrmetdata=ccrmetdata[-c(met_timechange-1),]

if (length(na.omit(ccrmetdata$TIMESTAMP[ccrmetdata$TIMESTAMP>start.time]))==0) { #if there is no data after start time, then a pdf will be made explaining this
  pdf(paste0("CCRMetDataFigures_", Sys.Date(), ".pdf"), width=8.5, height=11) #call PDF file
  plot(NA, xlim=c(0,5), ylim=c(0,5), bty='n',xaxt='n', yaxt='n', xlab='', ylab='') #creates empty plot
  mtext(paste("No data found between", start.time, "and", end.time, sep = " ")) #fills in text in top margin of plot
  dev.off() #file made!
} else {
  #merge instead of a for loop
  for(i in 1:length(full_time)){ #this loop looks for matching dates and extracts data from ccrmetdata file to obs4 array
    index = which(ccrmetdata$TIMESTAMP==full_time[i])
    if(length(index)>0){
      obs4[i,] <- unlist(ccrmetdata[index,c(1,2,3,5,8,9,10,11,12,13,14,15,16)])
    }
  }
  #obs4=merge(full_time,ccrmetdata, all.x=TRUE)#merge the data frame to get the last 7 days
  obs4<-as.data.frame(obs4) #make into DF
  colnames(obs4)<-names(ccrmetdata[index,c(1,2,3,5,8,9,10,11,12,13,14,15,16)]) #get column names
  obs4$TIMESTAMP<-full_time #now have your array with a proper timedate stamp!
  
  pdf(paste0("CCRMetDataFigures_", Sys.Date(), ".pdf"), width=8.5, height=11) #call PDF file
  par(mfrow=c(3,2))
  plot(obs4$TIMESTAMP,obs4$RECORD, main="RECORD", xlab="Time", ylab="Number", type='l')
  plot(obs4$TIMESTAMP,obs4$BattV, main="Battery", xlab="Time", ylab="Volts", type='l')
  plot(obs4$TIMESTAMP,obs4$AirTC_Avg, main="Air Temp", xlab="Time", ylab="degrees C", type='l')
  plot(obs4$TIMESTAMP,obs4$RH, main="Rel Hum", xlab="Time", ylab="%", type='l')
  plot(obs4$TIMESTAMP,obs4$Rain_mm_Tot, main="Rain", xlab="Time", ylab="mm", type='l')
  plot(obs4$TIMESTAMP,obs4$WS_ms_Avg, main="Wind speed", xlab="Time", ylab="m/s",type='l')
  plot(obs4$TIMESTAMP,obs4$WindDir, main="Wind direction", xlab="Time", ylab="degrees", type='l')
  plot(obs4$TIMESTAMP,obs4$SR01Up_Avg, main="Shortwave Up", xlab="Time", ylab="W/m2",type='l')
  plot(obs4$TIMESTAMP,obs4$SR01Dn_Avg, main="Shortwave Down", xlab="Time", ylab="W/m2",type='l')
  plot(obs4$TIMESTAMP,obs4$IR01UpCo_Avg, main="Longwave Up", xlab="Time", ylab="W/m2",type='l')
  plot(obs4$TIMESTAMP,obs4$IR01DnCo_Avg, main="Longwave Down", xlab="Time", ylab="W/m2",type='l')
  plot(obs4$TIMESTAMP,obs4$PAR_Den_Avg, main="PAR", xlab="Time", ylab="umol/s/m^2",type='l')
  dev.off() #file made!
}

#CCR water sensors. Going to have to edit when the EXOs are added 

ccrwaterheader<-read.csv("ccre-waterquality.csv", skip=1, as.is=T) #get header minus wonky Campbell rows
ccrwaterdata<-read.csv("ccre-waterquality.csv", skip=4, header=F) #get data minus wonky Campbell rows
names(ccrwaterdata)<-names(ccrwaterheader) #combine the names to deal with Campbell logger formatting


#Removes row if the TIMESTAMP column is blank
ccrwaterdata <- ccrwaterdata[complete.cases(ccrwaterdata$TIMESTAMP),]

#Removes row if the RECORD column has an NA or blank
ccrwaterdata=ccrwaterdata[!(is.na(ccrwaterdata$RECORD) | ccrwaterdata$RECORD==""), ]

#For the time sequence we can use the same as the FCR catwalk 
end.time1 <- with_tz(as.POSIXct(strptime(Sys.time(), format = "%Y-%m-%d %H")), tzone = "Etc/GMT+5") #gives us current time with rounded hours in EDT
start.time1 <- end.time1 - days(7) #to give us seven days of data for looking at changes
full_time1 <- as.data.frame(seq(start.time1, end.time1, by = "10 min")) #create sequence of dates from past 5 days to fill in data
colnames(full_time1)=c("TIMESTAMP") #make it a data frame to merge to make obs5 later

#obs5 <- array(NA,dim=c(length(full_time1),41)) #create array that will be filled in with 41 columns (the entire size of the array)
#cat_timechange=max(which(ccrwaterdata$TIMESTAMP=="2019-04-15 10:00:00"))
ccrwaterdata$TIMESTAMP<-as.POSIXct(strptime(ccrwaterdata$TIMESTAMP, "%Y-%m-%d %H:%M"), tz = "Etc/GMT+5") #get dates aligned
#ccrwaterdata$TIMESTAMP[c(1:cat_timechange-1)]<-with_tz(force_tz(ccrwaterdata$TIMESTAMP[c(1:cat_timechange-1)],"Etc/GMT+4"), "Etc/GMT+5") #pre time change data gets assigned proper timezone then corrected to GMT -5 to match the rest of the data set

if (length(na.omit(ccrwaterdata$TIMESTAMP[ccrwaterdata$TIMESTAMP>start.time1]))==0) { #if there is no data after start time, then a pdf will be made explaining this
  pdf(paste0("CCRWaterQualityDataFigures_", Sys.Date(), ".pdf"), width=8.5, height=11) #call PDF file
  plot(NA, xlim=c(0,5), ylim=c(0,5), bty='n',xaxt='n', yaxt='n', xlab='', ylab='') #creates empty plot
  mtext(paste("No data found between", start.time1, "and", end.time1, sep = " ")) #fills in text in top margin of plot
  dev.off() #file made!
} else {
  #I would end up with NAs for all of the data values
  #for(j in 5:39){
  #ccrwaterdata[,j]<-as.numeric(levels(ccrwaterdata[,j]))[ccrwaterdata[,j]]#need to set all columns to numeric values
  
  #}
  #for(i in 1:length(full_time1)){ #this loop looks for matching dates and extracts data from metdata file to obs array
  #index = which(ccrwaterdata$TIMESTAMP==full_time1[427])
  #if(length(index)>0){
  #obs5[n,] <- unlist(ccrwaterdata[index,c(1:41)])
  #}
  #}
  
  obs5=merge(full_time1,ccrwaterdata, all.x = TRUE)#merge the data frame to get the last 7 days
  #obs5<-as.data.frame(obs5) #make into DF
  #obs5[,1] <- full_time1 #now have your array with a proper timedate stamp!
  #colnames(obs5)<-names(ccrwaterdata[index,c(1:41)]) #get column names
  
  pdf(paste0("CCRWaterQualityDataFigures_", Sys.Date(), ".pdf"), width=8.5, height=11) #call PDF file
  par(mfrow=c(3,2))
  
  plot(obs5$TIMESTAMP,obs5$RECORD, main="Campbell Logger Record", xlab="Time", ylab="Number", type='l')
  plot(obs5$TIMESTAMP,obs5$BattV, main="Campbell Logger Battery", xlab="Time", ylab="Volts", type='l')
  #Going to add these back in when the EXos are in 
  #added y limits so the axises would show up when the are no data
  plot(obs5$TIMESTAMP,obs5$EXO_battery_9, main="EXO Battery", xlab="Time", ylab="Volts", type='l',lwd=1.5,  ylim=c(2,8))
  points(obs5$TIMESTAMP, obs5$EXO_battery_1, col="red", type='l', lwd=1.5 )
  legend("topleft", c("1.5m EXO", "9m EXO"), text.col=c("red","black"), x.intersp=0.001)
  
  plot(obs5$TIMESTAMP,obs5$EXO_cablepower_9, main="EXO Cable Power", xlab="Time", ylab="Volts", type='l',lwd=1.5, ylim=c(10,15))
  points(obs5$TIMESTAMP, obs5$EXO_cablepower_1, col="red", type='l', lwd=1.5 )
  legend("topleft", c("1.5m EXO", "9m EXO"), text.col=c("red","black"), x.intersp=0.001)
  
  plot(obs5$TIMESTAMP,obs5$EXO_depth_9, main="EXO Depth", xlab="Time", ylab="Meters", type='l', ylim=c(0,11))
  points(obs5$TIMESTAMP, obs5$EXO_depth_1, col="red", type="l", lwd=1.5)
  legend("topleft", c("1.5m EXO", "9m EXO"), text.col=c("red","black"), x.intersp=0.001)
  
  
  plot(obs5$TIMESTAMP,obs5$EXO_pressure_9, main="Sonde Pressure", xlab="Time", ylab="psi", type='l', ylim=c(-1,37))
  points(obs5$TIMESTAMP, obs5$EXO_pressure_1, col="purple", type="l", lwd=1.5)
  points(obs5$TIMESTAMP, obs5$Lvl_psi, col="blue4", type='l', lwd=1.5)
  legend("topleft", c("1.5m EXO", "9m EXO", "19m PT"), text.col=c("purple","black", "blue4"), x.intersp=0.001)
  
  
  plot(obs5$TIMESTAMP,obs5$doobs_9, main="DO", xlab="Time", ylab="mg/L", type='l', col="medium sea green", lwd=1.5, ylim=c(-0.5,15))
  legend("topleft", c("1.5m EXO", "9m EXO"), text.col=c("magenta", "medium sea green"), x.intersp=0.001)
  
  plot(obs5$TIMESTAMP,obs5$dosat_9, main="DO % saturation", xlab="Time", ylab="% saturation", type='l', col="medium sea green", lwd=1.5, ylim=c(-0.5,170))
  points(obs5$TIMESTAMP, obs5$dosat_1, col="magenta", type='l', lwd=1.5)
  legend("topleft", c("1.5m EXO", "9m EXO"), text.col=c("magenta", "medium sea green"), x.intersp=0.001)
  
  plot(obs5$TIMESTAMP,obs5$Cond_9, main="Cond, SpCond, TDS @ 1.5m and 9m", xlab="Time", ylab="uS/cm or mg/L", type='l', col="red", lwd=1.5, ylim=c(20,90))
  points(obs5$TIMESTAMP, obs5$Cond_1, col="magenta", type="l", lwd=1.5)
  points(obs5$TIMESTAMP, obs5$SpCond_9, col="black", type='l', lwd=1.5)
  points(obs5$TIMESTAMP, obs5$SpCond_1, col="gray", type='l', lwd=1.5)
  points(obs5$TIMESTAMP, obs5$TDS_9, col="orange", type='l', lwd=1.5)
  points(obs5$TIMESTAMP, obs5$TDS_1, col="DarkOrange1", type="l", lwd=1.5)
  legend("topleft", c("TDS 1m", "TDS 9m", "SPCond 1m","SpCond 9m", "Cond 1m","Cond 9m"), text.col=c("DarkOrange1", "orange", "gray", "black", "magenta","red"), x.intersp=0.001)
  # 
  plot(obs5$TIMESTAMP,obs5$Chla_1, main="Chla, Phyco, fDOM", xlab="Time", ylab="ug/L or QSU", type='l', col="green", lwd=1.5, ylim=c(-0.5,40))
  points(obs5$TIMESTAMP, obs5$BGAPC_1, col="blue", type='l', lwd=1.5)
  points(obs5$TIMESTAMP, obs5$fDOM_QSU_1, col="firebrick4", type='l', lwd=1.5)
  points(obs5$TIMESTAMP, obs5$fDOM_QSU_9, col="DarkOrange3", type="l", lwd=1.5)
  legend("topleft", c("Chla 1.5m", "Phyco 1.5m", "fDOM 1.5m", "fDOM 9m"), text.col=c("green", "blue", "firebrick4", "DarkOrange3"), x.intersp=0.001)
  
  par(mfrow=c(1,1))
  par(oma=c(1,1,1,4))
  plot(obs5$TIMESTAMP,obs5$wtr_1, main="Water Temp", xlab="Time", ylab="degrees C", type='l', col="firebrick4", lwd=1.5, ylim=c(0, 35))
  points(obs5$TIMESTAMP, obs5$wtr_2, col="firebrick1", type='l', lwd=1.5)
  points(obs5$TIMESTAMP, obs5$wtr_3, col="DarkOrange1", type='l', lwd=1.5)
  points(obs5$TIMESTAMP, obs5$wtr_4, col="gold", type='l', lwd=1.5)
  points(obs5$TIMESTAMP, obs5$wtr_5, col="greenyellow", type='l', lwd=1.5)
  points(obs5$TIMESTAMP, obs5$wtr_6, col="medium sea green", type='l', lwd=1.5)
  points(obs5$TIMESTAMP, obs5$wtr_7, col="sea green", type='l', lwd=1.5)
  points(obs5$TIMESTAMP, obs5$wtr_8, col="DeepSkyBlue4", type='l', lwd=1.5)
  points(obs5$TIMESTAMP, obs5$wtr_9, col="blue2", type='l', lwd=1.5)
  points(obs5$TIMESTAMP, obs5$EXO_wtr_9, col="blue4", type='l', lwd=1.5)
  points(obs5$TIMESTAMP, obs5$wtr_10, col="darkslateblue", type='l', lwd=1.5)
  points(obs5$TIMESTAMP, obs5$wtr_11, col="magenta2", type='l', lwd=1.5)
  points(obs5$TIMESTAMP, obs5$wtr_12, col="darkmagenta", type='l', lwd=1.5)
  points(obs5$TIMESTAMP, obs5$wtr_13, col="black", type='l', lwd=1.5)
  par(fig=c(0,1,0,1), oma=c(0,0,0,0), mar=c(0,0,0,0), new=T)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  legend("right",c("0.1m","1m", "2m", "3m", "4m", "5m", "6m", "7m", "8m","EXO_9m","10m","11m","15m","19m"),
         text.col=c("firebrick4", "firebrick1", "DarkOrange1", "gold", "greenyellow", "medium sea green", "sea green",
                    "DeepSkyBlue4", "blue2", "blue4", "darkslateblue", "magenta2", "darkmagenta", "black"), 
         cex=1, y.intersp=1, x.intersp=0.001, inset=c(0,0), xpd=T, bty='n')
  
  dev.off() #file made!
}