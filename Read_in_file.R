#current BVR data
bvrheader<-read.csv("bvre-waterquality.csv", skip=1, as.is=T) #get header minus wonky Campbell rows
bvrdata<-read.csv("bvre-waterquality.csv", skip=4, header=F) #get data minus wonky Campbell rows
names(bvrdata)<-names(bvrheader) 