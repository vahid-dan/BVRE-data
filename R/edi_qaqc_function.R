qaqc_bvr <- function(data_file, data2_file, 
                     maintenance_file,  output_file, start_date, end_date)
{
  
  #bvrdata=data_file
  #change column names
  BVRDATA_COL_NAMES = c("DateTime", "RECORD", "CR6Battery_V", "CR6Panel_Temp_C", "ThermistorTemp_C_1",
                        "ThermistorTemp_C_2", "ThermistorTemp_C_3", "ThermistorTemp_C_4", "ThermistorTemp_C_5",
                        "ThermistorTemp_C_6", "ThermistorTemp_C_7", "ThermistorTemp_C_8", "ThermistorTemp_C_9",
                        "ThermistorTemp_C_10","ThermistorTemp_C_11","ThermistorTemp_C_12","ThermistorTemp_C_13",
                        "RDO_mgL_6", "RDOsat_percent_6", "RDOTemp_C_6", "RDO_mgL_13",
                        "RDOsat_percent_13", "RDOTemp_C_13", "EXO_Date", "EXO_Time", "EXOTemp_C_1.5", "EXOCond_uScm_1.5",
                        "EXOSpCond_uScm_1.5", "EXOTDS_mgL_1.5", "EXODOsat_percent_1.5", "EXODO_mgL_1.5", "EXOChla_RFU_1.5",
                        "EXOChla_ugL_1.5", "EXOBGAPC_RFU_1.5", "EXOBGAPC_ugL_1.5", "EXOfDOM_RFU_1.5", "EXOfDOM_QSU_1.5",
                        "EXOTurbidity_FNU_1.5", "EXOTSS_mg_1.5","EXOPressure_psi", "EXODepth_m", "EXOBattery_V",
                        "EXOCablepower_V", "EXOWiper_V", "LvlPressure_psi_13", "LvlTemp_C_13")
  
  
  #Adjustment period of time to stabilization after cleaning in seconds
  ADJ_PERIOD = 2*60*60 
  
  #read in data from obs1 above
  
  # read bvrwalk data and maintenance log
  # NOTE: date-times throughout this script are processed as UTC
  # read_csv was not working for me on 01 SEP 22 so went to read.csv
  # Maybe it will work again 
  # bvrdata1 <- read_csv(data_file, skip=1, col_names = BVRDATA_COL_NAMES,
  #col_types = cols(.default = col_double(), DateTime = col_datetime()))
  
  bvrdata1 <- read.csv(data_file, skip=1, col.names = BVRDATA_COL_NAMES)
  
  bvrdata2<-read_csv(data2_file, skip=1, col_names = BVRDATA_COL_NAMES)
  
  ## read in maintenance file 
  log_read <- read_csv(maintenance_file, col_types = cols(
    .default = col_character(),
    TIMESTAMP_start = col_datetime("%Y-%m-%d %H:%M:%S%*"),
    TIMESTAMP_end = col_datetime("%Y-%m-%d %H:%M:%S%*"),
    flag = col_integer()
  ))
  
  log <- log_read
  
  ### identify the date subsetting for the data
  if (!is.null(start_date)){
    bvrdata1 <- bvrdata1 %>% 
      filter(DateTime >= start_date)
    bvrdata2 <- bvrdata2 %>% 
      filter(DateTime >= start_date)
    log <- log %>% 
      filter(TIMESTAMP_start >= start_date)
  }
  
  if(!is.null(end_date)){
    bvrdata1 <- bvrdata1 %>% 
      filter(DateTime <= end_date)
    bvrdata2 <- bvrdata2 %>% 
      filter(DateTime <= end_date)
    log <- log %>% 
      filter(TIMESTAMP_end <= end_date)
  }
  
  if (nrow(log) == 0){
    log <- log_read
  }
  
  #bad headers so remove them
  
  bvrdata1<-bvrdata1%>%filter(grepl("^20", DateTime)) #keep only the right TIMESTAMP rows 
  
  bvrdata1$DateTime=ymd_hms(bvrdata1$DateTime)#convert the DateTime column
  
  bvrdata1[,-1] <- sapply(bvrdata1[, -1], as.numeric)#converts all columns to numeric minus the DateTime
  
  
  bvrdata= bvrdata1%>%
    rbind(.,bvrdata2)%>% #combine manual and most recent files
    drop_na(DateTime)%>% #take out the rows with blank timestamps
    distinct(DateTime, .keep_all= TRUE) #taking out the duplicate values 
  
  #bvrdata$DateTime<-as.POSIXct(bvrdata$DateTime,format = "%Y-%m-%d %H:%M:%S", tz="UTC")
  #after$DateTime<-as.POSIXct(strptime(after$DateTime, "%Y-%m-%d %H:%M"), tz = "EST")
  
  
  #######Chekc for gaps and missing data####################################################################################################################
  #order data by timestamp
  BVRdata2=bvrdata
  BVRdata2=BVRdata2[order(BVRdata2$DateTime),]
  BVRdata2$DOY=yday(BVRdata2$DateTime)
  
  
  #check record for gaps
  #daily record gaps by day of year
  for(i in 2:nrow(BVRdata2)){ #this identifies if there are any data gaps in the long-term record, and where they are by record number
    if(BVRdata2$DOY[i]-BVRdata2$DOY[i-1]>1){
      print(c(BVRdata2$DateTime[i-1],BVRdata2$DateTime[i]))
    }
  }
  BVR2=BVRdata2%>%filter(!is.na(RECORD))
  for(i in 2:length(BVR2$RECORD)){ #this identifies if there are any data gaps in the long-term record, and where they are by record number
    if(abs(BVR2$RECORD[i]-BVR2$RECORD[i-1])>1){
      print(c(BVR2$DateTime[i-1],BVR2$DateTime[i]))
    }
  }
  ########Create Flag columns####################################################################################################################### 
  
  # Change NaN into NA
  bvrdata[sapply(bvrdata, is.nan)] <- NA
  
  # for loop to create flag columns
  for(j in c(5:23,26:46)) { #for loop to create new columns in data frame
    bvrdata[,paste0("Flag_",colnames(bvrdata[j]))] <- 0 #creates flag column + name of variable
    bvrdata[c(which(is.na(bvrdata[,j]))),paste0("Flag_",colnames(bvrdata[j]))] <-7 #puts in flag 7 if value not collected
  }
  #update 
  for(k in c(18:19,21:22,27:39)) { #for loop to create new columns in data frame
    bvrdata[c(which((bvrdata[,k]<0))),paste0("Flag_",colnames(bvrdata[k]))] <- 3
    bvrdata[c(which((bvrdata[,k]<0))),k] <- 0 #replaces value with 0
  }
  
  ##########Maintenance Log QAQC############ 
  
  #Read in the maintneance log 
  
  # log <- read_csv(maintenance_file, col_types = cols(
  #   .default = col_character(),
  #   TIMESTAMP_start = col_datetime("%Y-%m-%d %H:%M:%S%*"),
  #   TIMESTAMP_end = col_datetime("%Y-%m-%d %H:%M:%S%*"),
  #   flag = col_integer()
  # )) 
  
  # modify catdata based on the information in the log  
  
  #Filter out the 7 flag because it is already NAs in the dataset and not maintenance
  log=log%>%filter(flag!=7)
  
  # modify bvrdata based on the information in the log
  for(i in 1:nrow(log))
  {
    # get start and end time of one maintenance event
    start <- log$TIMESTAMP_start[i]
    end <- log$TIMESTAMP_end[i]
    
    
    # get indices of columns affected by maintenance
    if(grepl("^\\d+$", log$colnumber[i])) # single num
    {
      maintenance_cols <- intersect(c(2:46), as.integer(log$colnumber[i]))
    }
    
    else if(grepl("^c\\(\\s*\\d+\\s*(;\\s*\\d+\\s*)*\\)$", log$colnumber[i])) # c(x;y;...)
    {
      maintenance_cols <- intersect(c(2:46), as.integer(unlist(regmatches(log$colnumber[i],
                                                                          gregexpr("\\d+", log$colnumber[i])))))
    }
    else if(grepl("^c\\(\\s*\\d+\\s*:\\s*\\d+\\s*\\)$", log$colnumber[i])) # c(x:y)
    {
      bounds <- as.integer(unlist(regmatches(log$colnumber[i], gregexpr("\\d+", log$colnumber[i]))))
      maintenance_cols <- intersect(c(2:46), c(bounds[1]:bounds[2]))
    }
    else
    {
      warning(paste("Could not parse column colnumber in row", i, "of the maintenance log. Skipping maintenance for",
                    "that row. The value of colnumber should be in one of three formats: a single number (\"47\"), a",
                    "semicolon-separated list of numbers in c() (\"c(47;48;49)\"), or a range of numbers in c() (\"c(47:74)\").",
                    "Other values (even valid calls to c()) will not be parsed properly."))
      next
    }
    
    # remove EXO_Date and EXO_Time columns from the list of maintenance columns, because they will be deleted later
    maintenance_cols <- setdiff(maintenance_cols, c(24, 25))
    
    if(length(maintenance_cols) == 0)
    {
      warning(paste("Did not parse any valid data columns in row", i, "of the maintenance log. Valid columns have",
                    "indices 2 through 46, excluding 24 and 25, which are deleted by this script. Skipping maintenance for that row."))
      next
    }
    
    #index the Flag columns
    if(grepl("^\\d+$", log$flagcol[i])) # single num
    {
      flag_cols <- intersect(c(47:86), as.integer(log$flagcol[i]))
      
    }
    else if(grepl("^c\\(\\s*\\d+\\s*(;\\s*\\d+\\s*)*\\)$", log$flagcol[i])) # c(x;y;...)
    {
      flag_cols <- intersect(c(47:86), as.integer(unlist(regmatches(log$flagcol[i],
                                                                    gregexpr("\\d+", log$flagcol[i])))))
    }
    
    else if(grepl("^c\\(\\s*\\d+\\s*:\\s*\\d+\\s*\\)$", log$flagcol[i])) # c(x:y)
    {
      bounds_flag <- as.integer(unlist(regmatches(log$flagcol[i], gregexpr("\\d+", log$flagcol[i]))))
      flag_cols <- intersect(c(47:86), c(bounds_flag[1]:bounds_flag[2]))
    }
    else
    {
      warning(paste("Could not parse column flagcol in row", i, "of the maintenance log. Skipping maintenance for",
                    "that row. The value of colnumber should be in one of three formats: a single number (\"47\"), a",
                    "semicolon-separated list of numbers in c() (\"c(47;48;49)\"), or a range of numbers in c() (\"c(47:74)\").",
                    "Other values (even valid calls to c()) will not be parsed properly."))
      next
    }
    
    #Get the Maintenance Flag 
    
    flag <- log$flag[i]
    
    # replace relevant data with NAs and set flags while maintenance was in effect
    if(flag==6 && maintenance_cols==27){# correct bad temp data
      # Correct conductivity with a linear realationship to the CTD
      bvrdata[bvrdata$DateTime >= start & bvrdata$DateTime <= end, "EXOCond_uScm_1.5"]<- (bvrdata[bvrdata$DateTime >= start & bvrdata$DateTime <= end, "EXOCond_uScm_1.5"]*0.69)+4.99
      bvrdata[bvrdata$DateTime >= start & bvrdata$DateTime <= end, "Flag_EXOCond_uScm_1.5"] <- flag
    }else if (flag==6 && maintenance_cols==28){
      # Correct Specific Conductivity based on specific conductivity 
      bvrdata[bvrdata$DateTime >= start & bvrdata$DateTime <= end, "EXOSpCond_uScm_1.5"]<- bvrdata[bvrdata$DateTime >= start & bvrdata$DateTime <= end, "EXOCond_uScm_1.5"]/(1+((bvrdata[bvrdata$DateTime >= start & bvrdata$DateTime <= end, "EXOTemp_C_1.5"]-25)*0.02))
      bvrdata[bvrdata$DateTime >= start & bvrdata$DateTime <= end, "Flag_EXOSpCond_uScm_1.5"] <- flag
    }else if (flag==6 && maintenance_cols==29){
      # Correct TDS based on corrected specific conductivity
      bvrdata[bvrdata$DateTime >= start & bvrdata$DateTime <= end, "EXOTDS_mgL_1.5"]<-bvrdata[bvrdata$DateTime >= start & bvrdata$DateTime <= end, "EXOSpCond_uScm_1.5"]*0.65
      bvrdata[bvrdata$DateTime >= start & bvrdata$DateTime <= end, "Flag_EXOTDS_mgL_1.5"]<-flag
    }else if (flag==5){
      
      bvrdata[bvrdata$DateTime >= start & bvrdata$DateTime <= end, flag_cols] <- flag
    }
    else{
      bvrdata[bvrdata$DateTime >= start & bvrdata$DateTime <= end, maintenance_cols] <- NA
      bvrdata[bvrdata$DateTime >= start & bvrdata$DateTime <= end, flag_cols] <- flag
      next
    }
    
    #Add the 2 hour adjustment for DO 
    if (log$colnumber[i]=="c(1:46)" & flag==1){
      DO_col=c("RDO_mgL_6", "RDOsat_percent_6", "RDO_mgL_13","RDOsat_percent_13","EXODOsat_percent_1.5", "EXODO_mgL_1.5")
      DO_flag_col=c("Flag_RDO_mgL_6", "Flag_RDOsat_percent_6", "Flag_RDO_mgL_13","Flag_RDOsat_percent_13","Flag_EXODOsat_percent_1.5", "Flag_EXODO_mgL_1.5")
      bvrdata[bvrdata$DateTime>start&bvrdata$DateTime<(end+ADJ_PERIOD),DO_col] <- NA
      bvrdata[bvrdata$DateTime>start&bvrdata$DateTime<(end+ADJ_PERIOD),DO_flag_col] <- flag
    }
    else if(log$colnumber[i] %in% c(" 18"," 19") & flag==1){
      DO_col=c(RDO_mgL_6, RDOsat_percent_6)
      DO_flag_col=c(Flag_RDO_mgL_6, Flag_RDOsat_percent_6)
      bvrdata[bvrdata$DateTime>start[i]&bvrdata$DateTime<end[i]+ADJ_PERIOD,DO_col] <- NA
      bvrdata[bvrdata$DateTime>start[i]&bvrdata$DateTime<end[i]+ADJ_PERIOD,DO_flag_col] <- flag
      
    }
    else if(log$colnumber[i] %in% c(" 21"," 22") & flag==1){
      DO_col=c(RDO_mgL_13,RDOsat_percent_13)
      DO_flag_col=c(Flag_RDO_mgL_13,Flag_RDOsat_percent_13)
      bvrdata[bvrdata$DateTime>start[i]&bvrdata$DateTime<end[i]+ADJ_PERIOD,DO_col] <- NA
      bvrdata[bvrdata$DateTime>start[i]&bvrdata$DateTime<end[i]+ADJ_PERIOD,DO_flag_col] <- flag
      
    }
    else if (log$colnumber[i] %in% c(" c(26:44)"," 30"," 31") & flag==1){
      DO_col=c(EXODOsat_percent_1.5, EXODO_mgL_1.5)
      DO_flag_col=c(Flag_EXODOsat_percent_1.5, Flag_EXODO_mgL_1.5)
      bvrdata[bvrdata$DateTime>start[i]&bvrdata$DateTime<(end[i]+ADJ_PERIOD),DO_col] <- NA
      bvrdata[bvrdata$DateTime>start[i]&bvrdata$DateTime<(end[i]+ADJ_PERIOD),DO_flag_col] <-1
      
    }
    else{
      warning(paste("No DO time to adjust in row",i,"."))
      
    }
  }
  
  ############## Remove and Flag when sensors are out of position ####################
  
  #change EXO values to NA if EXO depth is less than 0.5m and Flag as 2
  
  #index only the colummns with EXO at the beginning
  exo_idx <-grep("^EXO",colnames(bvrdata))
  
  
  #create list of the Flag columns that need to be changed to 2
  exo_flag <- grep("^Flag_EXO*$",colnames(bvrdata))
  
  #Change the EXO data to NAs when the EXO is above 0.2m and not due to maintenance
  #Flag the data that was removed with 2 for outliers
  bvrdata[which(bvrdata$EXODepth_m<0.2),exo_flag]<- 2
  bvrdata[which(bvrdata$EXODepth_m < 0.2), exo_idx] <- NA
  
  
  #Change the EXO data to NAs when the EXO is above 0.2m and not due to maintenance
  #Flag the data that was removed with 2 for outliers
  #bvrdata[which(bvrdata$EXOCond_uScm_1.5 < 14),exo_flag]<- 2
  #bvrdata[which(bvrdata$EXOCond_uScm_1.5 < 14), exo_idx] <- NA
  
  
  
  #change the temp string and pressure sensor to NA if the psi is less than XXXXX and Flag as 2
  
  #index only the colummns with EXO at the beginning
  temp_idx <-grep("^Ther*|^RDO*|^Lvl*",colnames(bvrdata))
  
  #create list of the Flag columns that need to be changed to 2
  temp_flag <- grep("^Flag_Ther*|^Flag_RDO*|^Flag_Lvl*",colnames(bvrdata))
  
  #Change the EXO data to NAs when the pressure sensor is less than 9.94 psi which is roughly 7m and not due to maintenance. 
  # Also remove when the pressure sensor is NA because we don't know at what depth the sensors are at. 
  bvrdata[which(bvrdata$LvlPressure_psi_13 < 10), temp_flag]<- 2
  bvrdata[which(bvrdata$LvlPressure_psi_13 < 10), temp_idx] <- NA
  #Flag the data that was removed with 2 for outliers
  
  # If the pressure sensor is reading NA then have to take out all readings because we can't get a depth
  
  bvrdata[which(is.na(bvrdata$LvlPressure_psi_13)), temp_flag]<-2
  bvrdata[which(is.na(bvrdata$LvlPressure_psi_13)), temp_idx]<-NA
  
  
  ############## Leading and Lagging QAQC ##########################
  # This finds the point that is way out of range from the leading and lagging point 
  
  # loops through all of the columns to catch values that are above 2 or 4 sd above or below
  # the leading or lagging point 
  
  # need to make it a data frame because I was having issues with calculating the mean
  
  bvrdata=data.frame(bvrdata)
  
  for (a in c(5:23,26:46)){
    Var_mean <- mean(bvrdata[,a], na.rm = TRUE)
    
    # For Algae sensors we use 4 sd as a threshold but for the others we use 2
    if (colnames(bvrdata[a]) %in% c("EXOChla_RFU_1.5","EXOChla_ugL_1.5","EXOBGAPC_RFU_1.5","EXOBGAPC_ugL_1.5")){
      Var_threshold <- 4 * sd(bvrdata[,a], na.rm = TRUE)
    }else{ # all other variables we use 2 sd as a threshold
      Var_threshold <- 2 * sd(bvrdata[,a], na.rm = TRUE)
    }
    # Create the observation column, the lagging column and the leading column
    bvrdata$Var <- lag(bvrdata[,a], 0)
    bvrdata$Var_lag = lag(bvrdata[,a], 1)
    bvrdata$Var_lead = lead(bvrdata[,a], 1)
    
    # Replace the observations that are above the threshold with NA and then put a flag in the flag column
    
    bvrdata[c(which((abs(bvrdata$Var_lag - bvrdata$Var) > Var_threshold) &
                      (abs(bvrdata$Var_lead - bvrdata$Var) > Var_threshold)&!is.na(bvrdata$Var))) ,a] <-NA
    
    bvrdata[c(which((abs(bvrdata$Var_lag - bvrdata$Var) > Var_threshold) &
                      (abs(bvrdata$Var_lead - bvrdata$Var) > Var_threshold)&!is.na(bvrdata$Var))) ,paste0("Flag_",colnames(bvrdata[a]))]<-2
  }
  
  # Remove the leading and lagging columns
  
  bvrdata<-bvrdata%>%select(-c(Var, Var_lag, Var_lead))
  
  ########################################################################################################################### 
  
  #create depth column
  bvrdata=bvrdata%>%mutate(Depth_m_13=LvlPressure_psi_13*0.70455)#1psi=2.31ft, 1ft=0.305m
  
  #offsets from BVR_
  bvrdata=bvrdata%>%
    mutate(
      depth_1=Depth_m_13-11.82, #Gets depth of thermistor 1
      depth_2=Depth_m_13-11.478, #Gets depth of thermistor 2
      depth_3=Depth_m_13-10.47, #Gets depth of thermistor 3
      depth_4=Depth_m_13-9.423, # Gets depth of thermistor 4
      depth_5=Depth_m_13-8.376) #Gets depth of thermistor 5. This will have to be recalculated if/when the thermistor comees out of the water. 
  
  
  # For loop to set the values to NA when the thermistor is out of the water
  for(b in c(88:92)){
    if (colnames(bvrdata[b])=="depth_1"){
      d="ThermistorTemp_C_1"
    } else if (colnames(bvrdata[b])=="depth_2"){
      d="ThermistorTemp_C_2"
    }else if (colnames(bvrdata[b])=="depth_3"){
      d="ThermistorTemp_C_3"
    }else if (colnames(bvrdata[b])=="depth_4"){
      d="ThermistorTemp_C_4"
    }else if (colnames(bvrdata[b])=="depth_5"){
      d="ThermistorTemp_C_5"
    }
    
    bvrdata[c(which(!is.na(bvrdata[,b])& bvrdata[,b]<0)),paste0("Flag_",d)]<-2
    bvrdata[c(which(!is.na(bvrdata[,b])& bvrdata[,b]<0)),d]<-NA
    
  }
  
  ################################################################################################################################  
  # delete EXO_Date and EXO_Time columns
  bvrdata <- bvrdata %>% select(-EXO_Date, -EXO_Time, -depth_1, -depth_2, -depth_3, -depth_4, -depth_5)
  
  # add Reservoir and Site columns
  bvrdata$Reservoir="BVR"
  bvrdata$Site=50
  
  
  # reorder columns
  bvrdata <- bvrdata %>% select(Reservoir, Site, DateTime, ThermistorTemp_C_1:ThermistorTemp_C_13,
                                RDO_mgL_6, RDOsat_percent_6,
                                RDOTemp_C_6, RDO_mgL_13, RDOsat_percent_13, RDOTemp_C_13,
                                EXOTemp_C_1.5, EXOCond_uScm_1.5, EXOSpCond_uScm_1.5, EXOTDS_mgL_1.5, EXODOsat_percent_1.5,
                                EXODO_mgL_1.5, EXOChla_RFU_1.5, EXOChla_ugL_1.5, EXOBGAPC_RFU_1.5, EXOBGAPC_ugL_1.5,
                                EXOfDOM_RFU_1.5, EXOfDOM_QSU_1.5,EXOTurbidity_FNU_1.5, EXOPressure_psi, EXODepth_m, EXOBattery_V, EXOCablepower_V,
                                EXOWiper_V, LvlPressure_psi_13,Depth_m_13, LvlTemp_C_13, RECORD, CR6Battery_V, CR6Panel_Temp_C,
                                Flag_ThermistorTemp_C_1:Flag_ThermistorTemp_C_13,Flag_RDO_mgL_6, Flag_RDOsat_percent_6, Flag_RDOTemp_C_6,
                                Flag_RDO_mgL_13, Flag_RDOsat_percent_13, Flag_RDOTemp_C_13,Flag_EXOTemp_C_1.5, Flag_EXOCond_uScm_1.5, Flag_EXOSpCond_uScm_1.5,Flag_EXOTDS_mgL_1.5,
                                Flag_EXODOsat_percent_1.5, Flag_EXODO_mgL_1.5, Flag_EXOChla_RFU_1.5,Flag_EXOChla_ugL_1.5, Flag_EXOBGAPC_RFU_1.5,Flag_EXOBGAPC_ugL_1.5,
                                Flag_EXOfDOM_RFU_1.5,Flag_EXOfDOM_QSU_1.5, Flag_EXOTurbidity_FNU_1.5, 
                                Flag_EXOPressure_psi, Flag_EXODepth_m, Flag_EXOBattery_V, Flag_EXOCablepower_V,Flag_EXOWiper_V,Flag_LvlPressure_psi_13, Flag_LvlTemp_C_13)
  
  #order by date and time
  bvrdata <- bvrdata[order(bvrdata$DateTime),]
  
  
  # convert datetimes to characters so that they are properly formatted in the output file
  bvrdata$DateTime <- as.character(bvrdata$DateTime)
  
  
  # write to output file
  write.csv(bvrdata, output_file, row.names = FALSE, quote=FALSE)
  
} 


# example usage


#qaqc('https://github.com/FLARE-forecast/BVRE-data/raw/bvre-platform-data/BVRplatform.csv',
#      'https://raw.githubusercontent.com/CareyLabVT/ManualDownloadsSCCData/master/BVRplatform_manual_2020.csv',
#      "https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data/BVR_maintenance_log.txt",
#       "BVRplatform_clean.csv", 
#     "BVR_Maintenance_2020.csv")

