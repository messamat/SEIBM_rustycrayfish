#Author: Mathis L. Messager
#Related publication: Messager ML, Olden JD. Individual-based models forecast the spread and inform the management 
#                                             of an emerging riverine invader. Divers Distrib.2018;00:1-14. https://doi.org/10.1111/ddi.12829
#Purpose: Clean, format, and join water temperature data and land surface temperature data (2000-2016).
#         Develop model of water temperature data based on land surface temperature.
#Produce: Table S2.1

rootdir <- 'F:/Chapter2_HexSim_Crayfish/Data' 
setwd(file.path(rootdir,"ISEMP"))
modisdat <- file.path(rootdir,'MODIS_LST_download')

##########################Import all data tables##################################
library(dplyr)
library(data.table)
library(ggplot2)
library(plyr)
library(car)
library(zoo)
library(caret)
library(grid)
library(gridExtra)

#Collate all the temperature data in a list
collate_dat <- function(region_name, root_dir) {
  require(dplyr)
  require(data.table)
  #List files/directories in directory
  site_doc_list <- list.files(root_dir)
  #Iterate over every directory within directory
  alltemp <- lapply(site_doc_list[grep(region_name, site_doc_list)], function(x) {
    #Create path
    temp_period <- paste(root_dir, x, sep = "/")
    #List files within directory
    temp_period_list <- list.files(temp_period)
    #Identify table containing water quality data
    temp_period_datatab <- temp_period_list[grep("WaterQuality_WaterQuality", temp_period_list)]
    #Create path for table
    temp_period_datatab_path <- paste(temp_period, temp_period_datatab, sep = "/")
    #Read table
    temp_outpop <- read.csv(temp_period_datatab_path)
    #Return table
    temp_outpop
  })
  #Merge all tables within a dataframe
  return(as.data.frame(rbindlist(alltemp)))
}

#LowerJohnDay, UpperJohnDay, MiddleFork, NorthFork
LJD <- collate_dat(root_dir = getwd(), region_name = "LowerJohnDay")
UJD <- collate_dat(root_dir = getwd(), region_name = "UpperJohnDay")
MFJD <- collate_dat(root_dir = getwd(), region_name = "MiddleFork")
NFJD <- collate_dat(root_dir = getwd(), region_name = "NorthFork")


LJD <- read.csv("Collated/LowerJohnDay_collated.csv")
UJD <- read.csv("Collated/UpperJohnDay_collated.csv")
MFJD <- read.csv("Collated/MiddleForkJohnDay_collated.csv")
NFJD <- read.csv("Collated/NorthForkJohnDay_collated.csv")
NFJD$SampleDate <- as.Date(as.character(NFJD$SampleDate))
NFJD_2008 <-NFJD[NFJD$SampleDate > as.Date("2008-01-01") & NFJD$SampleDate < as.Date("2008-12-31") & !is.na(NFJD$WaterQualityValue) & NFJD$MethodName == "Water Temperature",]

#Pre-format and compute daily means for more easily cleaning data

format_f <- function(dataset) {
  #Format record date column to date format
  dataset$SampleDateTime_format <- as.POSIXct(strptime(dataset$SampleDateTime, "%Y-%m-%d %H:%M:%S"))
  #Take out Air temperature records
  dataset <- dataset[dataset$MethodName == "Water Temperature",]
  #Remove duplicate
  dataset_nodupli <-dataset[!duplicated(dataset[,c("SITE_IDENTIFIER", "SampleDateTime")]),]
  #Take out records with no temp data
  dataset_nodupli <-dataset_nodupli[!is.na(dataset_nodupli$WaterQualityValue),]
  #Create a date only (no time) field
  dataset_nodupli$SampleDateTime_format_dayonly <- as.Date(dataset_nodupli$SampleDateTime_format)
  #Calculate daily mean and SD for each site and day
  dataset_nodupli_stat <- ddply(dataset_nodupli, .(SampleDateTime_format_dayonly, SITE_IDENTIFIER), summarise, dailymean = mean(WaterQualityValue), dailysd = sd(WaterQualityValue))
  return(dataset_nodupli_stat)
}
UJD_dailymean <- format_f(UJD)
write.csv(UJD_dailymean , "Daily_mean_data/UJD_dailymean_noclean.csv")
MFJD_dailymean <- format_f(MFJD)
write.csv(MFJD_dailymean , "Daily_mean_data/MFJD_dailymean_noclean.csv")
NFJD_dailymean <- format_f(NFJD)
write.csv(NFJD_dailymean , "Daily_mean_data/NFJD_dailymean_noclean.csv")

###########
# Cleaned UJD, NFJD, MFJD using the time series cleaning app and wrote it out to e.g. F:/Hexsim/Data/ISEMP/Daily_mean_data/NFJD_dailymean_clean.csv
# For LJD, cleaned up manually before having built the app but ended re-cleaning with app as well

#################################################################################################################################
########################################### LOWER JOHN DAY RIVER ################################################################
#################################################################################################################################

###############################  CLEAN DATA ####################
#Number of unique sites
length(unique(LJD$SITE_IDENTIFIER))
#Format record date column to date format
LJD$SampleDateTime_format <- as.POSIXct(strptime(LJD$SampleDateTime, "%Y-%m-%d %H:%M:%S"))
#Take out Air temperature records
LJD <- LJD[LJD$MethodName == "Water Temperature",]

#########################
#####Check for duplicates 
str(LJD)
#Returning just the duplicates
dupli <- LJD[duplicated(LJD[,c("SITE_IDENTIFIER", "SampleDateTime")]),]
tripli <- dupli[duplicated(dupli[,c("SITE_IDENTIFIER", "SampleDateTime")]),]
quadrupli <- tripli[duplicated(tripli[,c("SITE_IDENTIFIER", "SampleDateTime")]),]

#Remove duplicates
LJD_nodupli <-LJD[!duplicated(LJD[,c("SITE_IDENTIFIER", "SampleDateTime")]),]
#########################
#Check out values of water temperature
summary(LJD_nodupli)
qplot(LJD_nodupli$WaterQualityValue)

#Remove records with no temperature data
LJD_nodupli <-LJD_nodupli[!is.na(LJD_nodupli$WaterQualityValue),]

####################################################################################################################################
######################## FINISH UP LOGGER DATA CLEANING
LJD_clean <- read.csv('Daily_mean_data/LJD_dailymean_clean.csv')
UJD_clean <- read.csv("Daily_mean_data/UJD_dailymean_clean.csv")
MFJD_clean <- read.csv("Daily_mean_data/MFJD_dailymean_clean.csv")
NFJD_clean <- read.csv("Daily_mean_data/NFJD_dailymean_clean.csv")

JD_logdat <- rbind(LJD_clean, UJD_clean, MFJD_clean, NFJD_clean)

#Check negatives
JD_logdat_neg <- JD_logdat[JD_logdat$dailymean < 0,]

#Delete negatives < -1C: jd_012, jd_005, wtr_temp_MFJD_bCoyoteCr, wtr_temp_HawkinsCre_bRoad, COLVIN#2
#Save the edits as e.g. UJD_dailymean_clean_2.csv
LJD_clean <- read.csv('Daily_mean_data/LJD_dailymean_clean_2.CSV')
UJD_clean <- read.csv("Daily_mean_data/UJD_dailymean_clean_3.csv")
MFJD_clean <- read.csv("Daily_mean_data/MFJD_dailymean_clean_4.csv")
NFJD_clean <- read.csv("Daily_mean_data/NFJD_dailymean_clean_4.csv")
#Extra column names tag along with read/writing from R
UJD_clean <- UJD_clean[,-1]
MFJD_clean <- MFJD_clean[,-1]
MFJD_clean <- MFJD_clean[,-1]
NFJD_clean <- NFJD_clean[,-1]
JD_logdat <- rbind(LJD_clean, UJD_clean, MFJD_clean, NFJD_clean)

#Change other negatives to 0C
JD_logdat[JD_logdat$dailymean < 0,"dailymean"] <- 0 

#Check for sites with "air" or "AIR" in site name, make sure they are erroneous, if so delete them
JD_logdat_air <- JD_logdat[grep("air", JD_logdat$SITE_IDENTIFIER),]
JD_logdat_AIR <- JD_logdat[grep("AIR", JD_logdat$SITE_IDENTIFIER),]
JD_logdat_Air <- JD_logdat[grep("Air", JD_logdat$SITE_IDENTIFIER),]
JD_logdat <- JD_logdat[-grep("air", JD_logdat$SITE_IDENTIFIER),]
JD_logdat <- JD_logdat[-grep("Air", JD_logdat$SITE_IDENTIFIER),]

#Re-check for sites with temperature in Farenheit
JD_logdat_F <- JD_logdat[JD_logdat$dailymean > 35,]
JD_logdat_F <- merge(JD_logdat_F, SitesInfo_JD_nodupli, by = "SITE_IDENTIFIER")

#2045429_ButteCreek, mf_camp, mf_oxb, myrtle, vineg_ditch
# #Delete these in MFJD_dailymean_clean_2 and save it as MFJD_dailymean_clean_2 with the app and then re-merge everything

#Take out more outlier temperature data based on seemingly erroneous relationship with LST

#Format site data to be merged with LST data
str(JD_logdat)
JD_logdat <- select(JD_logdat, -X.1, -X)
JD_logdat <- dplyr::rename(JD_logdat, date = SampleDateTime_format_dayonly)

write.csv(JD_logdat, "Daily_mean_data/JD_logdat2.csv")
JD_logdat <- read.csv("Daily_mean_data/JD_logdat2.csv")

#################################################################################################################################
########## #Export a table with all unique site IDs, period of records in LST format, and coordinates to grab LST in python #####
#################################################################################################################################

############ 
#Collate all the data logger info in a list
collate_dat_sites <- function(region_name, root_dir) {
  require(dplyr)
  require(data.table)
  #List files/directories in directory
  site_doc_list <- list.dirs(root_dir, full.names = F, recursive = F)
  #Iterate over every directory within directory
  alltemp <- lapply(site_doc_list[grep(region_name, site_doc_list)], function(x) {
    #Create path
    temp_period <- paste(root_dir, x, sep = "/")
    #List files within directory
    temp_period_list <- list.files(temp_period)
    #Identify table containing water quality data
    temp_period_datatab <- temp_period_list[grep("WaterQuality_Project", temp_period_list)]
    #Create path for table
    temp_period_datatab_path <- paste(temp_period, temp_period_datatab, sep = "/")
    #Read table
    temp_outpop <- read.csv(temp_period_datatab_path)
    #Return table
    temp_outpop
  })
  #Merge all tables within a dataframe
  return(as.data.frame(rbindlist(alltemp)))
}

SitesInfo_JD <- collate_dat_sites(root_dir = getwd(), region_name = "ISEMP")
write.csv(SitesInfo_JD, "Collated/SitesInfo_JD.csv")
SitesInfo_JD <- read.csv("Collated/SitesInfo_JD.csv")

#############
#Remove duplicates 
str(SitesInfo_JD)
#Returning just the duplicates
dupli <- SitesInfo_JD[duplicated(SitesInfo_JD[,c("SITE_IDENTIFIER")]),]
tripli <- dupli[duplicated(dupli[,c("SITE_IDENTIFIER")]),]
quadrupli <- tripli[duplicated(tripli[,c("SITE_IDENTIFIER")]),]

#Remove duplicates
SitesInfo_JD_nodupli <-SitesInfo_JD[!duplicated(SitesInfo_JD[,c("SITE_IDENTIFIER")]),] 

#Test whether all sites with data have info
unique_sites <- rbind(MFJD[!duplicated(MFJD[,"SITE_IDENTIFIER"]),],
                      NFJD[!duplicated(NFJD[,"SITE_IDENTIFIER"]),],
                      UJD[!duplicated(UJD[,"SITE_IDENTIFIER"]),],
                      LJD[!duplicated(LJD[,"SITE_IDENTIFIER"]),])
#Only look at those with water temperature data (exclude other variables)
unique_sites_noair <- unique_sites[unique_sites$MethodName == "Water Temperature",]

site_datainfo_merge <- merge(unique_sites_noair, SitesInfo_JD_nodupli, by = "SITE_IDENTIFIER", all.x = T)
#9 sites for which there are no information
site_datainfo_nomerge <- site_datainfo_merge[is.na(site_datainfo_merge$SiteLatitudeDD),]

#Export sites info to extract daily LST values and other local characteristics
write.csv(SitesInfo_JD_nodupli, "Collated/SitesInfo_JD_nodupli.csv")
#Took out bridge meyers camp because buggy coordinates
SitesInfo_JD_nodupli <- read.csv("Collated/SitesInfo_JD_nodupli_woutbridgemeyers.csv")


#################################################################################################################################
#################################################################################################################################

#In Python, extract daily temperature data from LST based on period of records for each site
#In Python, extract other site characteristics

#See LST downloads python script 
MODLST_1 <- read.csv(file.path(modisdat,'LST_loggers/LSTrecords_MOD2000066_MOD2010175.csv'))
MODLST_2 <- read.csv(file.path(modisdat,'LST_loggers/LSTrecords_MOD2010176_MOD2016290.csv'))

# str(MODLST_2)
MODLST_2_clean <- dplyr::select(MODLST_2, -OBJECTID, -ID, -SiteLatitudeDD, -SiteLongitudeDD)
# 
# MODLST <- merge(MODLST_1, MODLST_2_clean, by = "SITE_IDENTIFIER")
# 
# #Inspect whether there are missing days/columns
# MODLST_cols <- colnames(MODLST[,6:ncol(MODLST)])
# MODLST_date <- data.frame(year = substr(MODLST_cols, 4,7), day = as.numeric(substr(MODLST_cols, 8,10)))
# MODLST_date$interv <- MODLST_date$day - shift(MODLST_date$day, 1)
# MODLST_miss <- MODLST_date[MODLST_date$interv > 1 & !is.na(MODLST_date$interv),]
# #Missing 171 days of record
# sum(MODLST_miss$interv) - nrow(MODLST_miss)
# #Should have 6070 days of record
# (366-64+365+365+365+366+365+365+365+366+365+365+365+366+365+365+365+290)
#None of the missing days of in the LST_record extracted and projected folder
#Aside from the 100 day gap in 2011, all missing days of records are missing from the original LST data
#This must be due to the chosen bounding box having not a single pixel with data for these days
#So should have a total of 6000 records

#After re-download
MODLST_3 <- read.csv(file.path(modisdat,'LST_loggers/LSTrecords_MOD2011107_MOD2011206.csv'))
MODLST_3_clean <- dplyr::select(MODLST_3, -OBJECTID, -ID, -SiteLatitudeDD, -SiteLongitudeDD)
MODLST <- merge(merge(MODLST_1, MODLST_2_clean, by='SITE_IDENTIFIER'), MODLST_3_clean, by='SITE_IDENTIFIER')

#Clean columns
MODLST <- dplyr::select(MODLST, -OBJECTID, -ID, -SiteLatitudeDD, -SiteLongitudeDD)
#Melt data
MODLST_melt <- melt(MODLST, id = 'SITE_IDENTIFIER')
#Re-place 0 by NAs
MODLST_melt[MODLST_melt$value == 0,"value"] <- NA
#Re-scale temperature dat
MODLST_melt$value <- MODLST_melt$value*0.02-273.15
summary(MODLST_melt$value)

MODLST_melt$date <- paste(substr(MODLST_melt$variable , 4,7), as.character(as.numeric(substr(MODLST_melt$variable, 8,10))), sep = '/')
MODLST_melt$date <- as.POSIXlt(MODLST_melt$date, format = "%Y/%j")


#################################################################################################################################
#################################################################################################################################
#Join everything in the same dataframe

#Merge the two datasets
LST_ISEMP_merge <- merge(JD_logdat, MODLST_melt, all.x = T, by = c("SITE_IDENTIFIER", "date"))
write.csv(LST_ISEMP_merge, "Daily_mean_data/LST_ISEMP_merge2.csv")
LST_ISEMP_merge <- read.csv("Daily_mean_data/LST_ISEMP_merge2.csv")

#Edited again based on obvious anomalies in air-temp relationships
LST_ISEMP_merge <- read.csv("Daily_mean_data/LST_ISEMP_merge2_edit.csv")

#How many NA for LST?
ISEMP_noLST <- nrow(LST_ISEMP_merge[is.na(LST_ISEMP_merge$value),])
ISEMP_noLST/nrow(LST_ISEMP_merge)

#Average size of the gaps?

#Import GIS attributes
sites_attrib <- read.csv("Collated/Sites_GISattributes.csv")
LST_ISEMP_merge_attri <- merge(LST_ISEMP_merge, sites_attrib, by = 'SITE_IDENTIFIER')

LST_ISEMP_merge_attri$PctFstWs <-  with(LST_ISEMP_merge_attri, PctDecid2011Ws+PctConif2011Ws  +PctMxFst2011Ws)
LST_ISEMP_merge_attri$PctFst100 <-  with(LST_ISEMP_merge_attri, PctDecid2011CatRp100 +PctConif2011CatRp100   + PctMxFst2011CatRp100)

#Add Julian day
LST_ISEMP_merge_attri$Julday <- as.POSIXlt(LST_ISEMP_merge_attri$date, "%Y-%m-%d")$yday
#Add year
LST_ISEMP_merge_attri$year <- format(as.Date(LST_ISEMP_merge_attri$date, format = "%Y-%m-%d"), "%Y")
#Add quadratic term
LST_ISEMP_merge_attri$value2 <- LST_ISEMP_merge_attri$value^2
#Calculate log temp + continuity correction
LST_ISEMP_merge_attri$logdailymean <- log(LST_ISEMP_merge_attri$dailymean+0.1)

#Take just those records with LST values for developing model
LST_ISEMP_merge_attri_noNA <- LST_ISEMP_merge_attri[!is.na(LST_ISEMP_merge_attri$value),]

#Check which Julian day is on average the day of maximum temperature
LST_ISEMP_merge_attri_noNA_julrec <- ddply(LST_ISEMP_merge_attri_noNA[,c("Julday", "SITE_IDENTIFIER", "dailymean")], .(Julday), summarize, meantemp = mean(dailymean))
LST_ISEMP_merge_attri_noNA_julrec$records <- as.vector(table(LST_ISEMP_merge_attri_noNA$Julday))
LST_ISEMP_merge_attri_noNA_julrec$avg <- rollmean(LST_ISEMP_merge_attri_noNA_julrec$meantemp, 3, na.pad = TRUE)
LST_ISEMP_merge_attri_noNA_julrec[LST_ISEMP_merge_attri_noNA_julrec$avg == max(LST_ISEMP_merge_attri_noNA_julrec$avg, na.rm = T),]

ggplot(LST_ISEMP_merge_attri_noNA_julrec, aes(x = Julday, y = avg)) + geom_line()
ggplot(LST_ISEMP_merge_attri_noNA_julrec, aes(x = Julday, y = records)) + geom_line(color="red")

#####################################################################
#Check linear relationships
qplot(LST_ISEMP_merge_attri$value)
qplot(LST_ISEMP_merge_attri$dailymean)
qplot(sqrt(LST_ISEMP_merge_attri$dailymean))

#Elevation within catchment
qplot(LST_ISEMP_merge_attri$ElevCat)
ggplot(LST_ISEMP_merge_attri, aes(x = ElevCat, y = dailymean)) + geom_point()
mod1 <- lm(dailymean~value + ElevCat, data = LST_ISEMP_merge_attri_noNA)
summary(mod1)

#Elevation within watershed
qplot(LST_ISEMP_merge_attri$ElevWs)
ggplot(LST_ISEMP_merge_attri, aes(x = ElevWs, y = dailymean)) + geom_point()
mod2 <- lm(dailymean~value + ElevWs, data = LST_ISEMP_merge_attri_noNA)
summary(mod2)

#Stream maximum elevation
qplot(LST_ISEMP_merge_attri$MAXELEVSMO)
ggplot(LST_ISEMP_merge_attri, aes(x = MAXELEVSMO, y = dailymean)) + geom_point()
mod3 <- lm(dailymean~value + MAXELEVSMO, data = LST_ISEMP_merge_attri_noNA)
summary(mod3)

#Stream slope
qplot(LST_ISEMP_merge_attri$SLOPE)
qplot(log10(LST_ISEMP_merge_attri$SLOPE))
qplot(sqrt(LST_ISEMP_merge_attri$SLOPE))
ggplot(LST_ISEMP_merge_attri, aes(x = sqrt(SLOPE), y = dailymean)) + geom_point()
mod4 <- lm(dailymean~value + sqrt(SLOPE), data = LST_ISEMP_merge_attri_noNA)
summary(mod4)

#Watershed area
qplot(LST_ISEMP_merge_attri$TotDASqKM)
qplot(log10(LST_ISEMP_merge_attri$TotDASqKM))
ggplot(LST_ISEMP_merge_attri, aes(x = log10(TotDASqKM), y = dailymean, label = SITE_IDENTIFIER)) + geom_point() + geom_text(check_overlap = T)
mod5 <- lm(dailymean~value + NLCD2011_Region17_csv_WsAreaSqKm, data = LST_ISEMP_merge_attri_noNA)
mod5 <- lm(dailymean~value + log10(NLCD2011_Region17_csv_WsAreaSqKm), data = LST_ISEMP_merge_attri_noNA)
summary(mod5)

#Percentage forest in watershed
qplot(LST_ISEMP_merge_attri$PctFstWs)
ggplot(LST_ISEMP_merge_attri, aes(x = PctFstWs, y = dailymean)) + geom_point()
mod6 <- lm(dailymean~value + PctFstWs, data = LST_ISEMP_merge_attri_noNA)
summary(mod6)

#Percentage forest within 100m of river in catchment
qplot(LST_ISEMP_merge_attri$PctFst100)
ggplot(LST_ISEMP_merge_attri, aes(x = PctFst100 , y = dailymean)) + geom_point()
mod7 <- lm(dailymean~value + PctFst100, data = LST_ISEMP_merge_attri_noNA)
summary(mod7)


#Percentage woody wetland within 100m of river in catchment
qplot(LST_ISEMP_merge_attri$PctWdWet2011CatRp100)
ggplot(LST_ISEMP_merge_attri, aes(x = PctWdWet2011CatRp100  , y = dailymean)) + geom_point()

#Housing density within catchment and within watershed
qplot(LST_ISEMP_merge_attri$HUDen2010Cat)
ggplot(LST_ISEMP_merge_attri, aes(x = HUDen2010Cat  , y = dailymean)) + geom_point()
mod8 <- lm(dailymean~value + log10(HUDen2010Cat), data = LST_ISEMP_merge_attri_noNA)
summary(mod8)

qplot(LST_ISEMP_merge_attri$HUDen2010Ws)
qplot(log10(LST_ISEMP_merge_attri$HUDen2010Ws))
ggplot(LST_ISEMP_merge_attri, aes(x = HUDen2010Ws  , y = dailymean)) + geom_point()
mod8 <- lm(dailymean~value + log10(HUDen2010Ws), data = LST_ISEMP_merge_attri_noNA)
summary(mod8)

##############################################################################################
#TRY MODELS
##############################################################################################

ggplot(LST_ISEMP_merge_attri_noNA, aes(x = value, y = dailymean, color = as.character(HUC_8))) + geom_point(alpha = 1/3)

#Simple model with just LST
lm_globalT <- lm(dailymean~value, data = LST_ISEMP_merge_attri_noNA)
summary(lm_globalT)
AIC(lm_globalT)
vif(lm_globalT)
train_control <- trainControl(method="repeatedcv", number=2, repeats=100)
lmCV_globalT <- train(dailymean~value, data = LST_ISEMP_merge_attri_noNA, trControl = train_control, method = "lm")
print(lmCV_globalT)

globalT_res <- resid(lm_globalT)
outliers <- outlierTest(lm_globalT) 
LST_ISEMP_merge_attri[outliers,]

#Model with LST + LST2
lm_globalT2 <- lm(dailymean~ value + value2, data = LST_ISEMP_merge_attri_noNA)
summary(lm_globalT2)
AIC(lm_globalT2)
vif(lm_globalT2)
lmCV_globalT2 <- train(dailymean~value+ value2, data = LST_ISEMP_merge_attri_noNA, trControl = train_control, method = "lm")
print(lmCV_globalT2)

#Model with LST + Julian day
lm_globalT3 <- lm(dailymean~ value + Julday, data = LST_ISEMP_merge_attri_noNA)
summary(lm_globalT3)
AIC(lm_globalT3)
vif(lm_globalT3)
lmCV_globalT3 <- train(dailymean~value+ Julday, data = LST_ISEMP_merge_attri_noNA, trControl = train_control, method = "lm")
print(lmCV_globalT3)

###############################
# MODELS FOR SPRING
################################

#Model with LST + Julian day for Spring
lm_globalT4 <- lm(dailymean~ value + Julday, data = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday < 210,])
summary(lm_globalT4)
AIC(lm_globalT4)
vif(lm_globalT4)
lmCV_globalT4 <- train(dailymean~value+ Julday, data = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday < 210,], trControl = train_control, method = "lm")
print(lmCV_globalT4)

#Model with LST + LST2 + Julian day for Spring
lm_globalT5 <- lm(dailymean~ value + value2 + Julday, data = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday < 210,])
summary(lm_globalT5)
AIC(lm_globalT5)
vif(lm_globalT5)
lmCV_globalT5 <- train(dailymean~value+ + value2 +Julday, data = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday < 210,], trControl = train_control, method = "lm")
print(lmCV_globalT5)
lm_out5 <- data.frame(preds = predict(lm_globalT5), ref = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday < 210, "dailymean"])
ggplot(lm_out5, aes(x = preds, y = ref)) + geom_point() + geom_abline(slope = 1, intercept = 0, color = "red")


#Model with LST + LST2 + Julian day + Watershed Area for Spring
lm_globalT6 <- lm(dailymean~ value + value2 + Julday + log10(TotDASqKM) , data = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday < 210,])
summary(lm_globalT6)
AIC(lm_globalT6)
vif(lm_globalT6)
lmCV_globalT6 <- train(dailymean~value+ + value2 +Julday+ log10(TotDASqKM), data = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday < 210,], trControl = train_control, method = "lm")
print(lmCV_globalT6)
lm_out6 <- data.frame(preds = predict(lm_globalT6), ref = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday < 210, "dailymean"])
ggplot(lm_out6, aes(x = preds, y = ref)) + geom_point() + geom_abline(slope = 1, intercept = 0, color = "red")


#Model with LST + LST2 + Julian day + Watershed Area + Reach elevation for Spring
lm_globalT7 <- lm(dailymean~ value + value2 + Julday + log10(TotDASqKM) +MAXELEVSMO, data = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday < 210,])
summary(lm_globalT7)
AIC(lm_globalT7)
vif(lm_globalT7)
lmCV_globalT7 <- train(dailymean~value+ + value2 +Julday+ log10(TotDASqKM) +MAXELEVSMO, data = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday < 210,], trControl = train_control, method = "lm")
print(lmCV_globalT7)
lm_out7 <- data.frame(preds = predict(lm_globalT7), ref = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday < 210, "dailymean"])
ggplot(lm_out7, aes(x = preds, y = ref)) + geom_point() + geom_abline(slope = 1, intercept = 0, color = "red")

qplot(x = value, y =  (0.01567*value + 0.001984*value^2 - 2.357),data = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday < 210,])

#Model with LST + LST2 + Julian day + Watershed Area + Reach elevation + Slope for Spring
lm_globalT8 <- lm(dailymean~ value + value2 + Julday + TotDASqKM +MAXELEVSMO + sqrt(SLOPE), data = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday < 210,])
summary(lm_globalT8)
AIC(lm_globalT8)
vif(lm_globalT8)
lmCV_globalT8 <- train(dailymean~value+ + value2 +Julday+ TotDASqKM +MAXELEVSMO + sqrt(SLOPE), data = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday < 210,], trControl = train_control, method = "lm")
print(lmCV_globalT8)
lm_out8 <- data.frame(preds = predict(lm_globalT8), ref = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday < 210, "dailymean"])
ggplot(lm_out8, aes(x = preds, y = ref)) + geom_point() + geom_abline(slope = 1, intercept = 0, color = "red")

#Model with LST2 + Julian day + log10(watershed area) + Elevation + sqrt(Slope) for Spring
lm_globalT9 <- lm(dailymean~ value2 +  Julday + log10(TotDASqKM) +MAXELEVSMO + sqrt(SLOPE), data = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday < 210,])
summary(lm_globalT9)
AIC(lm_globalT9)
vif(lm_globalT9)
lmCV_globalT9 <- train(dailymean~value2 +Julday+ log10(TotDASqKM) +MAXELEVSMO + sqrt(SLOPE), data = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday < 210,], trControl = train_control, method = "lm")
print(lmCV_globalT9)
lm_out9 <- data.frame(preds = predict(lm_globalT9), ref = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday < 210, "dailymean"])
ggplot(lm_out9, aes(x = preds, y = ref)) + geom_point() + geom_abline(slope = 1, intercept = 0, color = "red")

#Model with LST2 + Julian day + log10(watershed area) + Elevation + sqrt(Slope) + Housing density for Spring
lm_globalT10 <- lm(dailymean~ value2 +  Julday + log10(TotDASqKM) +MAXELEVSMO + sqrt(SLOPE) + HUDen2010Ws,
                  data = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday < 210,])
summary(lm_globalT10)
AIC(lm_globalT10)
vif(lm_globalT10)
lmCV_globalT10 <- train(dailymean~value2 +Julday+ log10(TotDASqKM) +MAXELEVSMO + sqrt(SLOPE)+ HUDen2010Ws,
                       data = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday < 210,], trControl = train_control, method = "lm")
print(lmCV_globalT10)

lm_out10 <- data.frame(preds = predict(lm_globalT10), ref = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday < 210, "dailymean"])
ggplot(lm_out10, aes(x = preds, y = ref)) + geom_point() + geom_abline(slope = 1, intercept = 0, color = "red")

#Model with LST2 + Julian day + log10(watershed area) + Elevation + sqrt(Slope) + riaprian Forest cover for Spring
lm_globalT11 <- lm(dailymean~ value2 +  Julday + log10(TotDASqKM) +MAXELEVSMO + sqrt(SLOPE) + PctFst100,
                   data = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday < 210,])
summary(lm_globalT11)
AIC(lm_globalT11)
vif(lm_globalT11)
lmCV_globalT11<- train(dailymean~value2 +Julday+ log10(TotDASqKM) +MAXELEVSMO + sqrt(SLOPE)+ PctFst100,
                       data = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday < 210,], trControl = train_control, method = "lm")
print(lmCV_globalT11)

lm_out11- data.frame(preds = predict(lm_globalT11, ref = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday < 210, "dailymean"]))
ggplot(lm_out11, aes(x = preds, y = ref)) + geom_point() + geom_abline(slope = 1, intercept = 0, color = "red")

#Model with LST2 + Julian day + log10(watershed area) + Elevation + sqrt(Slope) + Forest cover for Spring
lm_globalT12 <- lm(dailymean~ value2 +  Julday + log10(TotDASqKM) +MAXELEVSMO + sqrt(SLOPE) + PctFstWs,
                   data = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday < 210,])
summary(lm_globalT12)
AIC(lm_globalT12)
vif(lm_globalT12)
lmCV_globalT12 <- train(dailymean~value2 +Julday+ log10(TotDASqKM) +MAXELEVSMO + sqrt(SLOPE)+ PctFstWs,
                        data = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday < 210,], trControl = train_control, method = "lm")
print(lmCV_globalT12)

lm_out12 <- data.frame(preds = predict(lm_globalT12), ref = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday < 210, "dailymean"])
ggplot(lm_out12, aes(x = preds, y = ref)) + geom_point() + geom_abline(slope = 1, intercept = 0, color = "red")

#Model with LST2 + Julian day + log10(watershed area) + Elevation + sqrt(Slope) + Forest cover for Spring
lm_globalT13 <- lm(dailymean~ value2 +  Julday + log10(TotDASqKM) + MAXELEVSMO, data = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday < 210,])
summary(lm_globalT13)
AIC(lm_globalT13)
vif(lm_globalT13)
lmCV_globalT13 <- train(dailymean~value2 +Julday+ log10(TotDASqKM) +MAXELEVSMO + sqrt(SLOPE),data = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday < 210,], trControl = train_control, method = "lm")
print(lmCV_globalT13)

lm_out13 <- data.frame(preds = predict(lm_globalT13), 
                       ref = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday < 210, "dailymean"],
                       site = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday < 210, "SITE_IDENTIFIER"])
ggplot(lm_out13, aes(x = preds, y = ref, color = site)) + geom_point() + geom_abline(slope = 1, intercept = 0, color = "red") + theme(legend.position = "none")


################################
# MODELS FOR FALL
################################

#Model with LST + Julian day for Fall
lm_globalT14 <- lm(dailymean~ value + Julday, data = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210,])
summary(lm_globalT14)
AIC(lm_globalT14)
vif(lm_globalT14)
lmCV_globalT14<- train(dailymean~value+ Julday, data = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210,], trControl = train_control, method = "lm")
print(lmCV_globalT14)

#Model with LST + LST2 + Julian day for Fall
lm_globalT15 <- lm(dailymean~ value + value2 + Julday, data = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210,])
summary(lm_globalT15)
AIC(lm_globalT15)
vif(lm_globalT15)
lmCV_globalT15<- train(dailymean~value+ value2 + Julday, data = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210,], trControl = train_control, method = "lm")
print(lmCV_globalT15)
lm_out15 <- data.frame(preds = predict(lm_globalT15, ref = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210, "dailymean"], site =  LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210, "SITE_IDENTIFIER"], date = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210, "date"]))
ggplot(lm_out15, aes(x = preds, y = ref, label = paste(site, as.character(date), sep = "-"))) + geom_point() + geom_abline(slope = 1, intercept = 0, color = "red") + geom_text(size = 2, check_overlap = T)

#Model with LST + LST2 + Julian day + watershed area for Fall
lm_globalT16 <- lm(dailymean~ value + value2 + Julday + log10(TotDASqKM) , data = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210,])
summary(lm_globalT16)
AIC(lm_globalT16)
vif(lm_globalT16)
lmCV_globalT16<- train(dailymean~value+ value2 + Julday+ log10(TotDASqKM), data = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210,], trControl = train_control, method = "lm")
print(lmCV_globalT16)
lm_out16 <- data.frame(preds = predict(lm_globalT16), ref = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210, "dailymean"], site =  LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210, "SITE_IDENTIFIER"], date = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210, "date"])
ggplot(lm_out16, aes(x = preds, y = ref, label = paste(site, as.character(date), sep = "-"))) + geom_point() + geom_abline(slope = 1, intercept = 0, color = "red") + geom_text(size = 2, check_overlap = T)
                
#Model with LST + LST2 + Julian day + watershed area + Elevation for Fall
lm_globalT17 <- lm(dailymean~ value + value2 + Julday + log10(TotDASqKM) +MAXELEVSMO, data = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210,])
summary(lm_globalT17)
AIC(lm_globalT17)
vif(lm_globalT17)
lmCV_globalT17<- train(dailymean~value+ value2 + Julday+ log10(TotDASqKM)+MAXELEVSMO, data = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210,], trControl = train_control, method = "lm")
print(lmCV_globalT17)
lm_out17 <- data.frame(preds = predict(lm_globalT17), ref = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210, "dailymean"], site =  LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210, "SITE_IDENTIFIER"], date = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210, "date"])
ggplot(lm_out17, aes(x = preds, y = ref, label = paste(site, as.character(date), sep = "-"))) + geom_point() + geom_abline(slope = 1, intercept = 0, color = "red") + geom_text(size = 2, check_overlap = T)

#Model with LST + LST2 + Julian day + watershed area + Elevation + Slope for Fall
lm_globalT18 <- lm(dailymean~ value + value2 + Julday + log10(TotDASqKM) +MAXELEVSMO + sqrt(SLOPE), data = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210,])
summary(lm_globalT18)
AIC(lm_globalT18)
vif(lm_globalT18)
lmCV_globalT18<- train(dailymean~value+ value2 + Julday+ log10(TotDASqKM)+MAXELEVSMO + sqrt(SLOPE), data = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210,], trControl = train_control, method = "lm")
print(lmCV_globalT18)
lm_out18 <- data.frame(preds = predict(lm_globalT18), ref = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210, "dailymean"], site =  LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210, "SITE_IDENTIFIER"], date = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210, "date"])
ggplot(lm_out18, aes(x = preds, y = ref, label = paste(site, as.character(date), sep = "-"))) + geom_point() + geom_abline(slope = 1, intercept = 0, color = "red") + geom_text(size = 2, check_overlap = T)

#Model with LST + LST2 + Julian day + watershed area + Elevation + Slope + Housing density for Fall
lm_globalT19 <- lm(dailymean~ value + value2 + Julday + log10(TotDASqKM) +MAXELEVSMO +  HUDen2010Ws, data = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210,])
summary(lm_globalT19)
AIC(lm_globalT19)
vif(lm_globalT19)
lmCV_globalT19<- train(dailymean~value+ value2 + Julday+ log10(TotDASqKM)+MAXELEVSMO + HUDen2010Ws, data = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210,], trControl = train_control, method = "lm")
print(lmCV_globalT19)
lm_out19 <- data.frame(preds = predict(lm_globalT19), ref = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210, "dailymean"], site =  LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210, "SITE_IDENTIFIER"], date = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210, "date"])
ggplot(lm_out19, aes(x = preds, y = ref, label = paste(site, as.character(date), sep = "-"))) + geom_point() + geom_abline(slope = 1, intercept = 0, color = "red") + geom_text(size = 2, check_overlap = T)
                       

#Model with LST + LST2 + Julian day + watershed area + Elevation + Slope for Spring
lm_globalT20 <- lm(dailymean~ value + value2 + Julday + log10(TotDASqKM) +MAXELEVSMO +  PctFstWs, data = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210,])
summary(lm_globalT20)
AIC(lm_globalT20)
vif(lm_globalT20)
lmCV_globalT20<- train(dailymean~value+ value2 + Julday+ log10(TotDASqKM)+MAXELEVSMO + PctFstWs, data = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210,], trControl = train_control, method = "lm")
print(lmCV_globalT20)
lm_out20 <- data.frame(preds = predict(lm_globalT20), ref = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210, "dailymean"], site =  LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210, "SITE_IDENTIFIER"], date = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210, "date"])
ggplot(lm_out20, aes(x = preds, y = ref)) + geom_point() + geom_abline(slope = 1, intercept = 0, color = "red")

#Model with LST + LST2 + Julian day + watershed area + Elevation + Slope for Spring
lm_globalT20 <- lm(dailymean~ value + value2 + Julday + log10(TotDASqKM) +MAXELEVSMO +  PctFstWs, data = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210,])
summary(lm_globalT20)
AIC(lm_globalT20)
vif(lm_globalT20)
lmCV_globalT20<- train(dailymean~value+ value2 + Julday+ log10(TotDASqKM)+MAXELEVSMO + PctFstWs, data = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210,], trControl = train_control, method = "lm")
print(lmCV_globalT20)
lm_out20 <- data.frame(preds = predict(lm_globalT20), ref = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210, "dailymean"], site =  LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210, "SITE_IDENTIFIER"], TotDASqKM = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210, "TotDASqKM"])
ggplot(lm_out20, aes(x = preds, y = ref, color = TotDASqKM)) + geom_point() + geom_abline(slope = 1, intercept = 0, color = "red")+ xlim(0,35) + ylim(0,35)

#Model with LST + Julian day + watershed area + Elevation for Fall
lm_globalT21 <- lm(dailymean~ value + Julday + log10(TotDASqKM) +MAXELEVSMO, data = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210,])
summary(lm_globalT21)
AIC(lm_globalT21)
vif(lm_globalT21)
lmCV_globalT21<- train(dailymean~value+ value2 + Julday+ log10(TotDASqKM)+MAXELEVSMO, data = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210,], trControl = train_control, method = "lm")
print(lmCV_globalT21)
lm_out21 <- data.frame(preds = predict(lm_globalT21), 
                       ref = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210, "dailymean"], 
                       site =  LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210, "SITE_IDENTIFIER"], 
                       date = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210, "date"],
                       elev = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210, "MAXELEVSMO"])
ggplot(lm_out21, aes(x = preds, y = ref, label = paste(site, as.character(date), sep = "-"), color = site)) + geom_point() + geom_abline(slope = 1, intercept = 0, color = "red") + theme(legend.position = "none")
+ geom_text(size = 2, check_overlap = T)




#Model with LST + LST2 + Julian day + watershed area + Elevation + Slope for Fall
lm_globalT22 <- lm(logdailymean~ value + value2 + Julday + log10(TotDASqKM) +MAXELEVSMO +  PctFstWs, data = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210,])
summary(lm_globalT22)
AIC(lm_globalT22)
vif(lm_globalT22)
lmCV_globalT22<- train(dailymean~value+ value2 + Julday+ log10(TotDASqKM)+MAXELEVSMO + PctFstWs, data = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210,], trControl = train_control, method = "lm")
print(lmCV_globalT22)
lm_out22 <- data.frame(preds = exp(predict(lm_globalT22)), ref = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210, "dailymean"], site =  LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210, "SITE_IDENTIFIER"], date = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210, "date"])
ggplot(lm_out22, aes(x = preds, y = ref)) + geom_point() + geom_abline(slope = 1, intercept = 0, color = "red") + xlim(0,35) + ylim(0,35)

#Model with LST + LST2 + Julian day + watershed area + Elevation + Slope for Fall
lm_globalT23 <- lm(dailymean~ value + value2 + Julday + log10(TotDASqKM) +MAXELEVSMO +  PctFstWs, data = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210 & LST_ISEMP_merge_attri_noNA$value > 5,])
summary(lm_globalT23)
AIC(lm_globalT23)
vif(lm_globalT23)
lmCV_globalT23 <- train(dailymean~value+ value2 + Julday+ log10(TotDASqKM)+MAXELEVSMO + PctFstWs, data = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210& LST_ISEMP_merge_attri_noNA$value > 5,], trControl = train_control, method = "lm")
print(lmCV_globalT23)
lm_out23 <- data.frame(preds = predict(lm_globalT23), ref = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210 & LST_ISEMP_merge_attri_noNA$value > 5, "dailymean"], site =  LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210& LST_ISEMP_merge_attri_noNA$value > 5, "SITE_IDENTIFIER"], date = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210& LST_ISEMP_merge_attri_noNA$value > 5, "date"])
ggplot(lm_out23, aes(x = preds, y = ref)) + geom_point() + geom_abline(slope = 1, intercept = 0, color = "red")+ xlim(0,35) + ylim(0,35)

#Model with LST + LST2 + Julian day + watershed area + Elevation + Slope for Fall
lm_globalT24 <- lm(dailymean~ value + value2 + Julday + log10(TotDASqKM) +MAXELEVSMO +  PctFstWs, data = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210 & LST_ISEMP_merge_attri_noNA$TotDASqKM > 2000,])
summary(lm_globalT24)
AIC(lm_globalT24)
vif(lm_globalT24)
lmCV_globalT24 <- train(dailymean~value+ value2 + Julday+ log10(TotDASqKM)+MAXELEVSMO + PctFstWs, data = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210& LST_ISEMP_merge_attri_noNA$TotDASqKM > 2000,], trControl = train_control, method = "lm")
print(lmCV_globalT24)
lm_out24<- data.frame(preds = predict(lm_globalT24), ref = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210 & LST_ISEMP_merge_attri_noNA$TotDASqKM > 2000, "dailymean"], site =  LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210& LST_ISEMP_merge_attri_noNA$TotDASqKM > 2000, "SITE_IDENTIFIER"], date = LST_ISEMP_merge_attri_noNA[LST_ISEMP_merge_attri_noNA$Julday >= 210& LST_ISEMP_merge_attri_noNA$TotDASqKM >2000, "date"])
ggplot(lm_out24, aes(x = preds, y = ref, color = as.POSIXct(date), label = site)) + geom_point() + geom_abline(slope = 1, intercept = 0, color = "red")+ xlim(0,35) + ylim(0,35) + geom_text(size = 2)

ggplot(lm_out24, aes(x = preds, y = ref, color = as.POSIXct(date), label = date)) + geom_point() + geom_abline(slope = 1, intercept = 0, color = "red")+ xlim(0,35) + ylim(0,35) + geom_text(size = 2)

#Compare different years to the end of 2009
LST_ISEMP_merge_attri_noNA$date <- as.Date(LST_ISEMP_merge_attri_noNA$date)
LST_ISEMP_merge_attri_noNA_sub2009 <- subset(LST_ISEMP_merge_attri_noNA, date > "2009-08-01" & date < "2010-01-01")
ts2009 <- ggplot(LST_ISEMP_merge_attri_noNA_sub2009 , aes(x = date, y = dailymean, group = SITE_IDENTIFIER, color = TotDASqKM, label = SITE_IDENTIFIER)) + 
  geom_line(size = 2) + theme(legend.position="none") + geom_text(size = 2, check_overlap = T)
LST_ISEMP_merge_attri_noNA_sub2003 <- subset(LST_ISEMP_merge_attri_noNA, date > "2000-08-01" & date < "2001-01-01")
ts2003 <- ggplot(LST_ISEMP_merge_attri_noNA_sub2003 , aes(x = date, y = dailymean, group = SITE_IDENTIFIER, color = TotDASqKM, label = SITE_IDENTIFIER)) + 
  geom_line(size = 2) + theme(legend.position="none")+ geom_text(size = 2, check_overlap = T)
grid.arrange(ts2003, ts2009, ncol = 2)


adply(MODLST_melt[1:100,], 1, function(x) {
  if (is.na(x$value)) {
    print(max(which(MODLST_melt[1:100,]$date < x$date)))
  }
})
