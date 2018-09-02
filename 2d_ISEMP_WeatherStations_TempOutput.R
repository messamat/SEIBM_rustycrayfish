#Author: Mathis L. Messager
#Related publication: Messager ML, Olden JD. Individual-based models forecast the spread and inform the management 
#                                             of an emerging riverine invader. Divers Distrib.2018;00:1-14. https://doi.org/10.1111/ddi.12829
#Purpose: Clean, format, and join water temperature data and air temperature data (1999).
#         Develop model of water temperature data based on air temperature. Join water temperature predictions for 1999 and 2000-2016 and output dataset ready for HexSim model
#Produce: Table S2.1

rootdir <- 'F:/Chapter2_HexSim_Crayfish'
setwd(file.path(rootdir,'Data'))

library(reshape)
library(ggplot2)
library(data.table)
library(car)
library(caret)
library(plyr)
library(dplyr)
#
JD_logdat <- read.csv("ISEMP/Daily_mean_data/JD_logdat2.csv")
sites_attrib <- read.csv("ISEMP/Collated/Sites_GISattributes.csv")

#Downloaded date from 16 stations near the John Day River from https://www.ncdc.noaa.gov/cdo-web/search (Search for John Day River as Hydrologic Accounting Units)
#Ruesck used Case, Board Creek, Fall Mountain, and North Pole Ridge.
stationdat <- read.csv('ISEMP/NOAA_weatherstation_data/station_dat.csv')
stationdat$DATE <- as.Date(as.character(stationdat$DATE), format = "%Y%m%d")
str(stationdat)
unique(stationdat$STATION_NAME)
stationdat[stationdat == -9999] <- NA
ggplot(stationdat, aes(x = TAVG, fill = STATION_NAME)) + geom_histogram() + scale_y_log10()

#Check completeness of data
stationdat_dt <- as.data.table(stationdat)
stationdat_dt_count <- stationdat_dt[,
                                     .(NAcount = length(which(is.na(TAVG))), 
                                        dat_compl_NA = length(which(is.na(TAVG)))/.N, 
                                        dat_compl_temp = .N/6515), 
                                     by = STATION_NAME]
qplot(stationdat_dt[STATION_NAME == "SLIDE MOUNTAIN OREGON OR US", TAVG])
stationdat_dt[STATION_NAME == "SLIDE MOUNTAIN OREGON OR US" & TAVG > 30,]
#All data points above 30C for SLIDE MOUNTAIN are erroneous (occur in March -- must be bad conversion from Farenheit)
stationdat_dt[STATION_NAME == "FALL MOUNTAIN OREGON OR US" & TAVG < -25,]
#All data points below -25 for FALL MOUNTAIN are erroneous (occurr in May and June)
stationdat_dt <- stationdat_dt[!(STATION_NAME == "SLIDE MOUNTAIN OREGON OR US" & TAVG > 30) & 
                                 !(STATION_NAME == "FALL MOUNTAIN OREGON OR US" & TAVG < -25),]
#Take out those stations with a lot of NAs
stationdat_dt_clean <- stationdat_dt[(stationdat_dt$STATION_NAME %in% stationdat_dt_count[stationdat_dt_count$dat_compl_NA != 1,STATION_NAME]),]

#Cast data
stationdat_cast <- cast(stationdat_dt_clean, DATE~STATION_NAME, value = "TAVG")

#Interpolate NAs linearly
stationdat_cast <- timeSeries::interpNA(stationdat_cast, "linear")
#Keep this space replacement after interpNA because it the interpolation essentially undo it
colnames(stationdat_cast) <- gsub(" ", "_", colnames(stationdat_cast))
#Check which column has NA values which could essentially just be done with summary
colnames(stationdat_cast)[colSums(is.na(stationdat_cast)) > 0]
summary(stationdat_cast)
stationdat_cast_df<- as.data.frame(stationdat_cast)
#Replace NAs at the beginning KEENEY_TWO_OREGON_OR_US station data by temperature the first day that is not an NA
stationdat_cast_df[is.na(stationdat_cast_df$KEENEY_TWO_OREGON_OR_US), "KEENEY_TWO_OREGON_OR_US"] <- stationdat_cast_df[min(which(!is.na(stationdat_cast_df$KEENEY_TWO_OREGON_OR_US))), "KEENEY_TWO_OREGON_OR_US"]
#Get dates as columns
stationdat_cast_df$date <- row.names(stationdat_cast_df)
#Calculate (Blue_Mountain_Spring_OR_US^2)
stationdat_cast_dfBlue2 <- stationdat_cast_df$Blue_Mountain_Spring_OR_US^2
#Add Julian day
stationdat_cast_df$Julday <- as.POSIXlt(stationdat_cast_df$date, "%Y-%m-%d")$yday
#Add year/month
stationdat_cast_df$date_YM <- format(as.Date(stationdat_cast_df$date, format = "%Y-%m-%d"), "%Y-%m")

#Look at air temperature yearly averages over the period of record
stationdat_cast_df$year <- format(as.Date(stationdat_cast_df$date, format = "%Y-%m-%d"), "%Y")
stationdatcast_yearly <- ddply(stationdat_cast_df, .(year), summarize, 
                               meanATBlue=mean(Blue_Mountain_Spring_OR_US),
                               meanATBoard = mean(BOARD_CREEK_OREGON_OR_US),
                               meanATCase= mean(CASE_OREGON_OR_US),
                               meanATFall= mean(FALL_MOUNTAIN_OREGON_OR_US),
                               meanATKeeney = mean(KEENEY_TWO_OREGON_OR_US),
                               meanATNorth = mean(NORTH_POLE_RIDGE_OREGON_OR_US),
                               meanATSlide=mean(SLIDE_MOUNTAIN_OREGON_OR_US),
                               meanATStarr = mean(Starr_Ridge_OR_US))
stationdatcast_yearly$allaverage <- rowMeans(stationdatcast_yearly[,-1])
stationdatcast_yearly <- stationdatcast_yearly[order(stationdatcast_yearly[,10]),]

########################################################MODEL TESTING###############################################################
###################################################SEE BELOW FOR PREDICTION GENERATION###################################################
#Merge with ISEMP data
JD_logstation_dat <- merge(JD_logdat, stationdat_cast_df, by = "date")
JD_logstation_dat$date <- as.Date(JD_logstation_dat$date)
JD_logstationcovar_dat <- merge(JD_logstation_dat, sites_attrib, by = "SITE_IDENTIFIER", all.x = T)

########## Count of how many stations have data for 1999-2000
unique(JD_logstation_dat[JD_logstation_dat$date > "1999-01-01" & JD_logstation_dat$date < "2000-01-01", "SITE_IDENTIFIER"])

#calculate average temp across stations
JD_logstationcovar_dat$avg_airtemp <- rowMeans(JD_logstationcovar_dat[, c('Blue_Mountain_Spring_OR_US', 
                                                                            'BOARD_CREEK_OREGON_OR_US', 
                                                                            'FALL_MOUNTAIN_OREGON_OR_US' , 
                                                                            'KEENEY_TWO_OREGON_OR_US',
                                                                            'NORTH_POLE_RIDGE_OREGON_OR_US', 
                                                                            'SLIDE_MOUNTAIN_OREGON_OR_US',
                                                                            'Starr_Ridge_OR_US')])

#Calculate total watershed forest cover
JD_logstationcovar_dat$PctFstWs <-  with(JD_logstationcovar_dat, PctDecid2011Ws+PctConif2011Ws+PctMxFst2011Ws)

########################################################################################################################################################
# TRY MODELS

#Try models with all stations as covariates
allstations_mod1 <- lm(dailymean ~ Blue_Mountain_Spring_OR_US + 
                         BOARD_CREEK_OREGON_OR_US + 
                         FALL_MOUNTAIN_OREGON_OR_US + 
                         KEENEY_TWO_OREGON_OR_US + 
                         NORTH_POLE_RIDGE_OREGON_OR_US + 
                         SLIDE_MOUNTAIN_OREGON_OR_US +
                         Starr_Ridge_OR_US, data = JD_logstationcovar_dat) 

summary(allstations_mod1)
AIC(allstations_mod1)
vif(allstations_mod1)
train_control <- trainControl(method="repeatedcv", number=2, repeats=100)
lmCV_allstations_mod1 <- train(dailymean~Blue_Mountain_Spring_OR_US + 
                        BOARD_CREEK_OREGON_OR_US + 
                        FALL_MOUNTAIN_OREGON_OR_US + 
                        KEENEY_TWO_OREGON_OR_US + 
                        NORTH_POLE_RIDGE_OREGON_OR_US + 
                        SLIDE_MOUNTAIN_OREGON_OR_US +
                        Starr_Ridge_OR_US, data = JD_logstationcovar_dat, trControl = train_control, method = "lm")
print(lmCV_allstations_mod1)
#Huge multicollinearity

#Try models with one station at a time
onestation_mod2 <- lm(dailymean ~ Blue_Mountain_Spring_OR_US, data = JD_logstationcovar_dat) 
summary(onestation_mod2)
AIC(onestation_mod2)
lmCV_onestation_mod2 <- train(dailymean~Blue_Mountain_Spring_OR_US, data = JD_logstationcovar_dat, trControl = train_control, method = "lm")
print(lmCV_onestation_mod2)

onestation_mod3 <- lm(dailymean ~ BOARD_CREEK_OREGON_OR_US, data = JD_logstationcovar_dat) 
summary(onestation_mod3)
AIC(onestation_mod3)
lmCV_onestation_mod3 <- train(dailymean~BOARD_CREEK_OREGON_OR_US, data = JD_logstationcovar_dat, trControl = train_control, method = "lm")
print(lmCV_onestation_mod3)

onestation_mod4 <- lm(dailymean ~ Fone_MOUNTAIN_OREGON_OR_US, data = JD_logstationcovar_dat) 
summary(onestation_mod4)
AIC(onestation_mod4)
lmCV_onestation_mod4 <- train(dailymean~Fone_MOUNTAIN_OREGON_OR_US, data = JD_logstationcovar_dat, trControl = train_control, method = "lm")
print(lmCV_onestation_mod4)

onestation_mod5 <- lm(dailymean ~ KEENEY_TWO_OREGON_OR_US, data = JD_logstationcovar_dat) 
summary(onestation_mod5)
AIC(onestation_mod5)
lmCV_onestation_mod5 <- train(dailymean~KEENEY_TWO_OREGON_OR_US, data = JD_logstationcovar_dat, trControl = train_control, method = "lm")
print(lmCV_onestation_mod5)

onestation_mod6 <- lm(dailymean ~ NORTH_POLE_RIDGE_OREGON_OR_US, data = JD_logstationcovar_dat) 
summary(onestation_mod6)
AIC(onestation_mod6)
lmCV_onestation_mod6 <- train(dailymean~NORTH_POLE_RIDGE_OREGON_OR_US, data = JD_logstationcovar_dat, trControl = train_control, method = "lm")
print(lmCV_onestation_mod6)

onestation_mod7 <- lm(dailymean ~ SLIDE_MOUNTAIN_OREGON_OR_US, data = JD_logstationcovar_dat) 
summary(onestation_mod7)
AIC(onestation_mod7)
lmCV_onestation_mod7 <- train(dailymean~SLIDE_MOUNTAIN_OREGON_OR_US, data = JD_logstationcovar_dat, trControl = train_control, method = "lm")
print(lmCV_onestation_mod7)

onestation_mod8 <- lm(dailymean ~ Starr_Ridge_OR_US, data = JD_logstationcovar_dat) 
summary(onestation_mod8)
AIC(onestation_mod8)
lmCV_onestation_mod8<- train(dailymean~Starr_Ridge_OR_US, data = JD_logstationcovar_dat, trControl = train_control, method = "lm")
print(lmCV_onestation_mod8)

#Try model with average of stations' temp at a time
avgstation_mod9 <- lm(dailymean ~ avg_airtemp, data = JD_logstationcovar_dat) 
summary(avgstation_mod9)
AIC(avgstation_mod9)
lmCV_avgstation_mod9<- train(dailymean~avg_airtemp, data = JD_logstationcovar_dat, trControl = train_control, method = "lm")
print(lmCV_avgstation_mod9)

avgstation_mod10 <- lm(dailymean ~ Blue_Mountain_Spring_OR_US+SLIDE_MOUNTAIN_OREGON_OR_US, data = JD_logstationcovar_dat) 
summary(avgstation_mod10)
AIC(avgstation_mod10)
vif(avgstation_mod10)
lmCV_avgstation_mod10<- train(dailymean~Blue_Mountain_Spring_OR_US+SLIDE_MOUNTAIN_OREGON_OR_US, data = JD_logstationcovar_dat, trControl = train_control, method = "lm")
print(lmCV_avgstation_mod10)

##########################
# TRY WITH JUST 1999 and early 2000
JD_logstationcovar_dat9900 <- JD_logstationcovar_dat[JD_logstationcovar_dat$date < "2000-06-01",]
qplot(JD_logstationcovar_dat9900$TotDASqKM)
qplot(JD_logstationcovar_dat$TotDASqKM)

avgstation_mod11 <- lm(dailymean ~ avg_airtemp, data = JD_logstationcovar_dat9900) 
summary(avgstation_mod11)
AIC(avgstation_mod11)
vif(avgstation_mod11)
mod11 <- data.frame(preds = predict(avgstation_mod11), ref = JD_logstationcovar_dat9900[,"dailymean"])
ggplot(mod11 , aes(x = preds, y = ref)) + geom_point() + geom_abline(slope = 1, intercept = 0, color = "red")

avgstation_mod12 <- lm(dailymean ~ Blue_Mountain_Spring_OR_US, data = JD_logstationcovar_dat9900) 
summary(avgstation_mod12)
AIC(avgstation_mod12)
vif(avgstation_mod12)
mod12 <- data.frame(preds = predict(avgstation_mod12), ref = JD_logstationcovar_dat9900[,"dailymean"])
ggplot(mod12 , aes(x = preds, y = ref)) + geom_point() + geom_abline(slope = 1, intercept = 0, color = "red")

avgstation_mod13 <- lm(dailymean ~ Blue_Mountain_Spring_OR_US + Blue2, data = JD_logstationcovar_dat9900) 
summary(avgstation_mod13)
AIC(avgstation_mod13)
vif(avgstation_mod13)
mod13 <- data.frame(preds = predict(avgstation_mod13), ref = JD_logstationcovar_dat9900[,"dailymean"])
ggplot(mod13 , aes(x = preds, y = ref)) + geom_point() + geom_abline(slope = 1, intercept = 0, color = "red")



#TRY FOR SPRING
avgstation_mod14 <- lm(dailymean ~ Blue_Mountain_Spring_OR_US + Blue2, data = JD_logstationcovar_dat9900[JD_logstationcovar_dat9900$Julday < 210,]) 
summary(avgstation_mod14)
AIC(avgstation_mod14)
vif(avgstation_mod14)
mod14 <- data.frame(preds = predict(avgstation_mod14), ref = JD_logstationcovar_dat9900[JD_logstationcovar_dat9900$Julday < 210, "dailymean"])
ggplot(mod14 , aes(x = preds, y = ref)) + geom_point() + geom_abline(slope = 1, intercept = 0, color = "red")

avgstation_mod15 <- lm(dailymean ~ Blue_Mountain_Spring_OR_US + Blue2 + Julday, data = JD_logstationcovar_dat9900[JD_logstationcovar_dat9900$Julday < 210,]) 
summary(avgstation_mod15)
AIC(avgstation_mod15)
vif(avgstation_mod15)
mod15<- data.frame(preds = predict(avgstation_mod15), ref = JD_logstationcovar_dat9900[JD_logstationcovar_dat9900$Julday < 210, "dailymean"])
ggplot(mod15 , aes(x = preds, y = ref)) + geom_point() + geom_abline(slope = 1, intercept = 0, color = "red")

avgstation_mod16 <- lm(dailymean ~ Blue_Mountain_Spring_OR_US + Julday + log10(TotDASqKM), data = JD_logstationcovar_dat9900[JD_logstationcovar_dat9900$Julday < 210,]) 
summary(avgstation_mod16)
AIC(avgstation_mod16)
vif(avgstation_mod16)
mod16<- data.frame(preds = predict(avgstation_mod16), ref = JD_logstationcovar_dat9900[JD_logstationcovar_dat9900$Julday < 210, "dailymean"])
ggplot(mod16 , aes(x = preds, y = ref)) + geom_point() + geom_abline(slope = 1, intercept = 0, color = "red")

avgstation_mod17 <- lm(dailymean ~ Blue_Mountain_Spring_OR_US + Julday + log10(TotDASqKM) + MAXELEVSMO, data = JD_logstationcovar_dat9900[JD_logstationcovar_dat9900$Julday < 210,]) 
summary(avgstation_mod17)
AIC(avgstation_mod17)
vif(avgstation_mod17)
mod17<- data.frame(preds = predict(avgstation_mod17), ref = JD_logstationcovar_dat9900[JD_logstationcovar_dat9900$Julday < 210, "dailymean"])
ggplot(mod17 , aes(x = preds, y = ref)) + geom_point() + geom_abline(slope = 1, intercept = 0, color = "red")

avgstation_mod18 <- lm(dailymean ~ Blue_Mountain_Spring_OR_US + Julday + log10(TotDASqKM) + MAXELEVSMO + sqrt(SLOPE), data = JD_logstationcovar_dat9900[JD_logstationcovar_dat9900$Julday < 210,]) 
summary(avgstation_mod18)
AIC(avgstation_mod18)
vif(avgstation_mod18)
mod18<- data.frame(preds = predict(avgstation_mod18), ref = JD_logstationcovar_dat9900[JD_logstationcovar_dat9900$Julday < 210, "dailymean"])
ggplot(mod18 , aes(x = preds, y = ref)) + geom_point() + geom_abline(slope = 1, intercept = 0, color = "red")

avgstation_mod19 <- lm(dailymean ~ Blue_Mountain_Spring_OR_US + Julday + log10(TotDASqKM) + MAXELEVSMO + HUDen2010Ws, data = JD_logstationcovar_dat9900[JD_logstationcovar_dat9900$Julday < 210,]) 
summary(avgstation_mod19)
AIC(avgstation_mod19)
vif(avgstation_mod19)
mod19<- data.frame(preds = predict(avgstation_mod19), ref = JD_logstationcovar_dat9900[JD_logstationcovar_dat9900$Julday < 210, "dailymean"])
ggplot(mod19 , aes(x = preds, y = ref)) + geom_point() + geom_abline(slope = 1, intercept = 0, color = "red")

avgstation_mod20 <- lm(dailymean ~ Blue_Mountain_Spring_OR_US + Julday + log10(TotDASqKM) + MAXELEVSMO +  PctFstWs, data = JD_logstationcovar_dat9900[JD_logstationcovar_dat9900$Julday < 210,]) 
summary(avgstation_mod20)
AIC(avgstation_mod20)
vif(avgstation_mod20)
mod20<- data.frame(preds = predict(avgstation_mod20), ref = JD_logstationcovar_dat9900[JD_logstationcovar_dat9900$Julday < 210, "dailymean"])
ggplot(mod20 , aes(x = preds, y = ref)) + geom_point() + geom_abline(slope = 1, intercept = 0, color = "red")


#TRY FOR FALL
avgstation_mod21 <- lm(dailymean ~ Blue_Mountain_Spring_OR_US + Blue2, data = JD_logstationcovar_dat9900[JD_logstationcovar_dat9900$Julday >= 210,]) 
summary(avgstation_mod21)
AIC(avgstation_mod21)
vif(avgstation_mod21)
mod21<- data.frame(preds = predict(avgstation_mod21), ref = JD_logstationcovar_dat9900[JD_logstationcovar_dat9900$Julday >= 210, "dailymean"])
ggplot(mod21 , aes(x = preds, y = ref)) + geom_point() + geom_abline(slope = 1, intercept = 0, color = "red")

avgstation_mod22 <- lm(dailymean ~ Blue_Mountain_Spring_OR_US + Blue2 + Julday, data = JD_logstationcovar_dat9900[JD_logstationcovar_dat9900$Julday >= 210,]) 
summary(avgstation_mod22)
AIC(avgstation_mod22)
vif(avgstation_mod22)
mod22<- data.frame(preds = predict(avgstation_mod22), ref = JD_logstationcovar_dat9900[JD_logstationcovar_dat9900$Julday >= 210, "dailymean"])
ggplot(mod22 , aes(x = preds, y = ref)) + geom_point() + geom_abline(slope = 1, intercept = 0, color = "red")

avgstation_mod23 <- lm(dailymean ~ Blue_Mountain_Spring_OR_US + Julday + log10(TotDASqKM), data = JD_logstationcovar_dat9900[JD_logstationcovar_dat9900$Julday >= 210,]) 
summary(avgstation_mod23)
AIC(avgstation_mod23)
vif(avgstation_mod23)
mod23<- data.frame(preds = predict(avgstation_mod23), ref = JD_logstationcovar_dat9900[JD_logstationcovar_dat9900$Julday >= 210, "dailymean"])
ggplot(mod23 , aes(x = preds, y = ref)) + geom_point() + geom_abline(slope = 1, intercept = 0, color = "red")

avgstation_mod24 <- lm(dailymean ~ Blue_Mountain_Spring_OR_US + Julday + log10(TotDASqKM) + MAXELEVSMO, data = JD_logstationcovar_dat9900[JD_logstationcovar_dat9900$Julday >= 210,]) 
summary(avgstation_mod24)
AIC(avgstation_mod24)
vif(avgstation_mod24)
mod24<- data.frame(preds = predict(avgstation_mod24), ref = JD_logstationcovar_dat9900[JD_logstationcovar_dat9900$Julday >= 210, "dailymean"], site = JD_logstationcovar_dat9900[JD_logstationcovar_dat9900$Julday >= 210, "SITE_IDENTIFIER"])
ggplot(mod24 , aes(x = preds, y = ref)) + geom_point() + geom_abline(slope = 1, intercept = 0, color = "red") 
#+ geom_text(aes(label = site))

avgstation_mod25 <- lm(dailymean ~ Blue_Mountain_Spring_OR_US + Julday + log10(TotDASqKM) + MAXELEVSMO + sqrt(SLOPE), data = JD_logstationcovar_dat9900[JD_logstationcovar_dat9900$Julday >= 210,]) 
summary(avgstation_mod25)
AIC(avgstation_mod25)
vif(avgstation_mod25)
mod25 <- data.frame(preds = predict(avgstation_mod25), ref = JD_logstationcovar_dat9900[JD_logstationcovar_dat9900$Julday >= 210, "dailymean"], site = JD_logstationcovar_dat9900[JD_logstationcovar_dat9900$Julday >= 210, "SITE_IDENTIFIER"])
ggplot(mod25 , aes(x = preds, y = ref)) + geom_point() + geom_abline(slope = 1, intercept = 0, color = "red") 

avgstation_mod26 <- lm(dailymean ~ Blue_Mountain_Spring_OR_US + Julday + log10(TotDASqKM) + MAXELEVSMO + PctFstWs, data = JD_logstationcovar_dat9900[JD_logstationcovar_dat9900$Julday >= 210,]) 
summary(avgstation_mod26)
AIC(avgstation_mod26)
vif(avgstation_mod26)
mod26 <- data.frame(preds = predict(avgstation_mod26), ref = JD_logstationcovar_dat9900[JD_logstationcovar_dat9900$Julday >= 210, "dailymean"], site = JD_logstationcovar_dat9900[JD_logstationcovar_dat9900$Julday >= 210, "SITE_IDENTIFIER"])
ggplot(mod26 , aes(x = preds, y = ref)) + geom_point() + geom_abline(slope = 1, intercept = 0, color = "red") 

####TRY FOR ALL
avgstation_mod27 <- lm(dailymean ~ Blue_Mountain_Spring_OR_US + Blue2 + Julday + log10(TotDASqKM) + MAXELEVSMO, data = JD_logstationcovar_dat9900) 
summary(avgstation_mod27)
AIC(avgstation_mod27)
vif(avgstation_mod27)
mod27 <- data.frame(preds = predict(avgstation_mod27), ref = JD_logstationcovar_dat9900$dailymean, site = JD_logstationcovar_dat9900$SITE_IDENTIFIER)
ggplot(mod27 , aes(x = preds, y = ref)) + geom_point() + geom_abline(slope = 1, intercept = 0, color = "red") 


#TEST FINAL MODEL COMBINATION
mod_final<- data.frame(preds = c(predict(avgstation_mod17),predict(avgstation_mod26)), ref = c(JD_logstationcovar_dat9900[JD_logstationcovar_dat9900$Julday < 210, "dailymean"], site = JD_logstationcovar_dat9900[JD_logstationcovar_dat9900$Julday >= 210, "dailymean"]))
ggplot(mod_final , aes(x = preds, y = ref)) + geom_point() + geom_abline(slope = 1, intercept = 0, color = "red") 
rmse <- function(error){sqrt(mean(error^2))}               
rmse(mod_final$ref - mod_final$preds)

########################################################APPLY MODEL TO DATA###############################################################
###################################################PREDICTION GENERATION###################################################
elvslope <- read.csv('MODIS_LST_download/LST_NHD/NHDv2_elvslope.csv')
net20 <- read.csv('MODIS_LST_download/LST_NHD/Network_20_attribs.csv')
CumArea<- read.csv('MODIS_LST_download/LST_NHD/NHDv2_CumulativeArea.csv')
CumArea$COMID <- as.character(CumArea$ComID)
net <- merge(net20[,c("rid", "COMID", "RCAID")], elvslope[,c("COMID", "MAXELEVSMO")], by = "COMID")
net <- dplyr::select(net, -RCAID)
net <- merge(net, CumArea[,c("COMID", "TotDASqKM")], by = "COMID")

Airtemp_NHD <- expand.grid(net20$COMID, stationdat_cast_df$date[stationdat_cast_df$date < "2000-03-31"]) 
colnames(Airtemp_NHD) <- c("COMID", "date")
net <- merge(net, Airtemp_NHD, by = "COMID")
net <- merge(net, stationdat_cast_df[,c("date", "Blue_Mountain_Spring_OR_US", "Julday", "date_YM")], by = "date")

#Calculate daily water temp for every stream reach based on equations
#Apply linear regression model for Spring
net$Watertemp <- NULL
net[net$Julday < 210,"Watertemp"] <- with(net[net$Julday < 210,], 4.864410 + 0.2619132*Blue_Mountain_Spring_OR_US + 0.05555827*Julday + 0.4810596*log10(TotDASqKM) -0.00005258521*MAXELEVSMO)

#Apply linear regression model for Fall
net[net$Julday >= 210,]$Watertemp <- with(net[net$Julday >= 210,], 33.5358 + 0.2431526*Blue_Mountain_Spring_OR_US - 0.08495391*Julday + 1.144273*log10(TotDASqKM)- 0.00004201176*MAXELEVSMO)

#Take out negative values
net[net$Watertemp < 0 ,"Watertemp"] <- 0

#Compute monthly means and number of days under 6C
net_dt <- as.data.table(net)
net_monthly <- net_dt[, .(month_mean = mean(Watertemp, na.rm = T), 
                                month_u6 = length(Julday[Watertemp < 6])), 
                            by = .(rid, date_YM)]

#############################################################################################################################
#JOIN LST AND WEATHER STATIONS ESTIMATES
#############################################################################################################################

MODLST_monthly <- read.csv("MODIS_LST_download/LST_NHD/MODLST_monthly.csv", colClasses = c("numeric", "character", "numeric", "integer"))
MODLST_monthly$date_YM <- as.Date(paste(MODLST_monthly$date_YM, "-01", sep=""))
net_monthly$date_YM <- as.Date(paste(net_monthly$date_YM, "-01", sep=""))
tempdat_9916 <- rbind(MODLST_monthly, net_monthly)

#Check differences for overlapping month of march
cols <- rainbow(1+nrow(tempdat_9916[tempdat_9916$date_YM == "2000-03-01",])/2, s=.6, v=.9)[sample(1:1+nrow(tempdat_9916[tempdat_9916$date_YM == "2000-03-01",])/2,nrow(tempdat_9916[tempdat_9916$date_YM == "2000-03-01",])/2)]
ggplot(tempdat_9916[tempdat_9916$date_YM == "2000-03-01",], aes(x = rid, y = month_mean, group = rid, color = factor(rid))) + 
  geom_line()+ 
  scale_colour_manual(values = cols) +
  theme(legend.position = "none")

tempdat_9916 <- rbind(MODLST_monthly, net_monthly[net_monthly$date_YM < "2000-03-01",])
write.csv(tempdat_9916, "MODIS_LST_download/LST_NHD/tempdat_9916.csv")
tempdat_9916<- read.csv("MODIS_LST_download/LST_NHD/tempdat_9916.csv", row.names = 1, colClasses = c("character","character", "Date", "numeric", "numeric"))

#############################################################################################################################
#GENERATE DATA FOR 2016-2020
#############################################################################################################################

tempdat_9916_dt <- data.table(tempdat_9916)
tempdat_9916_dt$Year <- format(tempdat_9916_dt$date_YM, "%Y")
tempdat_9916_dt$Month <- format(tempdat_9916_dt$date_YM, "%m")

#Compute watershed-wide yearly mean
tempdat_9916_dt_yearly <- ddply(tempdat_9916_dt, .(Year), summarize, year_mean = mean(month_mean))

#Compute monthly mean over 2012-2016 for each site
tempdat_1216_stat <- tempdat_9916_dt[date_YM > "2012-01-01", .(month_mean = mean(month_mean), month_u6 = as.integer(mean(month_u6)+0.5)), .(rid, Month)]

#Compare it to that for 1999-2012
tempdat_9912_stat <- tempdat_9916_dt[date_YM < "2012-01-01", .(month_mean9912 =mean(month_mean)), .(rid, Month)]
tempdat_stat <- cbind(tempdat_9912_stat[order(Month)],tempdat_1216_stat[order(Month), "month_mean"])
ggplot(tempdat_stat, aes(x = Month, y = (month_mean- month_mean9912), group = rid)) + geom_line(aes(color = rid), alpha = 1/3) + theme(legend.position = 'none')

#Append four years of data using the average monthly means from 2012-2016
tempdat_1620 <- tempdat_1216_stat[rep(1:nrow(tempdat_1216_stat), times = 5),]
tempdat_1620$Year <- c(rep('2016', times = nrow(tempdat_1216_stat)), 
  rep('2017', times = nrow(tempdat_1216_stat)),
  rep('2018', times = nrow(tempdat_1216_stat)),
  rep('2019', times = nrow(tempdat_1216_stat)),
  rep('2020', times = nrow(tempdat_1216_stat)))
tempdat_1620$date_YM <- as.Date(with(tempdat_1620, paste(Year, Month, "01", sep="-")))
tempdat_9920 <- rbind(tempdat_9916, dplyr::select(tempdat_1620,rid, date_YM, month_mean, month_u6)[tempdat_1620$date_YM > "2016-10-01",])

tempdat_9920_mean <- cast(tempdat_9920[tempdat_9920$date_YM>"1999-05-01"], rid~date_YM, value = "month_mean")
write.csv(tempdat_9920_mean, "ISEMP/HexSim_ready/tempdat_9920_mean.csv", row.names =F)
tempdat_9920_u6<- cast(tempdat_9920[tempdat_9920$date_YM>"1999-05-01"], rid~date_YM, value = "month_u6")
write.csv(tempdat_9920_u6, "ISEMP/HexSim_ready/tempdat_9920_u6.csv", row.names =F)

#Divide all values by the number of days in that month to get a percentage and reverse from days under 6 to days over 6
colnames(tempdat_9920_u6[,2:ncol(tempdat_9920_u6)])
dayinmon<- c(rep(c(30,31,31,30,31,30,31,31,29,31,30,31), 21), 30,31,31,30,31,30,31)
dayinmon
tempdat_9920_u6[2:ncol(tempdat_9920_u6)] <- sweep(tempdat_9920_u6[2:ncol(tempdat_9920_u6)], 2, dayinmon, `/`)
tempdat_9920_u6[2:ncol(tempdat_9920_u6)] <- 1-tempdat_9920_u6[2:ncol(tempdat_9920_u6)]


#Format column names so that they correspond to HexSim monthly steps
colnames(tempdat_9920_mean)[2:ncol(tempdat_9920_mean)] <- paste(rep("Temp_mean", times = ncol(tempdat_9920_mean)-1), seq(1:(ncol(tempdat_9920_mean)-1)), sep="_")
colnames(tempdat_9920_u6)[2:ncol(tempdat_9920_u6)] <- paste(rep("Tempcold", times = ncol(tempdat_9920_u6)-1), seq(1:(ncol(tempdat_9920_u6)-1)), sep="_")

write.csv(tempdat_9920_mean, file.path(rootdir, "src/Crayfish_model/Hexsim_ready_data/Network_module/Network_20/tempdat_9920_mean_format.csv"), row.names =F)
write.csv(tempdat_9920_u6, file.path(rootdir, "src/Crayfish_model/Hexsim_ready_data/Network_module/Network_20/tempdat_9920_u6_format.csv"), row.names =F)
