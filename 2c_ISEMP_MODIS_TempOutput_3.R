#Author: Mathis L. Messager
#Related publication: Messager ML, Olden JD. Individual-based models forecast the spread and inform the management 
#                                             of an emerging riverine invader. Divers Distrib.2018;00:1-14. https://doi.org/10.1111/ddi.12829
#Purpose: Format land surface temperature data and predict water temperature in each reach of the John Day River based on models developed in ISEMP_format_9.R
#         Develop model of water temperature data.

library(ggplot2)
library(reshape)
library(plyr)
library(dplyr)
library(data.table)

rootdir <- 'F:/Chapter2_HexSim_Crayfish/Data' #UPDATE
setwd(file.path(rootdir,'MODIS_LST_download'))

#####################################
#Import LST data for all river reaches
MODLST_all1 <- read.csv('LST_NHD/LSTrecords_allJDR_MOD2000065_MOD2008133.csv')
MODLST_all2 <- read.csv('LST_NHD/LSTrecords_allJDR_MOD2008134_MOD2011109.csv')
MODLST_all3 <- read.csv('LST_NHD/LSTrecords_allJDR_MOD2011110_MOD2016290.csv')

MODLST_all <- merge(MODLST_all1, MODLST_all2, by = "COMID")
MODLST_all <- merge(MODLST_all, MODLST_all3, by = "COMID")

remove(MODLST_all1)
remove(MODLST_all2)
remove(MODLST_all3)
gc()

########################################################################
#Restrict the number of reaches analyzed to those in the HexSim river network
#In Arcmap, export "F:\Hexsim\Crayfish_model\Hexsim_ready_data\Network_module\Network_20\edges_20.shp" attribute table to F:\Hexsim\Data\MODIS_LST_download\LST_NHD\Network_20_attribs.csv
net20 <- read.csv('LST_NHD/Network_20_attribs.csv')
MODLST_all <- merge(net20[,c("COMID", "RCAID", "rid")], MODLST_all, by = "COMID")
MODLST_all <- dplyr::select(MODLST_all, -RCAID, -COMID)

########################################################################
#Perform temporal interpolation for all river reachesinterpNA
#Convert to matrix and rotate it to have dates as rows and sites as columns
MODLST_all_t <- as.matrix(t(MODLST_all[,-1]))
colnames(MODLST_all_t) <- MODLST_all$rid
#Convert 0 to NA
MODLST_all_t[which(MODLST_all_t == 0, arr.ind = T)] <- NA
nrow(which(is.na(MODLST_all_t), arr.ind = T))/length(MODLST_all_t) 

#Interpolation linear when bound by two values and just grabbing the next value for the first few days without data and the last value for the last few fays without data
MODLST_all_interp <- timeSeries::interpNA(MODLST_all_t, method = "linear")
#MODLST_all_interp[(nrow(MODLST_all_interp)/2):nrow(MODLST_all_interp),] <- timeSeries::interpNA(MODLST_all_interp[(nrow(MODLST_all_interp)/2):nrow(MODLST_all_interp),], method = "before")
#MODLST_all_interp[1:(nrow(MODLST_all_interp)/2),] <- timeSeries::interpNA(MODLST_all_interp[1:(nrow(MODLST_all_interp)/2),], method = "after")
#interpNA using before and after doesn't work so make custom function
#Convert to degC
MODLST_all_interp <- MODLST_all_interp*0.02 - 273.15

fillinterp <- function(x) {
  begin <- x[1:100]
  begin[is.na(begin)] <- begin[min(which(!is.na(begin)))]
  x[1:100] <- begin
  
  end <- x[(length(x)-100):length(x)]
  end[is.na(end)] <- end[max(which(!is.na(end)))]
  x[(length(x)-100):length(x)] <- end
  
  return(x)
}

MODLST_all_interp <- adply(MODLST_all_interp, 2, fillinterp)
colnames(MODLST_all_interp)[1] <- "rid"

#Melt data
MODLST_melt <- melt(MODLST_all_interp, id.vars = "rid")
remove(MODLST_all)
remove(MODLST_all_interp)
MODLST_melt$rid <- as.numeric(as.character(MODLST_melt$rid))
MODLST_melt$rid[which(!(unique(MODLST_melt$rid) %in% unique(net20$rid)))]
MODLST_melt_2 <- merge(MODLST_melt, net20[,c("rid", "COMID")], by = "rid")
MODLST_melt <- MODLST_melt_2

########################################################################
#Import covariate data for all river reaches
#Export "F:\Hexsim\Data\River_network\NHDplusV2\NHDPlusPN\NHDPlus17\NHDPlusAttributes\elevslope.dbf" to 'F:/Hexsim/Data/MODIS_LST_download/LST_NHD/NHDv2_elvslope.csv' in Arcmap
elvslope <- read.csv('LST_NHD/NHDv2_elvslope.csv')

#elvslope$COMID <- as.character(elvslope$COMID)
dt1 <- data.table(MODLST_melt, key = "COMID")
dt2 <- data.table(elvslope[,c("COMID", "MAXELEVSMO")], key = "COMID")
MODLST_melt_elv <- as.data.frame(dt1[dt2, nomatch=0])
remove(dt1)
remove(dt2)

#MODLST_melt_elv_2<- merge(MODLST_melt_elv_2, elvslope[,c("COMID", "MAXELEVSMO")])
#MODLST_melt_elv[which(is.na(MODLST_melt_elv$MAXELEVSMO)),c(1, ncol(MODLST_melt_elv))]
#None of these missing values actually intersect with the network

#Add watershed area, using datatable is much more memory efficient
CumArea<- read.csv('LST_NHD/NHDv2_CumulativeArea.csv')
CumArea$COMID <- CumArea$ComID
dt1 <- data.table(MODLST_melt_elv, key = "COMID")
dt2 <- data.table(CumArea[,c("COMID", "TotDASqKM")], key = "COMID")
MODLST_melt_elv_2 <- as.data.frame(dt1[dt2, nomatch=0])
remove(dt1)
remove(dt2)

#Add date
MODLST_melt_elv_2$date <- paste(substr(MODLST_melt_elv_2$variable , 4,7), as.character(as.numeric(substr(MODLST_melt_elv_2$variable, 8,10))), sep = '/')
MODLST_melt_elv_2$date <- as.POSIXlt(MODLST_melt_elv_2$date, format = "%Y/%j")
#Add calendar day
MODLST_melt_elv_2$Julday <- as.POSIXlt(MODLST_melt_elv_2$date, "%Y-%m-%d")$yday
#Add year/month
MODLST_melt_elv_2$date_YM <- format(MODLST_melt_elv_2$date, "%Y-%m")
#Add square of LST2
MODLST_melt_elv_2$LST2 <- MODLST_melt_elv_2$value^2


write.csv(MODLST_melt_elv_2, "LST_NHD/MODLST_melt_elv_2.csv", row.names = F)
MODLST_melt_elv_2 <- read.csv("LST_NHD/MODLST_melt_elv_2.csv")

########################################################################
#Apply linear regression model for Spring
MODLST_melt_elv_2$Watertemp <- NULL
MODLST_melt_elv_2[MODLST_melt_elv_2$Julday < 210,"Watertemp"] <- with(MODLST_melt_elv_2[MODLST_melt_elv_2$Julday < 210,], 0.002197*LST2 + 0.0774*Julday + 1.4374*log10(TotDASqKM) -0.00001915*MAXELEVSMO - 2.2751)

#Apply linear regression model for Fall
MODLST_melt_elv_2[MODLST_melt_elv_2$Julday >= 210,]$Watertemp <- with(MODLST_melt_elv_2[MODLST_melt_elv_2$Julday >= 210,], 29.2396 + 0.1437*value - 0.08421*Julday + 1.534*log10(TotDASqKM)- 0.00002345*MAXELEVSMO)

#Take out negative values
MODLST_melt_elv_2[MODLST_melt_elv_2$Watertemp < 0 ,"Watertemp"] <- 0

#Compute monthly means and number of days under 6C
MODLST_dt <- as.data.table(MODLST_melt_elv_2)
MODLST_monthly <- MODLST_dt[, .(month_mean = mean(Watertemp, na.rm = T), 
                                month_u6 = length(Julday[Watertemp < 6])), 
                            by = .(rid, date_YM)]

write.csv(MODLST_monthly , "LST_NHD/MODLST_monthly.csv", row.names = F)