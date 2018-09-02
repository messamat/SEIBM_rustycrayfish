#Author: Mathis L. Messager
#Related publication: Messager ML, Olden JD. Individual-based models forecast the spread and inform the management 
#                                             of an emerging riverine invader. Divers Distrib.2018;00:1-14. https://doi.org/10.1111/ddi.12829
#Purpose: Develop model of river width based on wetted width measured on satellite-imagery, estimated mean annual discharge, and elevation

library(boot)
library(caret)
library(ggplot2)

#Set-up directory structure
rootdir <- "F:/Chapter2_HexSim_Crayfish/"
resdir <- file.path(rootdir, 'results/width_model')
datadir <- file.path(rootdir, 'data/width_model')

#Import and format width data (taken at two different dates)
width_dat_1 <- read.csv(file.path(datadir,"Network_points_select_width.csv"))
width_dat_2 <- read.csv(file.path(datadir, "Network_points_select_width_Jan21st2017.csv"))
width_dat <- rbind(width_dat_1[,colnames(width_dat_1) %in% colnames(width_dat_2)], width_dat_2[,colnames(width_dat_2) %in% colnames(width_dat_1)])


width_dat_sel <- width_dat[width_dat$Width_1 >= 1 & width_dat$Width_2 > 0,] #Only keep records with positive width estimates (0 means that width wasn't measured, -1 means that forest cover impeded measurement)
width_dat_sel$width_mea <- rowMeans(width_dat_sel[,c('Width_1','Width_2','Width_3')])
width_dat_sel$width_sd <- apply(width_dat_sel[,c('Width_1','Width_2','Width_3')], 1, sd)

#Visualize data Q0001E_MA fromt NHDv2 plus 
ggplot(width_dat_sel, aes(x= Q0001E_MA, y = Width_1, color = factor(HUC8))) + geom_point() +
  geom_point(aes(y = Width_2)) + geom_point(aes(y = Width_3)) + theme_bw() + 
  labs(x = "Mean annual discharge, cfs", y = "width, m")
#Plot relationship with other predictors
ggplot(width_dat_sel, aes(x= SLOPE, y = width_mea)) + geom_point() + scale_y_sqrt() + scale_x_sqrt()
ggplot(width_dat_sel, aes(x= MAXELEVSMO, y = width_mea)) + geom_point()+ scale_y_sqrt() + scale_x_sqrt()
#Plot autocorrelation 
ggplot(width_dat_sel, aes(x= MAXELEVSMO, y = Q0001E_MA)) + geom_point()+ scale_y_sqrt() + scale_x_sqrt()

#Plot variable distribution for dependent and independent variables
qplot(width_dat_sel$width_mea)
qplot(log10(width_dat_sel$width_mea))
qplot(width_dat_sel$Q0001E_MA)
qplot(log10(width_dat_sel$Q0001E_MA))
qplot(sqrt(width_dat_sel$Q0001E_MA))
qplot(width_dat_sel$MAXELEVSMO)

wid_mod_QMA <- lm(log10(width_mea)~log10(Q0001E_MA), data = width_dat_sel)
summary(wid_mod_QMA)
ggplot(width_dat_sel, aes(x= exp(fitted(wid_mod_QMA)), y = width_mea)) + geom_point()

wid_mod_elv <- lm(width_mea~MAXELEVSMO, data = width_dat_sel)
summary(wid_mod_elv)

######################### CHOSEN MODEL #############################################
wid_mod_tot <- lm(log10(width_mea)~log10(Q0001E_MA) + MAXELEVSMO, data = width_dat_sel)
summary(wid_mod_tot)
ggplot(width_dat_sel, aes(x= 10^fitted(wid_mod_tot)*exp(2.65*summary(wid_mod_tot)$sigma**2), y = width_mea)) + geom_point() + geom_abline(intercept = 0, slope = 1)
ggplot(width_dat_sel, aes(x= 10^fitted(wid_mod_tot)*exp(2.65*summary(wid_mod_tot)$sigma**2), y = width_mea)) + geom_point() + geom_abline(intercept = 0, slope = 1) + 
  scale_y_log10() + scale_x_log10() + labs(x = "Estimated width,m ", y = "Empirical width, m")
plot(wid_mod_tot)


train_control <- trainControl(method="repeatedcv", number=2, repeats=100)
wid_mod_tot_train<- train(log10(width_mea)~log10(Q0001E_MA) + MAXELEVSMO, data = width_dat_sel, trControl = train_control, method = "lm")
print(wid_mod_tot_train)
wid_mod_tot_train$finalModel

train_control <- trainControl(method="boot", number=1000)
wid_mod_tot_train<- train(log10(width_mea)~log10(Q0001E_MA) + MAXELEVSMO, data = width_dat_sel, trControl = train_control, method = "lm")
print(wid_mod_tot_train)
wid_mod_tot_train$finalModel

#######################################################################################################
# NON-LINEAR LEAST SQUARE REGRESSION MODELS

wid_nls_mod <- nls(width_mea~a*(Q0001E_MA^b), data = width_dat_sel, start = list(a = 0.01, b = 0.4, c = -0.0004405))
summary(wid_nls_mod)
ggplot(width_dat_sel, aes(x= fitted(wid_nls_mod), y = width_mea)) + geom_point() + geom_abline(slope = 1, intercept = 0) +
  labs(x = "Estimated width,m ", y = "Empirical width, m") + theme_bw()

wid_nls_mod <- nls(width_mea~a*(Q0001E_MA^b) + c*MAXELEVSMO, data = width_dat_sel, start = list(a = 0.01, b = 0.4, c = -0.0004405))
summary(wid_nls_mod)
ggplot(width_dat_sel, aes(x= fitted(wid_nls_mod), y = width_mea)) + geom_point() + geom_abline(slope = 1, intercept = 0) +
  labs(x = "Estimated width,m ", y = "Empirical width, m") + theme_bw()

#######################################################################################################
#Apply to all reaches
width_dat_pred <- read.csv(file.path(resdir, "edges_1_withfields.csv"))
width_dat_pred$width_pred  <- exp(2.65*(summary(wid_mod_tot)$sigma**2))*10^(predict(wid_mod_tot, newdata = width_dat_pred)) #Apply log-log correction from Ferguson, R. I. River loads underestimated by rating curves. Water Resour. Res. 22, 74-76 (1986).
#Plot results
ggplot(width_dat_pred, aes(x= Q0001E_MA, y = width_pred, color = MAXELEVSMO)) + geom_point() + 
  labs(x = "Mean annual discharge, cfs", y = "width, m") + theme_bw()
#Lower width bound of 0.5 meters
width_dat_pred[width_dat_pred$width_pred < 0.5, "width_pred"] <- 0.5
width_dat_pred$RCAIDCOMID <- with(width_dat_pred, paste(RCAID, COMID, sep="-"))
#Write out predictions
write.csv(width_dat_pred[,c("RCAIDCOMID", "width_pred")], file.path(resir,"width_pred2.csv"))
#######################################################################################################
#Compare estimated values to field-recorded values in August 2016 during drought period
network20_widthpred <- read.csv(file.path(rootdir, "data/Field_work_Data/Sampled_sites_notes_network20_join.csv"))
siteinfo <- read.csv(file.path(rootdir, "data/Field_work_Data/Site_info.csv"))
width_testing <- merge(network20_widthpred , siteinfo, by.x = "Site_ID", by.y = "Site.ID")

width_obsmod <- lm(width_pred~Width, data=width_testing)
summary(width_obsmod)

ggplot(width_testing, aes(x=Width, y=width_pred)) + 
  geom_point() + 
  geom_abline(slope = 1.34, intercept = 9.4) + 
  geom_smooth(method='lm') +
  scale_y_continuous(breaks = c(0,5,10,20,30,40,50,60,70), limits = c(0,45)) + 
  scale_x_continuous(breaks = c(0,5,10,20,30,40,50,60,70), limits = c(0,70)) + 
  labs(y = "Observed width August 2016", x = "Predicted width") + 
  coord_fixed()


