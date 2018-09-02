#Author: Mathis L. Messager
#Related publication: Messager ML, Olden JD. Individual-based models forecast the spread and inform the management 
#                                             of an emerging riverine invader. Divers Distrib.2018;00:1-14. https://doi.org/10.1111/ddi.12829
#Purpose: Post-process HexSim model population size and effort results  
#Produce: Figure S6.3

library(ggplot2)
library(plyr)
library(grid)
library(gridExtra)
library(stringr)
library(data.table)
library(scales)
library(gstat) 
library(sp)
library(maptools)
library(gtable)

rootdir <- 'F:/Chapter2_HexSim_Crayfish' #TO UPDATE
setwd(file.path(rootdir, 'src/Crayfish_model'))
outdir <- file.path(rootdir,'/doc/Manuscript/Figures/Kernels')
hexsimres <- 'Parameters_and_outputs/Network_23'

######
#Look at change in super-individual YOY number equivalent
for (i in 0:13) {
  print(i)
  x = 4*(0.25^(1/12))^(i)
  print(x)
}

#Define network and scenario
network = 23
test = "16_2025_control2009_50perc_2"
effortyr = 2009
network_csv = read.csv("Hexsim_ready_data/Network_module/Network_20/edges.csv")


#HexSim output directory
root_dir <- paste("Network_", network, "/Results/Network", network, "_test", test, sep = "")
#Wanted directory to write results
root_dir_out <- paste("Parameters_and_outputs/Network_", network, "/Network", 
                      network, "_test", test, sep = "")


#List all directories in HexSim output directory, each corresponding to 
scenario_reps_list <- list.files(root_dir)
scenario_reps_list
#Check population adjusting for super-individual YOY 
popnames_files <- lapply(scenario_reps_list, function(x) {
  scenario_rep <- paste(root_dir, x, sep = "/")
  scenario_reps_res_list <- list.files(scenario_rep)
  scenario_reps_res<-paste(scenario_rep, scenario_reps_res_list[2], sep = "/")
  temp_outpop <- read.csv(scenario_reps_res)

  data.frame(temp_outpop[,1:2],temp_outpop[,7]*4+rowSums(temp_outpop[,7:10]))
})
#temp_outpop[,7]*0.25^(temp_outpop[,2]-2)%%12+1 coverts the number of juvenile super-individuals to the actual number of juveniles in 
#the population given that the super-individual to juvenile rate decreases over the year due to differential mortality of juveniles
4*(0.25^(1/12))^((temp_outpop[,2]-2)%%12)

#ts <- 1:nrow(popnames_files[[1]])

#Convert the list of dataframes into a single dataframe
popnames_df <- ldply(popnames_files, data.frame)
popnames_df$Population.Size <- popnames_df[,3]
#Need to double the population size as model is female-only
popnames_df$Population.Size <- popnames_df$Population.Size*2

#Convert time steps to years and months starting at month #6
popnames_df$year <- 1999+as.integer((popnames_df$Time.Step + 4)/12)
popnames_df$month <- ((popnames_df$Time.Step + 4) %% 12) +1
popnames_df$date <- as.Date(paste(popnames_df$year,popnames_df$month,"01", sep ="-"), format = "%Y-%m-%d")

#
popnames_df$Run <- factor(popnames_df$Run)

#Calculate for each time step, the mean population
popnames_stat <- ddply(popnames_df, .(date), summarise, meanpop = mean(Population.Size, na.rm = T))
popnames_cv <- ddply(popnames_df, .(date), summarise, cvpop = sd(Population.Size, na.rm = T)/mean(Population.Size, na.rm = T), sdpop = sd(Population.Size, na.rm = T))
popnames_stat <-cbind(popnames_stat,popnames_cv)
popnames_stat$month <- as.numeric(substr(popnames_stat$date,6,7))

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#df_melt <- melt(df, id.vars = 'ts', variable.name = 'replicate')
ggplot(popnames_df, aes(date,Population.Size)) + geom_line(aes(colour = Run, group = Run),size = 1.2, alpha = 0.5) + 
  #scale_y_log10(breaks = c(10, 100, 1000, 10000, 100000, 500000), labels = c(10, 100, 1000, 10000,100000, 500000), limits = c(1, 25000000)) +
  #scale_color_brewer(palette = "Spectral") +
  #scale_color_brewer(palette = "Accent") +
  geom_line(data = popnames_stat,aes(x = date, y= meanpop), color = "gray30", size = 1) + 
  theme_bw() + 
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        text = element_text(size = 18),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = c(0.15,0.60)) +
  labs(x = "Year", y = "Crayfish population size") + 
  scale_y_continuous(breaks = c(1000, 100000, 1000000,10000000, 25000000,100000000,500000000), 
                     labels = c("10,000", "100,000", "1,000,000","10,000,000","25,000,000","100,000,000", "500,000,000"), 
                     limits = c(0, 500000000)) +
  coord_trans(y = "sqrt") +
  scale_x_date(minor_breaks = NULL, limits = c(as.Date("1999-06-01"),as.Date("2025-06-01")))


#STATS
popnames_stat[popnames_stat$date == "2005-08-01","meanpop"]
popnames_cv[popnames_stat$date == "2005-08-01","cvpop"]
popnames_stat[popnames_stat$date == "2010-08-01","meanpop"]
popnames_cv[popnames_stat$date == "2010-08-01","cvpop"]
popnames_stat[popnames_stat$date == "2016-08-01","meanpop"]
popnames_cv[popnames_stat$date == "2016-08-01","cvpop"]
popnames_stat[popnames_stat$date == "2024-08-01","meanpop"]
popnames_cv[popnames_stat$date == "2024-08-01","cvpop"]

###########################################################################################################
#########
#Compute statistics on effort
effortnames_files <- lapply(scenario_reps_list, function(x) {
  scenario_rep <- paste(root_dir, x, "Generated Properties", sep = "/")
  scenario_reps_res_list <- as.vector(list.files(scenario_rep))
  scenario_reps_res<-paste(scenario_rep, scenario_reps_res_list[pmatch("Effort",scenario_reps_res_list)], sep = "/")
  temp_effort <- read.csv(scenario_reps_res)
  temp_effort[temp_effort == "-1.#IND"] <- NA
  temp_effort <- sapply(temp_effort, as.numeric)
  temp_effort <- merge(temp_effort, network_csv[,c("rid","reach_area", "Shape_Length")], by.x = "Reach", by.y = "rid")
  data.frame(Time.Step = as.numeric(str_sub(colnames(temp_effort[,2:(ncol(temp_effort)-2)]),8,-1)), 
             Total_Effort = colSums(temp_effort[,2:(ncol(temp_effort)-2)], na.rm = T), 
             Mean_Effort = colMeans(temp_effort[,2:(ncol(temp_effort)-2)], na.rm = T),
             Max_Effort = apply(temp_effort[,2:(ncol(temp_effort)-2)], 2, function(x) max(x, na.rm = T)),
             Mean_TrapInterval = 1/colMeans(temp_effort[,2:(ncol(temp_effort)-2)]/temp_effort[,"Shape_Length"], na.rm = T),
             Max_TrapInterval = 1/apply(temp_effort[,2:(ncol(temp_effort)-2)]/temp_effort[,"Shape_Length"], 2, function(x) max(x, na.rm = T)),
             Total_length = apply(temp_effort[,2:(ncol(temp_effort)-2)], 2, function(x) sum(temp_effort[!is.na(x), "Shape_Length"])))
})

#Convert the list of dataframes into a single dataframe
effort_df <- ldply(effortnames_files, data.frame)

#Convert time steps to years and months starting at month #6
effort_df$year <- 1999 + as.integer(as.numeric(as.character(effort_df$Time.Step))/12)

#Calculate for each time step the mean of the statistics over the 10 replicates
effort_stat <- data.table(effort_df)[,lapply(.SD, mean), by=year]
effort_sd <- data.table(effort_df)[,lapply(.SD, sd), by=year]
colnames(effort_sd) <- paste(colnames(effort_sd), "sd", sep="_")
effort_stat <- cbind(effort_stat, effort_sd[,3:ncol(effort_sd)])

write.csv(effort_stat, paste(root_dir_out,"effort_stat.csv", sep="/"), row.names = F)

############## DEFINE KERNELS FOR DIFFERENT HEXSIM SCENARIOS #######################################
#Network 18_4 to 20_7
try20_1 <- data.frame(prob = c(0.20,0.14,0.08,0.06,0.06,0.06, 0.06,0.05,0.05,0.05, 0.05, 0.04, 0.04, 0.03, 0.02, 0.01), 
                  dist = c(500,1000,1500,2000,2500,3000,3500, 4000, 4500, 5000, 5500, 6000, 6500, 7000, 7500, 8000))

ggplot(try20_1, aes(x= dist)) + 
  geom_line(aes(y = prob), size = 0.75, color = "#a8ddb5") +
  geom_point(aes(y = prob), size = 2, color = "#a8ddb5") +
  theme_bw() +
  labs(x = "Distance (m)", y = "Probability") + 
  theme(legend.position='none',
        text = element_text(size=20),
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(0.25,0.25,0,0), "cm"),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank())
sum(try20_1$prob)
sum(try20_1$prob*try20_1$dist)

#Network 20_8 to 20_9
try20_8 <- data.frame(prob = c(0.25,0.16,0.09,0.07,0.06,0.06, 0.05,0.05,0.04,0.04, 0.04, 0.03, 0.02, 0.02, 0.01, 0.01), 
                  dist = c(500,1000,1500,2000,2500,3000,3500, 4000, 4500, 5000, 5500, 6000, 6500, 7000, 7500, 8000))

ggplot(try20_8, aes(x= dist)) + 
  geom_line(aes(y = prob), size = 0.75, color = "#a8ddb5") +
  geom_point(aes(y = prob), size = 2, color = "#a8ddb5") +
  theme_bw() +
  labs(x = "Distance (m)", y = "Probability") + 
  theme(legend.position='none',
        text = element_text(size=20),
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(0.25,0.25,0,0), "cm"),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank())
sum(try20_8$prob)
sum(try20_8$prob*try$dist)


#Network 20_10 
try <- data.frame(prob_2010 = c(0.30,0.20,0.09,0.07,0.05,0.04, 0.04,0.04,0.03,0.03, 0.03, 0.02, 0.02, 0.01, 0.01, 0.01,0.01), 
                  prob_2011 = c(0.20,0.16,0.12,0.08,0.07,0.06, 0.05,0.05,0.04,0.04, 0.03, 0.03, 0.02, 0.02, 0.01, 0.01,0.01),
                  prob_2012 = c(0.18,0.12,0.09,0.08,0.07,0.07,0.06,0.05,0.05,0.04,0.04,0.04,0.03,0.03,0.02,0.02,0.01),
                  dist = c(0,500,1000,1500,2000,2500,3000,3500, 4000, 4500, 5000, 5500, 6000, 6500, 7000, 7500,8000))
ggplot(try, aes(x= dist, group = 1)) + 
  geom_line(aes(y = prob_2010), size = 1.25, color = "#a8ddb5") +
  geom_line(aes(y = prob_2011), size = 1.25, color = "#43a2ca") +
  geom_line(aes(y = prob_2012), size = 1.25, color = "#023858") +
  theme_bw() +
  labs(x = "Distance (m)", y = "Probability") 

sum(try$prob_2010)
sum(try$prob_2010*try$dist)
sum(try$prob_2011*try$dist)
sum(try$prob_2012*try$dist)
sum(try$prob_2012)

sum(c(0.18,0.12,0.09,0.08,0.08,0.07,0.06,0.05,0.05,0.04,0.04,0.04,0.03,0.03,0.02,0.02,0.01)*try$dist)

#Network 20_14, 20_16 and so on FINAL MODEL
try <- data.frame(prob_2010 = c(0.30,0.20,0.09,0.07,0.05,0.04, 0.04,0.04,0.03,0.03, 0.03, 0.02, 0.02, 0.01, 0.01, 0.01,0.01),
                  prob_2014 = c(0.30,0.09,0.08,0.06,0.06,0.06, 0.05,0.05,0.05,0.04, 0.04, 0.03, 0.03, 0.02, 0.02, 0.01,0.01), 
                  prob_2016_fast = c(0.19,0.09,0.08,0.08,0.07,0.07,0.06,0.06,0.05,0.05,0.04,0.04,0.03,0.03,0.03,0.02,0.01),
                  dist = c(0,500,1000,1500,2000,2500,3000,3500, 4000, 4500, 5000, 5500, 6000, 6500, 7000, 7500,8000))

#Figure S6.3b: Network 20_14, 20_16 and so on FINAL MODEL
kernelini <- ggplot(try, aes(x= dist, group = 1)) + 
  geom_bar(aes(y = prob_2010, fill = "black"),stat="identity", size = 0.5, width = 490, color = "black", alpha = 1) +
  geom_bar(aes(y = prob_2014, fill = "grey"),stat="identity", size = 0.5, width = 490, color = "black", alpha = 1/2) +
  theme_classic() +
  annotate(geom="text", x=8000, y=0.30, label="A", size=3) +
  scale_fill_identity(guide = "legend",name = "",labels=c('Initial model', 'Final model')) +
  theme(legend.position = c(0.75,0.5)) +
  labs(x = "Distance (m)", y = "Probability") 

kernelfast <- ggplot(try, aes(x= dist, group = 1)) + 
  geom_bar(aes(y = prob_2016_fast, fill = "black"),stat="identity", size = 0.5, width = 490, color = "black", alpha = 1) +
  geom_bar(aes(y = prob_2014, fill = "grey"),stat="identity", size = 0.5, width = 490, color = "black", alpha = 1/2) +
  theme_classic() +
  annotate(geom="text", x=8000, y=0.30, label="B", size=3) +
  scale_fill_identity(guide = "legend",name = "",labels=c('Fast model', 'Final model')) +
  theme(legend.position = c(0.75,0.5)) +
  labs(x = "Distance (m)", y = "Probability") 

png(file.path(outdir,"Kernels_3.png"),width=7,height=2.5,units='in', res=300)
grid.arrange(kernelini, kernelfast, ncol=2)
dev.off()

#Figure S6.3a: Compared scenario network 20_16 to Kernel Fast in sensitivity analysis
kernel <- ggplot(try, aes(x= dist, group = 1)) + 
  geom_bar(aes(y = prob_2010, fill = "black"),stat="identity", size = 0.5, width = 490, color = "black", alpha = 1) +
  geom_bar(aes(y = prob_2014, fill = "grey"),stat="identity", size = 0.5, width = 490, color = "black", alpha = 1/2) +
  theme_classic() +
  scale_fill_identity(guide = "legend",name = "",labels=c('Initial model', 'Final model')) +
  theme(legend.position = c(0.75,0.5)) +
  labs(x = "Distance (m)", y = "Probability") 

png(file.path(outdir,"Kernels_2.png"),width=3,height=2.5,units='in', res=300) 
kernel
dev.off()

sum(try$prob_2010)
sum(try$prob_2010*try$dist)
sum(try$prob_2014)
sum(try$prob_2014*try$dist)
sum(try$prob_2016_fast)
sum(try$prob_2016_fast*try$dist)

dens <- c(0,0.1,0.25,0.5,seq(1,50))
qplot(dens,dens^(1/1.5), geom = "line") + 
  geom_line(y = dens^(1/2)) + 
  geom_line(y = 2*dens^(1/2)) + 
  geom_line(y = 3*dens^(1/2)) + 
  theme_bw()

