#Author: Mathis L. Messager
#Related publication: Messager ML, Olden JD. Individual-based models forecast the spread and inform the management 
#                                             of an emerging riverine invader. Divers Distrib.2018;00:1-14. https://doi.org/10.1111/ddi.12829
#Purpose: Analyzes spread and population size trends of rusty crayfish for a given scenario. 
#Produce: Figure 3


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
outdir <- file.path(rootdir,'/doc/Manuscript/Figures/Model_output_graph') 
hexsimres <- file.path(rootdir, 'src/Crayfish_model/Parameters_and_outputs/Network_23')

#######################################################################################################################################
################## Compute mean invaded length at every time step #####################################################################
#Define network and scenario
network = 23
test = "16_2025"
network_csv = read.csv("Hexsim_ready_data/Network_module/Network_20/edges.csv")

#HexSim output directory
root_dir <- paste("Network_", network, "/Results/Network", network, "_test", test, sep = "")
#Wanted directory to write results
root_dir_out <- paste("Parameters_and_outputs/Network_", network, "/Network", 
                      network, "_test", test, sep = "")
#List all directories in HexSim output directory, each corresponding to 
scenario_reps_list <- list.files(root_dir)


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

#For every step of every replicate, compute the total length of invaded streams
densnames_files <- lapply(scenario_reps_list, function(x) {
  scenario_rep <- paste(root_dir, x, "Generated Properties", sep = "/")
  scenario_reps_res_list <- as.vector(list.files(scenario_rep))
  scenario_reps_res<-paste(scenario_rep, scenario_reps_res_list[pmatch("Dens",scenario_reps_res_list)], sep = "/")
  temp_dens <- read.csv(scenario_reps_res)
  temp_densdat <- temp_dens[,2:ncol(temp_dens)]
  temp_densdat[temp_densdat > 0] <- 1
  temp_dens[,2:ncol(temp_dens)] <- temp_densdat
  temp_dens <- merge(temp_dens, network_csv[,c("rid","reach_area", "Shape_Length")], by.x = "Reach", by.y = "rid")
  invaded_length <- sapply(temp_dens[,2:(ncol(temp_dens)-2)], function(x) sum(x*temp_dens$Shape_Length/1000))
  data.frame(Time.Step = as.numeric(str_sub(colnames(temp_dens[,2:(ncol(temp_dens)-2)]),6,-1)),
             Invaded_length = invaded_length)
})

#Convert the list of dataframes into a single dataframe
dens_df <- ldply(densnames_files, data.frame)

#Convert time steps to years and months starting at month #6
dens_df$year <- 1999+as.integer((dens_df$Time.Step + 4)/12)
dens_df$month <- ((dens_df$Time.Step + 4) %% 12) +1
dens_df$date <- as.Date(paste(dens_df$year,dens_df$month,"01", sep ="-"), format = "%Y-%m-%d")

#Calculate for each time step the mean of the statistics over the 10 replicates
dens_stat <- data.table(dens_df)[,lapply(.SD, mean), by=Time.Step]
dens_sd <- data.table(dens_df)[,lapply(.SD, sd), by=Time.Step]
colnames(dens_sd)[2] <- paste(colnames(dens_sd)[2], "sd", sep="_")
dens_stat <- cbind(dens_stat, dens_sd[,2])


dens_statjuly <- dens_stat[dens_stat$month == 8,]
spread_16 <- ggplot(dens_statjuly, aes(date,Invaded_length)) + 
  geom_ribbon(aes(ymin = Invaded_length-2*Invaded_length_sd, ymax= Invaded_length+2*Invaded_length_sd,fill = "gray"),size = 1, alpha = 0.8)+ 
  geom_line(aes(linetype="dotted"),size = 1.2) + 
  labs(x = "Year") + 
  scale_fill_identity(name='', guide = 'legend',labels ='95% prediction interval') +
  scale_linetype_identity(name='', guide = 'legend',labels ='Total invaded river length') +
  scale_y_continuous(breaks = c(0,250,500,750,1000,1250), name="Invaded river length (km)",limits=c(0,1260),expand=c(0,0))  +
  scale_x_date(limits = c(as.Date("1999-06-01"),as.Date("2025-06-01")), 
               breaks = as.Date(c("1999-06-01","2005-06-01","2010-06-01","2015-06-01","2020-06-01","2025-06-01")), 
               date_labels = "%Y",expand=c(0,0)) + 
  theme_bw() +  
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        text = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = c(0.04,0.95),
        legend.justification = c(0, 1), 
        legend.spacing.y = unit(-0.75, "cm"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.background = element_rect(fill = "transparent")) +
  #Doesn't work.. why?
  guides(fill = guide_legend(order = 0),linetype = guide_legend(order = 1))
#sec.axis = sec_axis(~., name=derive()),
spread_16


#####################################################################################################################################
popnames_statjuly <- popnames_stat[popnames_stat$month == 7,]
pop_16 <- ggplot(popnames_statjuly, aes(date,meanpop))  + 
  #scale_y_log10(breaks = c(10, 100, 1000, 10000, 100000, 500000), labels = c(10, 100, 1000, 10000,100000, 500000), limits = c(1, 25000000)) +
  #scale_color_brewer(palette = "Spectral") +
  #scale_color_brewer(palette = "Accent") +
  geom_ribbon(aes(ymin = meanpop-2*sdpop, ymax= meanpop+2*sdpop),size = 1, fill='gray',alpha = 0.8) + 
  geom_line(aes(color='black'),size = 1.2) +
  scale_color_identity(name='', guide = 'legend',labels ='Population size') +
  theme_classic() + 
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        text = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = c(0.04,0.78),
        legend.justification = c(0, 1), 
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y=element_text(hjust=0, vjust=0.2),
        legend.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent")) +
  labs(x = "Year", y = "Crayfish population size") + 
  scale_y_continuous(breaks = c(0,1000000,10000000, 100000000,500000000), 
                     labels= c(0,expression(10^6),expression(10^7),expression(10^8),expression(5.0%*%10^8)),
                     limits = c(0, 500000000),
                     expand=c(0,0),
                     trans = "sqrt") +
  scale_x_date(limits = c(as.Date("1999-06-01"),as.Date("2025-06-01")), breaks = as.Date(c("1999-06-01","2005-06-01","2010-06-01","2015-06-01","2020-06-01","2025-06-01")), date_labels = "%Y",expand=c(0,0)) 
pop_16

########## CODE FROM https://rpubs.com/kohske/dual_axis_in_ggplot2#######
# See https://cran.r-project.org/web/packages/gtable/gtable.pdf for gtable guide
#extract gtable
g1<-ggplot_gtable(ggplot_build(spread_16))
g2<-ggplot_gtable(ggplot_build(pop_16))

#overlap the panel of the 2nd plot on that of the 1st plot
g2$layout
pp<-c(subset(g2$layout, name=="panel", se=t:r))
g<-gtable_add_grob(g1,g2$grobs[[which(g2$layout$name =="panel")]], pp$t, pp$l, pp$b,pp$l)

#Add axis
ia <- which(g2$layout$name == "axis-l")
ga <- g2$grobs[[ia]]
ax <- ga$children[[2]]
ax$widths <- rev(ax$widths)
ax$grobs <- rev(ax$grobs)
ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.1, "cm")
g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths)-1)
g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)

#Add right-axis title
axt <- which(g2$layout$name == "ylab-l")
gaxt <- g2$grobs[[axt]]
gaxt$widths <- rev(gaxt$widths)
gaxt$grobs <- rev(gaxt$grobs)
g <- gtable_add_cols(g, g2$widths[g2$layout[axt, ]$l], length(g$widths) - 1)
g <- gtable_add_grob(g, gaxt, pp$t, length(g$widths) - 1, pp$b)

#Add legend
pp<-subset(g2$layout, name=="guide-box", se=t:r)
g<-gtable_add_grob(g,g2$grobs[[15]], 6,4, name="guide-box2")

#gtable_show_layout(g)
grid.draw(g)


######################## GRAPH OF RIVER SEGMENT SPECIFIC INVADED RIVER LENGTH ##############################################################
############################################# PANEL B ######################################################################################

################## Compute mean invaded length at every time step #####################################################################
#From python function pres_abs_match:
spread_tribs <- data.frame(year = c(1999, 2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023,2024,2025), 
                           up_main=c(381654,383030,383532,384364,385602,384331,384358,383037,386152,388003,389050,389177,389941,389575,389661,390138,390720,391153,390474,393821,393360,393692,395634,397005,398174,400740,401192),
                           down_main = c(377000,361139,347516,337240,323697,312353,296611,284177,269552,255534,236235,222950,209292,192853,177867,163901,149667,135292,122123,108032,92155,76854,62895,48765,32027,16045,1000),
                           up_south=c(0,0,0,148,1925,4073,9609,11442,14148,16165,18439,18804,19147,19350,20414,24312,23309,24880, 26387,27880,31370,29707,30974,30986,32251,32517,33757),
                           up_north=c(0,0,0,0,0,0,0,803,4912,10534,14976,18132,21784,23372,26893,30233,31427,35935,37728,40027,45299,46365,48454,50211,52021,52652,53570))

spread_tribs[,2:ncol(spread_tribs)] <- spread_tribs[,2:ncol(spread_tribs)]/1000

nrow(spread_tribs)
stg <- ggplot(spread_tribs, aes(x=year)) + 
  geom_ribbon(aes(ymin=down_main-379.327, ymax=up_main-379.327, fill='#ef3b2c'), alpha=1/1) + 
  geom_ribbon(aes(ymin=0, ymax=up_south, fill = '#377eb8'), alpha=1/2) + 
  geom_ribbon(aes(ymin=0, ymax=up_north, fill = '#dfc27d'), alpha=1/2) + 
  geom_hline(yintercept=0) +
  scale_fill_identity(name = '', guide = 'legend',breaks=c('#ef3b2c','#377eb8','#dfc27d'),labels = c('Mainstem JDR','South fork JDR','North fork JDR')) +
  scale_x_continuous(name='Year',limit=c(1999,2025), expand=c(0,0),
                     breaks=c(1999,2005,2010,2015,2020,2025))+
  scale_y_continuous(name='Invaded river length (km)', 
                     limit=c(-379.327, 60), expand=c(0,0),
                     breaks=c(-379.327, -300,-200,-100,0,25,50), 
                     labels=c('Mainstem \nColumbia River \n',300,200,100,'Initial \nintroduction',25,50 )) +
  geom_segment(aes(x = 1999, y = 0, xend = 1999, yend = -379.327), 
               arrow= arrow(length = unit(0.25,"cm"), type = "closed"), colour = "black",size=0.5) +
  geom_segment(aes(x = 1999, y = 0, xend = 1999, yend = 60), 
               arrow= arrow(length = unit(0.25,"cm"), type = "closed"), colour = "black",size=0.5) +
  annotate("text",label = 'Upstream', x = 1999.5, y = 25, hjust = 0,size=3) +
  annotate("text",label = 'Downstream', x = 1999.5, y = -110, hjust = 0,size=3) +
  theme_classic() +
  theme(panel.grid.minor = element_blank(),
        text = element_text(size=12),
        plot.margin = margin(unit='cm', c(0.1,0.5,0.1,0.1)),
        legend.position = c(0.25,0.25),
        axis.title.y = element_text(vjust=-20),
        legend.text = element_text(size=10))

gt <- ggplot_gtable(ggplot_build(stg))
gt$layout$clip[gt$layout$name=="panel"] <- "off"


########### PLOT THE TWO GRAPHS TOGETHER ###########
#Make sure thaT the top graph is aligned to the bottom graph's x-axis
#g$layout
#gtable_show_layout(g)
#g$widths
maxWidth = unit.pmax(g$widths, gt$widths)
g$widths <- maxWidth
gt$widths <- maxWidth

#Need to use rbind to be able to use the size="last" argument which forces the two plotting areas to be equal despite the fact that the bottom one has an x axis
grid.arrange(g,gt)

pdf(file.path(outdir,"Model_output_graph_3.pdf"),width=5,height=7)
grid.arrange(g,gt)
dev.off()
