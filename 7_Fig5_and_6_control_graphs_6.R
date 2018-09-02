#Author: Mathis L. Messager
#Related publication: Messager ML, Olden JD. Individual-based models forecast the spread and inform the management 
#                                             of an emerging riverine invader. Divers Distrib.2018;00:1-14. https://doi.org/10.1111/ddi.12829
#Purpose: Analyze and plot cost and effects of strategies to control the population and spread of rusty crayfish in the John Day River
#Produce: Figure 5 and 6

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

#Function to extract legend from graph as a grob to be re-inserted later
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

rootdir <- 'F:/Chapter2_HexSim_Crayfish'#TO UPDATE
setwd(file.path(rootdir, 'src/Crayfish_model/Parameters_and_outputs/Network_23')) 
outdir <- file.path(rootdir,'/doc/Manuscript/Figures/Control_graphs')

#######################################################################################################################################################################
########################################## FIGURE 5: SPREAD AND POPULATION EFFECTS FROM CONTROL STRATEGIES ############################################################
#Data output from HexSim models
management <- data.frame(start = c("no control",'2001','2001','2001','2005','2005','2009','2009','2009','2013','2013','2017','2017','2017'), 
                         removal = c("0%","10%", "25%", "50%","17.5%","37.5","10%", "25%", "50%","17.5%","37.5","10%", "25%", "50%"),
                         invlen = c(1111,954,624,106,817,507,1003,806,387,979,721,992,936, 697),
                         invpop = c(356378843,256840539,18417256,2106,151949960,1854351,289949897,108566330,276249,251603090,41776801,310266055,228353403,34893215),
                         sd_len = c(53,73,31,34,41,35,64,29,29,47,36,167,53,31),
                         sd_pop = c(21382731,20547243,7551075,1074,44065488,741740,28770461,17167936,85637,17612216,9608664,18615963,29685942,10816897),
                         shp = c(22, 21,21,21,24,24,23,23,23,26,26,25,25,25))
management$error_lenmin <- with(management,invlen - 2*sd_len)
management$error_lenmax <- with(management,invlen + 2*sd_len)
management$error_popmin <- with(management,invpop - 2*sd_pop)
management$error_popmax <- with(management,invpop + 2*sd_pop)
management[management$error_popmin < 0 & !is.na(management$error_popmin),"error_popmin"] <- 0.1

########################################################################################################
#HEATMAP OF DIFFERENCES IN SPREAD (from http://www.geo.ut.ee/aasa/LOOM02331/R_idw_interpolation.html)
management_format <- data.frame(startyr=c(2024, 2001,2001,2001,2005,2005,2009,2009,2009,2013,2013,2017,2017,2017),removal_numb=c(0,10,25,50,17.5,37.5,10,25,50,17.5,37.5,10,25,50))
management_format$invlen <- management$invlen 
management_format$invpop <- management$invpop 
management_format <- rbind(management_format, data.frame(startyr=c(2025,2025,2025,2025,2025,2025,2025,2025,2025,2025,2001,2005,2009,2013,2017,2023), removal_numb=c(5,10,14,17.5,21,25,31,37.5,43,50,rep(0,6)), invlen=rep(1111,16), invpop=rep(356378843,16)))

#Convert data to spatial points data frame
management_format <- management_format[!is.na(management_format$invlen),]
coordinates(management_format) <- ~startyr+removal_numb
#class(management_format)
#summary(management_format)

#Create interpolation surface grid
x.range <- as.numeric(c(1999, 2025))  # min/max x of the interpolation area
y.range <- as.numeric(c(0, 50))  # min/max y of the interpolation area
grd <- expand.grid(startyr = seq(from = x.range[1], to = x.range[2], by = 0.1), removal_numb = seq(from = y.range[1],to = y.range[2], by = 0.1))  # expand points to grid
coordinates(grd) <- ~startyr+removal_numb
gridded(grd) <- TRUE

#Compute empirical variogram
invlen_vgm <- variogram(invlen~startyr+removal_numb, management_format)
invlen_vgm
#Fit semivariogram with Gaussian function (other functions do not work)
invlen_fit = fit.variogram(invlen_vgm, model = vgm("Gau"))
plot(invlen_vgm, invlen_fit)

#Run universal kriging using fitted variogram
krige <- gstat::krige(invlen~startyr+removal_numb, management_format,grd,model=invlen_fit)  # apply kriging model for the data
krige.output = as.data.frame(krige)
names(krige.output)[1:3] <- c("startyr", "removal_numb", "invlen")

spread <- ggplot(krige.output, aes(x = startyr, y = removal_numb)) +
  geom_tile(aes(fill = invlen)) +
  #geom_point(data=as.data.frame(management_format), aes(x = startyr, y = removal_numb), alpha = 1/3) +
  scale_x_continuous(expand=c(0,0)) + 
  scale_y_continuous(expand=c(0,0)) +
  labs(x = 'Start of yearly removal (year)', y = 'Yearly mortality rate from trapping \n(% of population)') +
  scale_fill_distiller(type = "seq", palette = 'Spectral', direction = -1,
                       name = "2025 invaded \n river length (km)",
                       breaks = as.integer(c(min(krige.output$invlen)+1, 250,500,750,1111))) +
  theme_classic() +
  theme(legend.title.align=0.25,
        text = element_text(size = 12),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.background = element_rect(fill = "transparent")) 
spread
#Get legend as a grob
legend_1 <- get_legend(spread)
#Remove legend
spread_nolegend <- spread + theme(legend.position='none')

####################################################
# HEATMAP OF DIFFERENCES IN POPULATION SIZE
management_format <- data.frame(startyr=c(2024, 2001,2001,2001,2005,2005,2009,2009,2009,2013,2013,2017,2017,2017),removal_numb=c(0,10,25,50,17.5,37.5,10,25,50,17.5,37.5,10,25,50))
management_format$invpop <- management$invpop 
management_format <- rbind(management_format, data.frame(startyr=c(2024,2024,2024,2024,2024,2001,2005,2009,2013,2017,2023), removal_numb=c(10,17.5,25,37.5,50,rep(0,6)), invpop=c(0.9*356378843,0.825*356378843,0.75*356378843,0.625*356378843,0.5*356378843,rep(356378843,6))))
qplot(management_format$invpop)
qplot(log10(management_format$invpop))

#ggplot(data=management_format, aes(x=startyr,y=invpop,color=removal_numb)) + geom_point() + geom_text(aes(label=removal_numb)) + scale_y_log10()

#Remove NA values
management_format <- management_format[!is.na(management_format$invpop),]

#Look for outliers in variogram
vgm.cloud <- variogram(invpop~1, ~startyr+removal_numb, management_format,cloud=T)
plot(vgm.cloud$dist,vgm.cloud$gamma)

#Analyse anisotropy
vgm.aniso <- variogram(invpop~1, ~startyr+removal_numb, management_format, alpha=c(0,90))
plot(vgm.aniso)

#Convert data to spatial points data frame
coordinates(management_format) <- ~startyr+removal_numb
#class(management_format)
#summary(management_format)

#Create interpolation surface grid
x.range <- as.numeric(c(1999, 2025))  # min/max x of the interpolation area
y.range <- as.numeric(c(0, 50))  # min/max y of the interpolation area
grd <- expand.grid(startyr = seq(from = x.range[1], to = x.range[2], by = 0.1), removal_numb = seq(from = y.range[1],to = y.range[2], by = 0.1))  # expand points to grid
coordinates(grd) <- ~startyr+removal_numb
gridded(grd) <- TRUE

#Compute empirical variogram without logging invpop
invpop_vgm <- variogram(invpop~startyr+removal_numb, management_format)
#invpop_vgm
plot(invpop_vgm)

#Fit semivariogram with Gaussian function (other functions do not work)
invpop_fit = fit.variogram(invpop_vgm, model = vgm("Gau"))
plot(invpop_vgm, invpop_fit)

#Run universal kriging using fitted variogram
krigepop <- gstat::krige(invpop~startyr+removal_numb, management_format,grd, model=invpop_fit)  # apply kriging model for the data
krigepop.output = as.data.frame(krigepop)
#krigepop.output
names(krigepop.output)[1:3] <- c("startyr", "removal_numb", "invpop")
krigepop.output[krigepop.output$invpop < 0, "invpop"] <- 0.001


pop <- ggplot(krigepop.output, aes(x = startyr, y = removal_numb)) +
  geom_tile(aes(fill = (invpop)/10^6)) +
  #geom_point(data=as.data.frame(coordinates(management_format)),aes(x = startyr, y = removal_numb), alpha = 1/3) +
  #geom_text(data=as.data.frame(management_format), aes(x = startyr, y = removal_numb,label=format(invpop,scientific=T)),hjust=0.5, vjust=2) +
  theme_classic() +
  scale_x_continuous(expand=c(0,0), limit=c(1999,2025)) + 
  scale_y_continuous(expand=c(0,0)) +
  labs(x = 'Start of yearly removal (year)', y = 'Yearly mortality rate from trapping \n(% of population)') +
  scale_fill_distiller(type = "seq", palette = 'Spectral', direction = -1,
                       name = expression(atop(2025~population~size~phantom (1000000),(10^6~crayfish)~phantom (1000000)~phantom (1000000))),
                       breaks = c(10,100,200,300,356),
                       labels= c("<0.001", 100,200,300,356)) +
  theme(legend.title.align=-0.25,
        text = element_text(size = 12),
        legend.background = element_rect(fill = "transparent"))
pop

#Get legend as a grob
legend_2 <- get_legend(pop)
#Remove legend
pop_nolegend <- pop + theme(legend.position='none')

######################################################################
# PLOT SPREAD AND POPULATION HEATMAPS TOGETHER

#Make sure tha the top graph is aligned to the bottom graph's x-axis
p1 <- ggplot_gtable(ggplot_build(spread_nolegend))
p2 <- ggplot_gtable(ggplot_build(pop_nolegend))
maxWidth = unit.pmax(p1$widths[2:3], p2$widths[2:3])
p1$widths[2:3] <- maxWidth
p2$widths[2:3] <- maxWidth
lay= rbind(c(1,1,1,2,2))

#Need to use rbind to be able to use the size="last" argument which forces the two plotting areas to be equal despite the fact that the bottom one has an x axis
grid.arrange(rbind(p1, p2, size="last"), rbind(legend_1,legend_2, size='last'), ncol = 6, layout_matrix = lay)


pdf("Control_graphs_6_nopoint.pdf",width=5,height=6)
grid.arrange(rbind(p1, p2, size="last"), rbind(legend_1,legend_2, size='last'), ncol = 6, layout_matrix = lay)
dev.off()

########################################################################################################################################################################
########################################## FIGURE 7: EFFORT REQUIRED FOR CONTROL STRATEGIES ############################################################################
# SHOW EFFORT
effort2001_10perc_2 <- read.csv("Network23_test16_2025_control2001_10perc_2/effort_stat.csv")
effort2001_10perc_2$startyr <- 2001
effort2001_10perc_2$effortlevel <- 10
effort2001_25perc_2 <- read.csv("Network23_test16_2025_control2001_25perc_2/effort_stat.csv")
effort2001_25perc_2$startyr <- 2001
effort2001_25perc_2$effortlevel <- 25
effort2001_50perc_2 <- read.csv("Network23_test16_2025_control2001_50perc_2/effort_stat.csv")
effort2001_50perc_2$startyr <- 2001
effort2001_50perc_2$effortlevel <- 50

effort2009_10perc_2 <- read.csv("Network23_test16_2025_control2009_10perc_2/effort_stat.csv")
effort2009_10perc_2$startyr <- 2009
effort2009_10perc_2$effortlevel <- 10
effort2009_25perc_2 <- read.csv("Network23_test16_2025_control2009_25perc_2/effort_stat.csv")
effort2009_25perc_2$startyr <- 2009
effort2009_25perc_2$effortlevel <- 25
effort2009_50perc_2 <- read.csv("Network23_test16_2025_control2009_50perc_2/effort_stat.csv")
effort2009_50perc_2$startyr <- 2009
effort2009_50perc_2$effortlevel <- 50

effort2017_10perc_3<- read.csv("Network23_test16_2025_control2017_10perc_3/effort_stat.csv")
effort2017_10perc_3$startyr <- 2017
effort2017_10perc_3$effortlevel <- 10
effort2017_25perc_2 <- read.csv("Network23_test16_2025_control2017_25perc_2/effort_stat.csv")
effort2017_25perc_2$startyr <- 2017
effort2017_25perc_2$effortlevel <- 25
effort2017_50perc_2 <- read.csv("Network23_test16_2025_control2017_50perc_2/effort_stat.csv")
effort2017_50perc_2$startyr <- 2017
effort2017_50perc_2$effortlevel <- 50

effort_bind <- rbind(effort2001_10perc_2, effort2001_25perc_2, effort2001_50perc_2,effort2009_10perc_2,effort2009_25perc_2,effort2009_50perc_2, effort2017_10perc_3, effort2017_25perc_2, effort2017_50perc_2)

cumeffort <- adply(effort_bind, 1, function(x) {
  sum(effort_bind[effort_bind$year <= x$year &
                    effort_bind$startyr == x$startyr &
                    effort_bind$effortlevel == x$effortlevel ,"Total_Effort"])
  
})
effort_bind$cumeffort <- cumeffort$V1

#Compute mean yearly effort to apply at edges in test scenario
mean(effort_bind[effort_bind$startyr == 2017 & effort_bind$effortlevel == 50, "Total_Effort"])
mean(effort_bind[effort_bind$startyr == 2017 & effort_bind$effortlevel == 25, "Total_Effort"])
mean(effort_bind[effort_bind$startyr == 2017 & effort_bind$effortlevel == 10, "Total_Effort"])



effort_yearly <- ggplot(effort_bind, aes(x = year, group=interaction(effortlevel, startyr))) +
  #geom_point(aes(y = Total_Effort/1000, color = interaction(effortlevel,startyr)), size = 3) + 
  geom_line(aes(y = cumeffort/1000, color = interaction(effortlevel,startyr), 
                linetype=interaction(effortlevel,startyr)), size = 1.8,alpha=1,lineend="round") +
  theme_classic() + 
  scale_colour_manual(values = c("#6baed6", "#2171b5", "#08306b", "#a1d99b","#41ab5d","#006d2c","#fb6a4a","#cb181d","#67000d"), 
                      name = "Yearly mortality rate from trapping \n (% of population)")+
  scale_linetype_manual(values = rep(c('solid','longdash','dashed'),each=3),
                        name = "Yearly mortality rate from trapping \n (% of population)")+
  scale_x_continuous(expand=c(0,0),limits=c(2000,2024)) + 
  scale_y_continuous(expand=c(0,0), limits = c(0,1000)) +
  labs(x = "Year", y = expression(paste("Cumulative effort (",10^3," trap nights)"))) +
  #legend.title hjust argument doesn't work so have to adjust alignment by making an annotation
  annotate("text", label="Yearly mortality rate from \ntrapping (% of population)", x=2004,y=825,hjust = 0,lineheight=0.8)+
  annotate("text", label='2001', x=2005,y=750,angle=90, color="#08519c")+
  annotate("text", label='2009', x=2005,y=675,angle=90,color="#238b45")+
  annotate("text", label='2017', x=2005,y=600,angle=90,color="#cb181d")+
  annotate("text", label='Starting year', x=2004,y=675,angle=90) +
  theme(plot.margin = unit(c(0.2,0.4,0.2,0), "cm"),
        text = element_text(size = 12),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.y=element_text(vjust=-1),
        legend.title = element_blank(),
        legend.key.height= unit(0.35,"cm"),
        legend.key.width= unit(2.6,"cm"),
        legend.text.align = 0,
        legend.position = c(0.36,0.68),
        legend.background = element_rect(fill = "transparent"))
effort_yearly

pdf(file.path(outdir,"Control_graphs_effort_3.pdf"),width=4,height=4)
effort_yearly
dev.off()

