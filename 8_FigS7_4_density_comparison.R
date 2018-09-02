#Author: Mathis L. Messager
#Related publication: Messager ML, Olden JD. Individual-based models forecast the spread and inform the management 
#                                             of an emerging riverine invader. Divers Distrib.2018;00:1-14. https://doi.org/10.1111/ddi.12829
#Purpose: Plot comparison of observed and HexSim-modeled crayfish densities in 2010 and 2016 in the John Day River
#Produce: Figure S7.4


library(ggplot2)
library(foreign)
library(grid)
library(gridExtra)
library(gtable)

#Function to extract legend from graph as a grob to be re-inserted later
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

rootdir <- 'F:/Chapter2_HexSim_Crayfish' #TO UPDATE
setwd(file.path(rootdir,'/doc/Manuscript/Figures/Density_comparison')) 
hexsimres <- file.path(rootdir, 'src/Crayfish_model/Parameters_and_outputs/Network_23')

#################################################### 2010 ############################################################################################
dens_aug10 <- read.dbf(file.path(hexsimres,'Network23_test16_2025/Sorenson_Olden_sampling_edges_18_join_1.dbf'))
dens_aug10$mod_low <- with(dens_aug10, avgmoddens-stdmoddens)
dens_aug10[dens_aug10$mod_low<0,'mod_low']<-0
dens_aug10$mod_high<- with(dens_aug10, avgmoddens+stdmoddens)

dens_aug10_sub <- dens_aug10[dens_aug10$Observers == 'Sorenson',]

sorenson_mainstem <- ggplot(dens_aug10_sub[dens_aug10_sub$GNIS_NAME == 'John Day River',], aes(x=updist/1000, label=Site_name)) +
  geom_ribbon(aes(ymin=mod_low, ymax=mod_high),alpha=1/2, fill='orange') +
  geom_line(aes(y=avgmoddens),size=1,color='red') +
  geom_line(aes(y=net_n/30),size=1,color='black') + 
  geom_line(data=dens_aug10_sub[dens_aug10_sub$GNIS_NAME == 'John Day River' & !is.na(dens_aug10_sub$Trap_CPUE),],
            aes(y=as.numeric(as.character(Trap_CPUE))),size=1,color='black',linetype='dashed') + 
  geom_point(aes(y=net_n/30),color='black') +
  #geom_text(aes(y=net_n/30)) +
  theme_classic()
sorenson_mainstem

#################################################### 2016 ############################################################################################
dens_aug16 <- read.dbf(file.path(hexsimres, 'Network23_test16_2025/Sampled_sites_notes_CPUE_kick_edges_18_join.dbf'))
dens_aug16[dens_aug16$Site_ID==107,'updist'] <- dens_aug16[dens_aug16$Site_ID==107,'updist'] - 50
dens_aug16[dens_aug16$Site_Name =="Burnt Ranch",'kickmean'] <- 0.01
dens_aug16[dens_aug16$Site_Name == "John Day Upstream of dam",'kickmean'] <- 0.01
dens_aug16[dens_aug16$Site_Name == "Monument Boat Launch",'kickmean'] <- 0.01

dens_aug16[dens_aug16$Site_Name == "South Fork by the Johnsons",'updist'] <- 3500

dens_aug16$kick_low <- with(dens_aug16, kickmean-kickstd)
dens_aug16[dens_aug16$kick_low<0,'kick_low']<-0
dens_aug16$kick_high<- with(dens_aug16, kickmean+kickstd)
dens_aug16$mod_low <- with(dens_aug16, avgmoddens-stdmoddens)
dens_aug16[dens_aug16$mod_low<0,'mod_low']<-0
dens_aug16$mod_high<- with(dens_aug16, avgmoddens+stdmoddens)

dens_aug16$River_Trib <- as.character(dens_aug16$River_Trib)
dens_aug16[dens_aug16$River_Trib=='Upper mainsteam','River_Trib'] <- 'Upper mainstem'

#####Create custom legend####
legendplot <- ggplot(dens_aug16, aes(x=updist, y=avgmoddens)) +
  geom_ribbon(aes(ymin=mod_low, ymax=mod_high, fill='Modeled density (± SD)'), alpha=1/2) +
  geom_line(aes(y=avgmoddens, color='Modeled density (± SD)'), size=1) +
  geom_point(aes(y=avgmoddens, color='Modeled density (± SD)'), size=2, alpha=1/2) +
  geom_ribbon(aes(ymin=mod_low, ymax=mod_high, fill='Observed CPUE (± SD)'), alpha=1/2) +
  geom_line(aes(y=avgmoddens, color='Observed CPUE (± SD)'),size=1) +
  geom_point(aes(y=avgmoddens, color='Observed CPUE (± SD)'),size=2, alpha=1/2) +
  scale_color_manual(name='',values=c('red','black')) +
  scale_fill_manual(name='',values=c('orange','grey'))
legend <- get_legend(legendplot)

##################################### NORTH FORK ################################################
northobs <- ggplot(dens_aug16[(dens_aug16$GNIS_NAME == 'North Fork John Day River') & dens_aug16$kickmean > 0,], 
                   aes(x=updist/1000)) +
  geom_ribbon(aes(ymin=kick_low, ymax=kick_high),alpha=1/2, fill='gray') +
  geom_line(aes(y=kickmean),size=1,color='black',alpha=1/2) +
  geom_point(aes(y=kickmean),color='black',size=2,alpha=1/2) +
  geom_point(data=dens_aug16[dens_aug16$GNIS_NAME == 'North Fork John Day River' & dens_aug16$kickmean==0,],aes(y=kickmean+0.1),color='black',shape=4,size=3,alpha=1/2) +
  labs(y='Kick-seining CPUE') +
  scale_x_continuous(breaks=seq(0,35,10), limits=c(0,35), expand=c(0,0)) +
  scale_y_continuous(limits=c(0,3), expand=c(0,0)) +
  geom_vline(xintercept=35, color='red')+
  theme_classic() + 
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        text=element_text(size=12),
        legend.position = c(0.75,0.85),
        legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.background = element_rect(fill = "transparent"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

northmod <- ggplot(dens_aug16[(dens_aug16$GNIS_NAME == 'North Fork John Day River') & dens_aug16$avgmoddens > 0,],
                   aes(x=updist/1000)) +
  geom_ribbon(aes(ymin=mod_low, ymax=mod_high), alpha=1/2,fill='orange') +
  geom_line(aes(y=avgmoddens), size=1,color='red') +
  geom_point(aes(y=avgmoddens),color='red',size=2, alpha=1/2) +
  geom_point(data=dens_aug16[dens_aug16$GNIS_NAME == 'North Fork John Day River' & dens_aug16$avgmoddens == 0,],aes(y=avgmoddens+0.5),color='red',shape=4,size=3, alpha=1/2) +
  labs(x='River kilometers from confluence with mainstem JDR', y=expression(paste('Modeled crayfish density (crayfish/ ',m^2,')',sep=""))) +
  scale_x_continuous(breaks=seq(0,35,10), limits=c(0,35), expand=c(0,0)) +
  scale_y_continuous(limits=c(0,10), breaks=c(0,2,5,10),expand=c(0,0)) +
  theme_classic() +
  theme(text=element_text(size=12),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y= element_text(color='red'),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = margin(unit='cm', c(0,0.25,0.25,0)),
        legend.position='none',
        plot.background = element_rect(fill = "transparent"),
        axis.ticks.y = element_line(color='red'),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))


g3<-ggplot_gtable(ggplot_build(northobs))
g4<-ggplot_gtable(ggplot_build(northmod))

#overlap the panel of the 2nd plot on that of the 1st plot
pp<-c(subset(g4$layout, name=="panel", se=t:r))
g_north<-gtable_add_grob(g3,g4$grobs[[which(g4$layout$name =="panel")]], pp$t, pp$l, pp$b,pp$r)

#Add axis
ia <- which(g4$layout$name == "axis-l")
ga <- g4$grobs[[ia]]
ax <- ga$children[[2]]
ax$widths <- rev(ax$widths)
ax$grobs <- rev(ax$grobs)
ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.1, "cm")
g_north <- gtable_add_cols(g_north, g4$widths[g4$layout[ia, ]$l], length(g_north$widths)-1)
g_north <- gtable_add_grob(g_north, ax, pp$t, length(g_north$widths) - 1, pp$b)

grid.draw(g_north)


##################################### SOUTH FORK ################################################
southobs <- ggplot(dens_aug16[(dens_aug16$GNIS_NAME == 'South Fork John Day River') & dens_aug16$kickmean > 0,], 
                   aes(x=updist/1000)) +
  geom_ribbon(aes(ymin=kick_low, ymax=kick_high),alpha=1/2, fill='gray') +
  geom_line(aes(y=kickmean),size=1,color='black') +
  geom_point(aes(y=kickmean),color='black',size=2, alpha=1/2) +
  geom_point(data=dens_aug16[dens_aug16$GNIS_NAME == 'South Fork John Day River' & dens_aug16$kickmean==0,],aes(y=kickmean+2),color='black',shape=4,size=3, alpha=1/2) +
  labs(y='Kick-seining CPUE') +
  scale_x_continuous(breaks=seq(0,35,10), limits=c(0,35), expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0), limit=c(0,60)) +
  geom_vline(xintercept=35, color='red')+
  theme_classic() + 
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        text=element_text(size=12),
        legend.position = c(0.75,0.85),
        legend.title = element_blank())

southmod <- ggplot(dens_aug16[(dens_aug16$GNIS_NAME == 'South Fork John Day River') & dens_aug16$avgmoddens > 0,],
                   aes(x=updist/1000)) +
  geom_ribbon(aes(ymin=mod_low, ymax=mod_high), alpha=1/2,fill='orange') +
  geom_line(aes(y=avgmoddens), size=1,color='red') +
  geom_point(aes(y=avgmoddens),color='red',size=2, alpha=1/2) +
  geom_point(data=dens_aug16[dens_aug16$GNIS_NAME == 'South Fork John Day River' & dens_aug16$avgmoddens == 0,],aes(y=avgmoddens+0.2),color='red',shape=4,size=3, alpha=1/2) +
  labs(x='River kilometers from confluence with mainstem JDR', y=expression(paste('Modeled crayfish density (crayfish/ ',m^2,')',sep=""))) +
  scale_x_continuous(breaks=seq(0,35,10), limits=c(0,35), expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0), limit=c(0,4)) +
  theme_classic() +
  theme(text=element_text(size=12),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y= element_text(color='red'),
        axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = margin(unit='cm', c(0,0.25,0.25,0)),
        axis.ticks.y = element_line(color='red'),
        legend.position='none',
        panel.background = element_blank())


g5<-ggplot_gtable(ggplot_build(southobs))
g6<-ggplot_gtable(ggplot_build(southmod))

#overlap the panel of the 2nd plot on that of the 1st plot
pp<-c(subset(g6$layout, name=="panel", se=t:r))
g_south<-gtable_add_grob(g5,g6$grobs[[which(g6$layout$name =="panel")]], pp$t, pp$l, pp$b,pp$r)

#Add axis
ia <- which(g6$layout$name == "axis-l")
ga <- g6$grobs[[ia]]
ax <- ga$children[[2]]
ax$widths <- rev(ax$widths)
ax$grobs <- rev(ax$grobs)
ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.1, "cm")
g_south <- gtable_add_cols(g_south, g6$widths[g6$layout[ia, ]$l], length(g_south$widths)-1)
g_south <- gtable_add_grob(g_south, ax, pp$t, length(g_south$widths) - 1, pp$b)

#Add right-axis title
axt <- which(g6$layout$name == "ylab-l")
gaxt <- g6$grobs[[axt]]
gaxt$widths <- rev(gaxt$widths)
gaxt$grobs <- rev(gaxt$grobs)
g_south <- gtable_add_cols(g_south, g6$widths[g6$layout[axt, ]$l], length(g_south$widths) - 1)
g_south <- gtable_add_grob(g_south, gaxt, pp$t, length(g_south$widths) - 1, pp$b)

#grid.draw(g_south)

#################################### MAINS STEM ###################
mainobs <- ggplot(dens_aug16[dens_aug16$River_Trib %in% c('Lower mainstem','Upper mainstem') & dens_aug16$kickmean > 0,], aes(x=updist/1000)) +
  geom_ribbon(aes(ymin=kick_low, ymax=kick_high), fill='grey',alpha=1/2) +
  geom_point(aes(y=kickmean),color='black',size=2, alpha=1/2) +
  geom_line(aes(y=kickmean),color='black',size=1) +
  labs(x='River kilometers from mainstem of Columbia River', y='Kick-seining CPUE') +
  geom_point(data=dens_aug16[dens_aug16$River_Trib %in% c('Lower mainstem','Upper mainstem') & dens_aug16$kickmean==0,],aes(y=kickmean+1),color='black',shape=4,size=3, alpha=1/2) +
  scale_x_continuous(breaks=seq(0,400,50), limits=c(0,400), expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0), limit=c(0,50)) +
  geom_vline(xintercept=292.7, color='black',alpha=1/2, size=1)+
  geom_vline(xintercept=335.230, color='black',alpha=1/2, size=1)+
  geom_vline(xintercept=391.271, color='black',alpha=1/2, size=1)+
  geom_vline(xintercept=400, color='red')+
  annotation_custom(grob = g_south, xmin = 130, xmax = 260, ymin=25, ymax=50)+
  annotation_custom(grob = g_north, xmin = 0, xmax = 130, ymin=25, ymax=50)+
  annotation_custom(grob = legend, xmin = 65, xmax = 65, ymin = 7, ymax=7) +
  annotate('text', x=282.7,y=45,angle=90, label='NF confluence') +
  annotate('text', x=325.230,y=45,angle=90, label='SF confluence') +
  annotate('text', x=381.271,y=45,angle=90, label='Low head dam') +
  annotate('text', x=100,y=45, label='NF') +
  annotate('text', x=230,y=45, label='SF') +
  theme_classic() + 
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        text=element_text(size=12))

mainmod <- ggplot(dens_aug16[dens_aug16$River_Trib %in% c('Lower mainstem','Upper mainstem') & dens_aug16$avgmoddens > 0,], aes(x=updist/1000)) +
  geom_ribbon(aes(ymin=mod_low, ymax=mod_high), fill='orange', alpha=1/2) +
  geom_point(aes(y=avgmoddens),color='red',size=2, alpha=1/2) +
  geom_point(data=dens_aug16[dens_aug16$River_Trib %in% c('Lower mainstem','Upper mainstem') & dens_aug16$avgmoddens == 0,],aes(y=avgmoddens+0.5),color='red',shape=4,size=3, alpha=1/2) +
  geom_line(aes(y=avgmoddens),color='red', size=1) +
  labs(x='River kilometers from mainstem of Columbia River') +
  scale_x_continuous(breaks=seq(0,400,50), limits=c(0,400), expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0), limit=c(0,20), name=expression(paste('Modeled crayfish density (crayfish/',m^2,')',sep=""))) +
  annotate('text', x=130,y=10, label='River kilometers from mainstem of JDR') +
  theme_classic() +
  theme(text=element_text(size=12),
        axis.title.x=element_text(hjust=0.75),
        plot.margin = margin(unit='cm', c(0,0.5,0.25,0)),
        axis.line = element_line(colour = "black"),
        axis.text.y= element_text(color='red'),
        axis.title.y = element_text(color='red',angle=270,vjust=-1),
        axis.ticks.y = element_line(color='red'),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank())


g1<-ggplot_gtable(ggplot_build(mainobs))
g2<-ggplot_gtable(ggplot_build(mainmod))

#overlap the panel of the 2nd plot on that of the 1st plot
pp<-c(subset(g2$layout, name=="panel", se=t:r))
g_main<-gtable_add_grob(g1,g2$grobs[[which(g2$layout$name =="panel")]], pp$t, pp$l, pp$b,pp$r)

#Add axis
ia <- which(g2$layout$name == "axis-l")
ga <- g2$grobs[[ia]]
ax <- ga$children[[2]]
ax$widths <- rev(ax$widths)
ax$grobs <- rev(ax$grobs)
ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.1, "cm")
g_main <- gtable_add_cols(g_main, g2$widths[g2$layout[ia, ]$l], length(g_main$widths)-1)
g_main <- gtable_add_grob(g_main, ax, pp$t, length(g_main$widths) - 1, pp$b)

#Add right-axis title
axt <- which(g2$layout$name == "ylab-l")
gaxt <- g2$grobs[[axt]]
gaxt$widths <- rev(gaxt$widths)
gaxt$grobs <- rev(gaxt$grobs)
g_main <- gtable_add_cols(g_main, g2$widths[g2$layout[axt, ]$l], length(g_main$widths) - 1)
g_main <- gtable_add_grob(g_main, gaxt, pp$t, length(g_main$widths) - 1, pp$b)

grid.draw(g_main)

pdf("Density_comparison_2016_20180616.pdf",width=8,height=6)
grid.draw(g_main)
dev.off()