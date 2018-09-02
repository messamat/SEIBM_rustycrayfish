#Author: Mathis Messager
#Related publication: Messager ML, Olden JD. Individual-based models forecast the spread and inform the management 
#                                             of an emerging riverine invader. Divers Distrib.2018;00:1-14. https://doi.org/10.1111/ddi.12829
#Purpose: produce fecundity plots for Figure S1.1

library(ggplot2)

#UPDATE:
setwd('F:/Chapter2_HexSim_Crayfish/doc/Manuscript/Figures/Diagram')

#### Convert yearly survival to monthly for HexSim model
data.frame(yearly = seq(0.01:0.99, by = 0.01), monthly = seq(0.01:0.99, by = 0.01)^(1/12))
monthly = seq(0.01:0.99, by = 0.01)^(1/12)

norm_proba <- ggplot(data.frame(x = c(0, 400)), aes(x)) +
  stat_function(fun = dnorm, args = list(mean = 80, sd = 10), color = "#ece2f0", size = 1) + 
  stat_function(fun = dnorm, args = list(mean = 120, sd = 20), color = "#a6bddb", size = 1) + 
  stat_function(fun = dnorm, args = list(mean = 150, sd = 30), color = "#1c9099", size = 1) + 
  theme_bw() + 
  labs(x = "Number of offspring", y = "Probability density") +
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        text = element_text(size = 8),
        axis.line = element_line(colour = "black"),
        axis.line.y=element_blank(),
        axis.text.y=element_blank(),axis.ticks.y=element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

pdf("norm_proba_1.pdf",width = 2, height = 2)
norm_proba
dev.off()

df <- data.frame(dens = c(0, 15), stage1 = c(40,0), stage2 = c(60,0), stage3 = c(75,0))

dens_plot <- ggplot(df, aes(x= dens)) + 
  geom_line(aes(y = stage1), color = "#ece2f0", size = 1) + 
  geom_line(aes(y = stage2), color = "#a6bddb", size = 1) + 
  geom_line(aes(y = stage3), color = "#1c9099", size = 1) +
  theme_bw() + 
  labs(x = "Crayfish density (indiv/m2)", y = "Mean recruitment") +
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        text = element_text(size = 8),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank())

pdf("dens_plot_1.pdf",width = 2, height = 2)
dens_plot
dev.off()
