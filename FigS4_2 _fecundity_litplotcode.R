#Author: Mathis L. Messager

#Purpose: plots carapace length-fecundity relationships from a systematic review of the literature
#for rusty crayfish Faxonius/Orconectes rusticus and prepare figures:
# - Figure S4.2

#For references, see: Messager and Olden (2018) - Supplementary information: Table S4.3

library(ggplot2)

#----------------------------- Figure S4.2 --------------------------------------------
#Plot carapace length-fecundity relationships
prinsova68 <- function (CL) {12.50*CL - 160.49}
langlois35 <- function(CL) {17.42*CL - 278.08}
prinspleo68 <- function(CL) {8.31*CL - 91.49}
corey87 <- function(CL) {12.51*CL - 135.36}
corey88_river <- function (CL) {11.5*CL - 126.2}
corey88_lake <- function(CL) {13.1*CL - 132.9}
lormanpleo80 <- function(CL) {3.772*CL - 27.2}
lormanyg80 <- function(CL) {3.425*CL - 20.5}
hein2002 <- function(CL) {10^(0.0329 * CL + 1.0636)}
hein2003 <- function(CL) {10^(0.0357 * CL + 1.0672)}
loughman09 <- function(CL) {4.1963*CL + 43.222}
sorrenson12 <- function(CL) {13.316*CL - 214.4}

df <- data.frame(CL=seq(20,27))
langlois35(x)
prinspleo68(x)
corey87(x)
corey88_river(x)
lormanpleo80(x)
hein2002(x)
hein2003(x)
sorrenson12(x)

ggplot(df, aes(CL)) + 
   stat_function(fun = prinsova68, color = "#225ea8", size = 1.25) +
   stat_function(fun = langlois35, color = "#fc8d62", size = 0.6) + 
   stat_function(fun = prinspleo68, color = "#66c2a5", size = 1.25) + 
   stat_function(fun = corey87, color = "#66c2a5", size = 1.25)+ 
   stat_function(fun = corey88_river, color = "#66c2a5", size = 1.25) + 
   stat_function(fun = corey88_lake, color = "#fc8d62", size = 1.25) +
   stat_function(fun = lormanpleo80, color = "#fc8d62", size = 1.25) +
   stat_function(fun = hein2002, color = "#fc8d62", size = 1.25) +
   stat_function(fun = hein2003, color = "#fc8d62", size = 1.25) +
   stat_function(fun = loughman09, color = "#66c2a5", size = 0.6) + 
  stat_function(fun = lormanyg80, color = "#e41a1c", size = 1.25) +
  stat_function(fun = sorrenson12, color = "#66c2a5", size = 1.25) +
  theme_bw() +
  xlab("Carapace length (mm)") + 
  ylab("Number of pleopodal eggs")

ggplot(df, aes(CL)) + 
  stat_function(fun = prinsova68, color = "#225ea8", size = 1) +
  stat_function(fun = langlois35, color = "#fc8d62", size = 1) + 
  stat_function(fun = prinspleo68, color = "#66c2a5", size = 1) + 
  stat_function(fun = corey87, color = "#66c2a5", size = 1)+ 
  stat_function(fun = corey88_river, color = "#66c2a5", size = 1) + 
  stat_function(fun = corey88_lake, color = "#fc8d62", size = 1) +
  stat_function(fun = lormanpleo80, color = "#fc8d62", size = 1) +
  stat_function(fun = hein2002, color = "#fc8d62", size = 1) +
  stat_function(fun = hein2003, color = "#fc8d62", size = 1) +
  stat_function(fun = loughman09, color = "#66c2a5", size = 1) + 
  stat_function(fun = lormanyg80, color = "#e41a1c", size = 1) +
  stat_function(fun = sorrenson12, color = "#66c2a5", size = 1) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  xlab("Carapace length (mm)") + 
  ylab("Number of eggs")

ggplot(df, aes(CL)) + 
  geom_hline(yintercept = 30, color = "#225ea8", size = 1) + 
  geom_hline(yintercept = 50, color = "#fc8d62", size = 1) + 
  geom_hline(yintercept = 70, color = "#66c2a5", size = 1) + 
  geom_hline(yintercept = 90, color = "#e41a1c", size = 1) +
  theme_bw()