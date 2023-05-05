rm(list = ls(all = TRUE)) #Start new session by clearing everything

# Title: "Graphs for N saving for Nitrogne paper"
# Author email: "t.sida@cgiar.org"
# Date: "September 2022"
# ---

##library(installr) ###Installs a package for updating R
###updateR() ###Updates R using library(installr) 22/06/20


###Load basic libraries

library(lattice)
library(caret) 
library(foreign)
library(readstata13)
library(stringr)
library(dplyr)
library(tidyr)
library(e1071)
library(prospectr) 
library(Hmisc)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(reshape)
library(jtools)
library(coefplot)
library(lme4)
library(lmerTest)
library(car)
library(olsrr) ### Model selection algorithm
library(sjPlot) ### To plot regression coefficients
library(ggforce) ### Accelerting ggplot2 for drowing shapes in the graphs
library(ggpubr) ### Arrange ggplot figures in column or row
library(MonteCarlo) ### For stochastic simulations in R
library(snow) ### Required to load MC

Nsaving <- read.csv("C:/Users/T.SIDA/OneDrive - CIMMYT/Nitrogen/Nsaving4.csv")

options(scipen=999) ### Disables the scientific notation (2e+05 etc) on the axises

names(Nsaving)
str(Nsaving)

Nsaving$Saved.Amount....<- as.numeric(Nsaving$Saved.Amount....)

library(scales)
label_percent(Nsaving$Saved.Amount.... )

cleanup = theme(panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              axis.line = element_line(size = 1, colour = "black"),
              axis.line.x = element_line(colour="black"),
              axis.line.y = element_line(colour="black"),
              legend.key = element_blank(),
              text = element_text(size=14),
              axis.text = element_text (size = 12, colour = "black")) 

Nsaving$Country_crop <- factor(Nsaving$Country_crop, levels = c("India-Maize" , "India-Rice" , "India-Wheat", "Ethiopia-Maize", "Ethiopia-Wheat", "Malawi-Maize"))

Nsaving$Saving.source <- factor(Nsaving$Saving.source, levels = c("Fertilizer Advisory", "High-N Fertilizer", "Legume", "Manure"))



alternative_1a <- ggplot(Nsaving, aes(x = Country_crop, y = Saved.Amount..MT.N., fill = Saving.source)) +
  geom_bar(position='stack', stat='identity', width = 0.4) +
  #scale_fill_grey(start = 0.7, end = 0.01) +
  theme(legend.position = c(0.8, 0.6), legend.text = element_text(size = 14))+
  labs(fill = "Source of Saving") +
  xlab("Country-Crop")+
  ylab("Amount of N saved (Metric Tonnes)")+
  #scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07", '#00BA38'))+
  scale_y_continuous(labels = comma) + ### Adds comma to the axis labels
  #ylim(0, 400000) +
  cleanup
  
show_col(hue_pal()(100)) ### Provides hex codes of colors

ggsave("alternative_1a.jpg", 
       path = "C:/Users/T.SIDA/OneDrive - CIMMYT/Nitrogen",
       dpi = 1000)



alternative_1b <- ggplot(Nsaving, aes(x = Country_crop, y = Saved.Amount....*100, fill = Saving.source)) +
  geom_bar(position='stack', stat='identity', width = 0.4) +
  #scale_fill_grey(start = 0.7, end = 0.01) +
  theme(legend.position = c(0, -0.6), legend.text = element_text(size = 14))+
  labs(fill = "Source of Saving") +
  xlab("Country-Crop")+
  ylab("\nProportion of N saved (% total N used)\n") +
  #scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07", '#00BA38'))+
  scale_y_continuous(labels = comma) + ### Adds comma to the axis labels
  #ylim(0, 400000) +
  cleanup 
  
ggsave("alternative_1b.jpg", 
       path = "C:/Users/T.SIDA/OneDrive - CIMMYT/Nitrogen",
       dpi = 1000)  

alternative_1 <- ggarrange(alternative_1a, alternative_1b, labels = c(" (a)", " (b)"),  
                                        ncol = 2, nrow = 1, hjust = -4, vjust = 2)

ggsave("alternative_1.jpg", 
       path = "C:/Users/T.SIDA/OneDrive - CIMMYT/Nitrogen",
       dpi = 1000)


alternative_2Vertical_clor <- ggarrange(alternative_1a, alternative_1b, labels = c(" (a)", " (b)"),  
                           ncol = 1, nrow = 2, hjust = -4, vjust = 2)

ggsave("alternative_2Vertical_color.jpg", 
       path = "C:/Users/T.SIDA/OneDrive - CIMMYT/Nitrogen",
       dpi = 1000)

Nsaving$Crop2

