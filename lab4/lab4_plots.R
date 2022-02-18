## GEOL 554 lab 4 scripts
## 2022-02-16
## really just making some figs 

library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(gridExtra)

area <- read.csv("lab4/lake_data.csv")


PlotFormat = theme(axis.text=element_text(size=20, color="black"),
                   axis.title.x=element_text(size=22, hjust=0.5, margin=margin(t=20, r=20, b=20, l=20), color="black"),              
                   axis.title.y=element_text(size=22, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20), color="black"),              
                   plot.title=element_text(size=26,face="bold",hjust=0.5, margin=margin(t=20, r=20, b=20, l=20)),      
                   legend.title=element_text(size=18, color="black"),                                                                    
                   legend.text=element_text(size=18, color="black"),                                                                   
                   legend.position = "right", 
                   panel.grid.major = element_line(colour = "grey80"),
                   #panel.grid.minor = element_line(colour = "grey80"),
                   #panel.grid.major = element_blank(), 
                   #panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   #axis.line = element_line(colour = "black"),
                   strip.text = element_text(size=28),
                   panel.border = element_rect(colour = "black", fill=NA, size=1),
                   legend.key=element_blank())
PLOT="Q1"
ggplot(area, aes(year, size_km2)) +
  geom_point(size=4) + geom_line(size=1)+
  theme_bw() +
  labs(x="Year", y=expression("Lake Area "(km^2))) +
  PlotFormat

ggsave(paste(PLOT,".png",sep=""), width = 15, height = 9)       

ndsi <- read.csv("lab4/NDSI.csv")


ndsi <- ndsi %>%
  mutate(Month = recode(Month, 
                        "10"="Oct-20",
                        "11"="Nov-20",
                        "12"="Dec-20",
                        "1"="Jan-21",
                        "2"="Feb-21",
                        "3"="Mar-21",
                        "4"="Apr-21",
                        "5"="May-21",
                        "6"="Jun-21"))

ndsi$Month <- factor(ndsi$Month, levels=c("Oct-20","Nov-20","Dec-20","Jan-21","Feb-21",
                                           "Mar-21","Apr-21","May-21","Jun-21"))  

PLOT="NDSI"
ggplot(ndsi, aes(x = Name, y = MEDIAN)) +
  geom_pointrange(aes(ymin = MIN, ymax = MAX), size=1) +
  facet_wrap(~Month) +
  labs(x="Watershed", y="NDSI") +
  PlotFormat +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave(paste(PLOT,".png",sep=""), width = 15, height = 9)


persis <- ndsi %>%
  filter(Name %in% c("bl4", "michiganditch", "montgomery"))

PLOT="persistent"
persfig <- ggplot(persis, aes(x=Month, y=MEDIAN, color=Name)) + 
  geom_point(size=5, shape=17) +
  labs(x="Date", y="Median NDSI") +
  PlotFormat +
  scale_colour_brewer(palette="Dark2")

ggsave(paste(PLOT,".png",sep=""), width = 15, height = 9)

trans <- ndsi %>%
  filter(Name %in% c("aspen", "south fork", "tunnel", "dadd", "washout", "dry"))

PLOT="trans"
transfig <- ggplot(trans, aes(x=Month, y=MEDIAN, color=Name)) + 
  geom_point(size=5, shape=17) +
  labs(x="Date", y="Median NDSI") +
  PlotFormat + scale_colour_brewer(palette="Dark2")

ggsave(paste(PLOT,".png",sep=""), width = 15, height = 9)

PLOT="combo"
combo <- grid.arrange(transfig, persfig)

ggsave(file="combo.png", combo, width=15, height=9)

thresh <- ndsi %>%
  filter(MEDIAN > 0.42)


ggplot(thresh, aes(x=Month, y=MEDIAN, color=Name)) + geom_point()
       