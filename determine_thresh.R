library(tidyverse)
library(plotly)
library(lubridate)

###########################################
## all sentinel 2 below
ground <- read.csv('sent_ground.csv') %>%
  pivot_longer(!Name, names_to = 'date', values_to ='binary_snow')
  

ndsi <- read.csv('sent_ndsi.csv') %>%
  pivot_longer(!Name, names_to = 'date', values_to = 'ndsi' )


thresh <- left_join(ground, ndsi, by=c('Name', 'date')) %>%
  filter(!binary_snow == 'na')

treshold <- ggplot(thresh, aes(binary_snow, ndsi,color=date)) + 
  geom_point()


ggplotly(treshold)

#going with senti threshold of 0.3

thresh_stats <- thresh %>%
  mutate(T_F = if_else(ndsi > 0.3 & binary_snow=='snow',
                       'truepos',
                       if_else(ndsi < 0.3 & binary_snow=='nosnow',
                         'trueneg',
                         if_else(ndsi > 0.3 & binary_snow=='nosnow',
                                 'falsepos',
                                 'falseneg'))))

thresh_totals <- thresh_stats %>%
  group_by(T_F) %>%
  count()

thresh_totals
prec_sent <- 56 / (56+1)
recall_sent <- 56 / (56 + 4)
fscore_sent <- 2 * ((prec_sent*recall_sent)/(prec_sent+recall_sent))
###############################################################
#comparing planet to ground data

planet <- read.csv('planet_snow.csv') %>%
  pivot_longer(!Name, names_to = 'date', values_to = 'landcover' )

str(planet)

plancomp <- left_join(ground, planet, by=c('Name', 'date')) %>% 
  filter(!date == 'X06.20.2021') %>%
  filter(!binary_snow == 'na')


plan_stats <- plancomp %>%
  mutate(T_F = if_else(landcover > 0 & binary_snow=='snow',
                       'truepos',
                       if_else(landcover == 0 & binary_snow=='nosnow',
                               'trueneg',
                               if_else(landcover > 0 & binary_snow=='nosnow',
                                       'falsepos',
                                       'falseneg'))))

plan_totals <- plan_stats %>%
  group_by(T_F) %>%
  count()

prec_plan <- 47 / (47+3)
recall_plan <- 47 / (47 + 16)
fscore_plan <- 2 * ((prec_plan*recall_plan)/(prec_plan+recall_plan))

######################################################################

##compare the snow cover areas

# snowarea <- read.csv('both_snowcover.csv')
# 
# str(snowarea)
# 
# area <- snowarea %>%
#   pivot_longer(!c(name, product), names_to='date', values_to = 'percent_area')

#write.csv(area, 'areas.csv')

PlotFormat = theme(axis.text=element_text(size=20, color="black"),
                   axis.title.x=element_text(size=22, hjust=0.5, margin=margin(t=20, r=20, b=20, l=20), color="black"),              
                   axis.title.y=element_text(size=22, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20), color="black"),              
                   plot.title=element_text(size=26,face="bold",hjust=0.5, margin=margin(t=20, r=20, b=20, l=20)),      
                   legend.title=element_text(size=14, color="black"),                                                                    
                   legend.text=element_text(size=14, color="black"),                                                                   
                   legend.position = "right", 
                   #panel.grid.major = element_blank(), 
                   #panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   #axis.line = element_line(colour = "black"),
                   strip.text = element_text(size=28),
                   panel.border = element_rect(colour = "black", fill=NA, size=1),
                   legend.key=element_blank())

areas <- read.csv('areas.csv') %>%
  mutate(date = ymd(date))

str(areas)

areas$name <- factor(areas$name,      # Reordering group factor levels
                        levels = c("BL4", "Montgomery", "Michigan trib", "Tunnel",
                                   "Aspen", "Mt. Campus"))

PLOT="areadiff"
ggplot(areas, aes(date, percent_area, shape=product, color=burn, linetype=product)) + 
  geom_point(size=4) + geom_line(size=1) +
  facet_wrap(~name) +
  theme_bw() + PlotFormat +
  labs(x='Date', y='Snow Cover Area') +
  scale_color_manual(name="burn",
                    values=c("burned" = 'red',
                             'partially burned' = 'orange',
                             'unburned' = 'darkgreen')) +
  scale_shape_manual(name='product',
                     values=c('planet' = 16,
                              'sentinel2' = 17)) +
  guides(shape=guide_legend(keywidth = 3, keyheight = 1))

ggsave(paste(PLOT,".png",sep=""), width = 15, height = 9)

####################################################

# percentdiff <- areas %>%
#   pivot_wider(names_from = product, values_from = percent_area)
# 
# write.csv(percentdiff, 'percentdiff.csv')  

perdiff <- read.csv('percentdiff.csv') %>%
  mutate(date = ymd(date))

str(perdiff)

perdiff <- perdiff %>%
  mutate(planetper = planet*100,
         sentinelper = sentinel2*100,
    percdiff = (abs(planetper-sentinelper)/((planetper+sentinelper)/2))*100) %>%
  select(!c(X.1, X))

         