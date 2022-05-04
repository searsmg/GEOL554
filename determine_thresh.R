library(tidyverse)
library(plotly)

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
prec <- 56 / (56+1)
recall <- 56 / (56 + 4)
fscore <- 2 * ((prec*recall)/(prec+recall))
###############################################################

