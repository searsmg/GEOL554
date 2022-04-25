

library(raster)
library(tidyverse)
library(ggplot2)


minus <-raster('C:/Users/sears/Documents/Classes_SP22/GEOL 554/Labs/Lab10/lab10_hi/Minus_DSM.tif')

ras <- as.data.frame(minus, xy=T)

ggplot(ras, aes(x=x, y=y, fill=Minus_DSM)) + 
  geom_raster()


ras_sub <- ras %>%
  na_if(-9999) %>% 
  filter(Minus_DSM >= 1 | Minus_DSM <= -1,
    x <262500, 
    y <2150000)

ggplot(ras_sub, aes(x=x, y=y, fill=Minus_DSM)) + 
  geom_raster()

sum(ras_sub$Minus_DSM)
