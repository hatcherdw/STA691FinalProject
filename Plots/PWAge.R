library(tidyverse)
library(ggplot2)

data <- readRDS('Transformed.rds')

PWAgeStats <- data %>%
    group_by(Status) %>%
    summarise(
        med = median(PWAge)/365    
    )

ggplot(data=data[which(data$PWAge < 3650),])+
    geom_boxplot(mapping=aes(y=PWAge/365,x=Status))+
    theme(panel.background = element_rect(fill = 'white', colour = 'white'))+
    xlab("")+
    ylab("Age of PWD (years)")+
    coord_flip()+
    scale_y_continuous(breaks=seq(0,10,by=2))+
    theme(text=element_text(size=16))+
    annotate('text',y=as.numeric(PWAgeStats[2,2]),x=2.5,label='16 months',fontface=2)+
    annotate('text',y=as.numeric(PWAgeStats[1,2]),x=1.5,label='8 months',fontface=2)
