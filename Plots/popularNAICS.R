library(tidyverse)
library(ggplot2)

rm(list=ls())

load('Wrangled.rdata')

NAICSTitles <- post2015 %>%
    group_by(NAICS_US_TITLE) %>%
    summarise(
        count = n()    
    )

orderedNAICSTitles <- NAICSTitles[order(NAICSTitles$count,decreasing = TRUE),]

ggplot(data=orderedNAICSTitles[1:10,]) +
    geom_bar(mapping=aes(x=reorder(NAICS_US_TITLE,count),y=count),stat='identity',fill='lightblue')+
    theme(panel.background = element_rect(fill = 'white', colour = 'white'))+
    coord_flip()+
    xlab("")+
    ylab("Count")+
    theme(text=element_text(size=16))
