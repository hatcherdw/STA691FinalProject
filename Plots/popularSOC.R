library(ggplot2)
library(tidyverse)

rm(list=ls())

load('Wrangled.rdata')

SOCTitles <- post2015 %>%
    filter(CASE_STATUS %in% c('Certified','Certified-Expired','Denied')) %>%
    group_by(PW_SOC_TITLE) %>%
    summarise(
        count = n()
    ) 

orderedSOCTitles <- SOCTitles[order(SOCTitles$count,decreasing=TRUE),]

ggplot(data=orderedSOCTitles[1:10,])+
    geom_bar(mapping=aes(x=reorder(PW_SOC_TITLE,count),y=count),stat='identity',fill='lightblue')+
    coord_flip()+
    theme(panel.background = element_rect(fill = 'white', colour = 'white'))+
    xlab("")+
    ylab("Count")+
    theme(text=element_text(size=16))+
    scale_y_continuous(breaks=seq(0,100000,by=50000),labels=c("0","50,000","100,000"))+
    theme(legend.position = 'none')
    