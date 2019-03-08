library(tidyverse)
library(ggplot2)

rm(list=ls())

data <- readRDS('Transformed.rds')

majorCodes <- data %>%
    group_by(SOCMajor) %>%
    summarise(
        count=n(),
        pctDenied=sum(Status=='Denied')/count
    )

ggplot(data=majorCodes) +
    geom_bar(mapping=aes(x=reorder(SOCMajor,-count),y=count),stat='identity',fill='lightblue')+
    theme(panel.background = element_rect(fill = 'white', colour = 'white'))+
    xlab("SOC Major Group")+
    ylab("Count")+
    theme(text=element_text(size=16))
