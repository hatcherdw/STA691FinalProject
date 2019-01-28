library(tidyverse)
library(reshape2)

data <- tribble(
  ~Year,~Certified,~"Certified-Expired",~Withdrawn,~Denied,
    2008,21092,28113,2063,10729, 
    2009,16041,13461,3389,5356,
    2010,32657,37580,2736,8439,
    2011,18714,41149,2960,10384,
    2012,2,54579,3265,8642,
    2013,13742,21461,3075,5874,
    2014,35615,27018,4016,4349,
    
    2015,41223,37715,4362,5999,
    2016,58750,57183,4650,5560,
    2017,42080,45529,3581,6413,
    2018,72187,37363,3971,6255 
)

data2 <- data %$%
    mutate(.,Total=Certified+`Certified-Expired`+Withdrawn+Denied) %>%
    mutate(.,Determined=Certified+`Certified-Expired`+Denied)

data.m <- melt(data,id.vars="Year")

ggplot(data.m, aes(x = Year, y = value,fill=variable)) +
  geom_bar(stat='identity',position="fill")+
  theme(panel.background = element_rect(fill = 'white', colour = 'white'))+
  theme(legend.title=element_blank())+
  ylab('Frequency')+
  xlab("Fiscal Year")+
  theme(legend.position = "top",legend.spacing.x = unit(0.5, 'cm'))+
  scale_fill_brewer(palette="Blues")+
  #theme( axis.line = element_line(colour = "black",size = 1, linetype = "solid"))+
  theme(text=element_text(size=16))+
  scale_y_continuous(breaks=seq(0,1,by=0.5),labels=c("0%","50%","100%"))

