library(tidyverse)


Year = c(2009:2018)
perm = c(38247,81412,73207,66488,44149,70998,89151,126143,97603,119776)
h1b = c(240801,328903,352214,408038,433586,508674,605809,633943,624650,654360)
h2a = c(7857,7378,7361,8047,8388,9405,10339,8684,10097,11698)
h2b = c(7217,4535,4409,4211,4710,5464,6521,7209,8970,9490)
total = perm + h1b + h2a + h2b

accentColor = 'lightblue'

ggplot()+
  geom_line(mapping = aes(x=Year,y=total),color='grey',size=2,lineend='round',linetype=2)+
  geom_line(mapping = aes(x=Year,y=perm),color=accentColor,size=2,lineend='round')+
  geom_line(mapping = aes(x=Year,y=h1b),color='grey',size=2,lineend='round')+
  geom_line(mapping = aes(x=Year,y=h2a+h2b),color='grey',size=2,lineend='round')+
  theme(panel.background = element_rect(fill = 'white', colour = 'white'))+
  ylab('Determinations')+
  xlab('Fiscal Year')+
  scale_y_continuous(breaks=seq(0,800000,by=200000),labels=c("0","200,000","400,000","600,000","800,000"))+
  theme( axis.line = element_line(colour = "black",size = 1, linetype = "solid"))+
  theme(text=element_text(size=16))+ 
  annotate("text", x = 2016.2, y = 800000, label = "Total",size=6,color='grey')+
  annotate("text", x = 2016.2, y = 670000, label = "H-1B",size=6,color='grey')+
  annotate("text", x = 2016.2, y = 180000, label = "Permanent",size=6,color=accentColor)+
  annotate("text", x = 2016.2, y = 60000, label = "H-1A + H-2A",size=6,color='grey')

  

  

