library(readr)
library(tidyverse)
PEPPOP2019_PEPANNRES_data_with_overlays_2021_02_11T114143 <- read_csv("PEPPOP2019.PEPANNRES_2021-02-15T110447/PEPPOP2019.PEPANNRES_data_with_overlays_2021-02-11T114143.csv")

newdat=PEPPOP2019_PEPANNRES_data_with_overlays_2021_02_11T114143[c(3,4)] 
newdat$POP=as.numeric(newdat$POP)
newdat = newdat %>%
  filter(DATE_CODE != "Estimate Date") %>%
  mutate(Year = substring(DATE_CODE,5,8),
         Type = ifelse(substring(DATE_CODE,1,1)==7,"ACS Yearly Estimates",""))

newdat[1,4]="2010 Census"
newdat[2,4]="ACS 2010 Baseline"

newdat2 = newdat %>%
  filter(Type=="ACS Yearly Estimates")
newdat3 = newdat %>%
  filter(Type!="ACS Yearly Estimates")


pdf(width = 7,height = 4,file = "Homework1_graph.pdf")
ggplot()+
  geom_line(data=newdat2,aes(Year,POP,group=1,col=Type))+
  geom_point(data = newdat2,aes(Year,POP,group=1,col=Type))+
  ylab("Population")+
  geom_point(data=newdat3,aes(Year,POP,col=Type),position = 'jitter')+
  labs(title = "Population Estimates Using Census Data",
       caption = "Data gathered from data.census.gov on 2/15/2021")+
  theme_classic()
dev.off()  
