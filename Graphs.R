source(paste(getwd(),"/Final_Project_Data.R",sep=""))
library(viridisLite)
library(gridExtra)
names(NYS_only_counties)[6]="County"
Final_shape_data = left_join(NYS_only_counties,Final_tidy_dataset)

Final_shape_data$centroid=st_centroid(Final_shape_data$geometry)
Final_shape_data$longitude=st_coordinates(st_centroid(Final_shape_data$geometry))[,1]
Final_shape_data$latitude=st_coordinates(st_centroid(Final_shape_data$geometry))[,2]

Final_shape_data$longcat=ifelse(Final_shape_data$longitude>=-75.25,1,0)
Final_shape_data$longcat2=ifelse(Final_shape_data$longitude<=-76,1,0)

ggplot(data=Final_tidy_dataset)+
  geom_point(aes(x=year,y=Emps_per_pop,group=County),color='black')+
  geom_line(aes(x=year,y=Emps_per_pop,group=County),color='black')
# No change over time
jpeg(filename = 'profileplot.jpg',width=10,height = 7,units='in',res=300)
ggplot(data=Final_tidy_dataset)+
  geom_point(aes(x=year,y=Cases))+
  geom_line(aes(x=year,y=Cases,group=County),color='black')+
  xlab('Year')+
  ylab("Lyme Cases")+
  facet_wrap(.~County)+
  theme(axis.text.x = element_text(angle=90,vjust=.5,hjust=1),
        plot.background = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color='black'))
dev.off()

summ_dat = Final_tidy_dataset %>%
  group_by(year) %>%
  summarize(Case_tots = sum(Cases))


jpeg(filename = 'casesbyyear.jpeg',width=10,height = 7,units='in',res=300)
tots=ggplot(data=summ_dat)+
  geom_point(aes(x=year,y=Case_tots))+
  geom_line(aes(x=year,y=Case_tots))+
  theme_classic()+
  ylab("Total Lyme Cases (NYS)")+
  xlab("Year")
Final_tidy_dataset$factyear=as.factor(Final_tidy_dataset$year)
bp=ggplot(data=Final_tidy_dataset,aes(y=Cases,x=factyear))+
  geom_boxplot()+
  theme_classic()+
  ylab("Total Lyme Cases (County per year)")+
  xlab("Year")

jpeg(filename="Figure1.jpeg",width=8,height=10,units='in',res = 300)
grid.arrange(tots,bp)

dev.off()
plot=st_drop_geometry(Final_shape_data)
plot$longcat=ifelse(plot$longcat==0,)
ggplot(data=plot)+
  geom_point(aes(x=year,y=Cases))+
  geom_line(aes(x=year,y=Cases,group=County),color='black')+
  geom_smooth(aes(x=year,y=Cases,method='loess'))+
  facet_wrap(.~longcat)+theme_classic()

              