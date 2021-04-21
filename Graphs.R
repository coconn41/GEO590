source(paste(getwd(),"/Final_Project_Data.R",sep=""))
library(viridisLite)

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
ggplot(data=Final_tidy_dataset)+
  geom_point(aes(x=year,y=Cases))+
  geom_line(aes(x=year,y=Cases,group=County),color='black')+
  theme_classic()+
  xlab('Year')+
  ylab("Lyme Cases")+
  facet_wrap(.~County)

summ_dat = Final_tidy_dataset %>%
  group_by(year) %>%
  summarize(Case_tots = sum(Cases))

jpeg(filename = 'casesbyyear.jpeg',width=10,height = 7,units='in',res=300)
ggplot(data=summ_dat)+
  geom_point(aes(x=year,y=Case_tots))+
  geom_line(aes(x=year,y=Case_tots))+
  theme_classic()+
  ylab("Total Lyme Cases")+
  xlab("Year")

dev.off()
plot=st_drop_geometry(Final_shape_data)
plot$longcat=ifelse(plot$longcat==0,)
ggplot(data=plot)+
  geom_point(aes(x=year,y=Cases))+
  geom_line(aes(x=year,y=Cases,group=County),color='black')+
  geom_smooth(aes(x=year,y=Cases,method='loess'))+
  facet_wrap(.~longcat)+theme_classic()

              