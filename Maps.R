source(paste(getwd(),"/Final_Project_Data.R",sep=""))
library(tmap)
library(tmaptools)
library(viridisLite)
library(sf)

names(NYS_only_counties)[6]="County"
Final_shape_data = left_join(NYS_only_counties,Final_tidy_dataset)

Final_shape_data$centroid=st_centroid(Final_shape_data$geometry)
Final_shape_data$longitude=st_coordinates(st_centroid(Final_shape_data$geometry))[,1]
Final_shape_data$latitude=st_coordinates(st_centroid(Final_shape_data$geometry))[,2]

Final_shape_data$longcat=ifelse(Final_shape_data$longitude>=-75.25,1,0)
Final_shape_data$longcat2=ifelse(Final_shape_data$longitude<=-76,1,0)

cases_by_year_map=tm_shape(Final_shape_data)+
  tm_polygons(col='Cases',
              breaks=c(0,50,100,150,200,300,500,1000),
              palette=viridisLite::viridis(7),border.col = 'black')+
  tm_facets(by='year')+
  tm_layout(legend.text.size = 1.5,legend.title.size = 1.5)+
  tm_graticules(lines=F)+
  tm_compass(position=c('left','bottom'))+tm_scale_bar(position = c('left','bottom'));cases_by_year_map
tmap_save(cases_by_year_map,file="cases_year_map.pdf",width = 10,height = 10,units = 'in',dpi=300)


emp_summ_dat = Final_shape_data %>%
  group_by(County) %>%
  summarize(avg=mean(Emps_per_pop))
emp_summ_map=tm_shape(emp_summ_dat)+
  tm_polygons(col='avg',
              breaks=c(0,5,10,15,20,100),
              palette=viridisLite::viridis(5),
              title="Average % of pop. working outdoors")+
  tm_shape(emp_summ_dat)+
  tm_borders(col='black')+
  tm_graticules(lines=F)+
  tm_compass(position=c('left','bottom'))+
  tm_scale_bar(position=c('left','bottom'))+
  tm_layout(legend.title.size = 1.5,legend.text.size = 1.5);emp_summ_map
tmap_save(emp_summ_map,file="Employeesummary.pdf",width = 10,height = 10,units = 'in',dpi=300)

long_cats_map=tm_shape(Final_shape_data)+
  tm_polygons(col='longcat',breaks=c(0,.5,1),
              title="Centroid Longitude",
              labels=c("< -75.25","\u2265 -75.25"),
              palette=c('white','grey'),)+
  tm_shape(Final_shape_data)+
  tm_borders(col='black')+
  tm_compass(position=c('left','bottom'))+
  tm_scale_bar(position=c('left','bottom'))+
  tm_graticules(lines=F)+
  tm_layout(legend.text.size = 1.5,legend.title.size = 1.5);long_cats_map
tmap_save(long_cats_map,file="longcatsmap.jpeg",width = 10,height = 10,units = 'in',dpi=300)

Pop_est = Final_shape_data %>%
  group_by(County) %>%
  summarize(estimates=mean(estimate))

popmap=tm_shape(Pop_est)+
  tm_polygons(col='estimates',
              border.col='black',
              breaks=c(0,5000,10000,20000,40000,80000,1500000),
              palette=viridisLite::viridis(6),
              title="Mean population estimates (2014-2018)",
              labels=c("< 5,000","5,000 to 10,000","10,000 to 20,000",
                       "20,000 to 40,000","40,000 to 80,000","> 80,000"))+
  tm_graticules(lines=F)+
  tm_compass(position=c('left','bottom'))+
tm_scale_bar(position=c('left','bottom'))+
  tm_layout(legend.text.size = 1.5,legend.title.size = 1.5)
tmap_save(popmap,file="popmap.jpeg",width = 10,height=10,units='in',dpi=300)
