source(paste(getwd(),"/Final_Project_Data.R",sep=""))
library(spdep)
library(lme4)
library(AER)
library(MASS)
library(sjPlot)
#Dataset is Final_tidy_dataset

sum_stats= Final_tidy_dataset %>%
  group_by(year) %>%
  summarize(mean = mean(Cases),
            stdev = sd(Cases),
            min = min(Cases),
            max = max(Cases))



#merge with shapefiles
names(NYS_only_counties)[6]="County"
Final_shape_data = left_join(NYS_only_counties,Final_tidy_dataset)

Final_shape_data$centroid=st_centroid(Final_shape_data$geometry)
Final_shape_data$longitude=st_coordinates(st_centroid(Final_shape_data$geometry))[,1]
Final_shape_data$latitude=st_coordinates(st_centroid(Final_shape_data$geometry))[,2]

#below is plain model, controlling for longitude

model1=glm(Cases ~ Emps_per_pop + longitude + year,
    data = Final_shape_data,
    family = 'poisson'(link='log'),offset=log(estimate))
summary(model1)
par(mfrow=c(2,2))
plot(model1)
#ugly diagnostics
dev.off()
#Check for temporal autocorrelation
model2=glm(Cases ~ year,
           data = Final_shape_data,
           family = 'poisson'(link='log'),offset=log(estimate))
summary(model2)
acf(residuals(model2))
#autocorrelation!


model3=glmer(formula=Cases ~ Emps_per_pop + (1|County) + (1|year),
              data=Final_shape_data,family="poisson"(link='log'),
              offset=log(estimate))

summary(model3)
acf(residuals(model3))
#temporal autocorrelation is controlled
jpeg(filename = "tempcontrol.jpeg",width=12,height=6,units='in',res=300)
par(mfrow=c(1,2))
acf(residuals(model2))
acf(residuals(model3))
dev.off()
#check for overdispersion
overdisp_fun = function(model){
  rdf <- df.residual(model)
  rp <- residuals(model,type='pearson')
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf,lower.tail = FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}
overdisp_fun(model3)
# we have overdisperson


Final_shape_data$longcat=ifelse(Final_shape_data$longitude>=-75.25,1,0)
Final_shape_data$longcat2=ifelse(Final_shape_data$longitude<=-76,1,0)

model4 = glmer.nb(formula = Cases ~ Emps_per_pop + longcat + (1|County) + (1|year),
                  data=Final_shape_data,offset=log(estimate))
summary(model4)
acf(residuals(model4))
#Emps is significant, look for moran's i residuals



#testing for spatial autocorrelation of residuals

res=data.frame(res=abs(residuals(model4)))
new_df=cbind(Final_shape_data,res)
tm_shape(new_df)+
  tm_polygons(col='res')

#finding neighbors

nb_NYS=poly2nb(Final_shape_data)
one_yr_only = Final_shape_data %>% filter(year==2014)


k1=knn2nb(knearneigh(one_yr_only$centroid))
critical.threshold = max(unlist(nbdists(k1,coords = one_yr_only$centroid)))
nb.dist.band=dnearneigh(one_yr_only$centroid,0,critical.threshold)
dist.band.card=card(nb.dist.band)

polygon(NYS_only_counties$County)
plot(nb.dist.band,one_yr_only$centroid,col='black',cex=.5)


#average number of links is 4.736842 
# do 5 nearest neighbors
# need to create nb2listw
mat_list=nb2listw(knn2nb(knearneigh(one_yr_only$centroid,k=5)))


mat_list[["neighbours"]][[285]]=0
for(i in 58:285){
  if(i<115){mat_list[["neighbours"]][[i]]=mat_list[["neighbours"]][[i-57]]}
  if(i<172 & i >= 115){mat_list[["neighbours"]][[i]]=mat_list[["neighbours"]][[i-114]]}
  if(i<229 & i >= 172){mat_list[["neighbours"]][[i]]=mat_list[["neighbours"]][[i-171]]}
  if(i>=229 ){mat_list[["neighbours"]][[i]]=mat_list[["neighbours"]][[i-228]]}
  }
for(i in 1:285){
  mat_list[["weights"]][[i]]=mat_list[["weights"]][[1]]
  }


moran.test(residuals(model4),listw=mat_list)
#significant! residual spatial autocorrelation exists
# must create autocovariate weights

dt=st_drop_geometry(Final_shape_data)

Coordinates=SpatialPointsDataFrame(cbind(dt$longitude,dt$latitude),dt)

Final_shape_data$weights=autocov_dist(z=residuals(model4),xy=Coordinates,type='inverse',longlat = T)

#try to model with autocov weights

model5 = glmer.nb(formula = Cases ~ Emps_per_pop + longcat + weights +(1|County) + (1|year),
                  data=Final_shape_data,offset=log(estimate))

summary(model5)
acf(residuals(model5))
#still less temporal autocorrelation
overdisp_fun(model5)
#overdispersion is gone
mat_list_final=nb2listw(knn2nb(knearneigh(one_yr_only$centroid,k=5)))


mat_list_final[["neighbours"]][[285]]=0
for(i in 58:285){
  if(i<115){mat_list_final[["neighbours"]][[i]]=mat_list_final[["neighbours"]][[i-57]]}
  if(i<172 & i >= 115){mat_list_final[["neighbours"]][[i]]=mat_list_final[["neighbours"]][[i-114]]}
  if(i<229 & i >= 172){mat_list_final[["neighbours"]][[i]]=mat_list_final[["neighbours"]][[i-171]]}
  if(i>=229 ){mat_list_final[["neighbours"]][[i]]=mat_list_final[["neighbours"]][[i-228]]}
}
for(i in 1:285){
  mat_list_final[["weights"]][[i]]=mat_list_final[["weights"]][[1]]
}

moran.test(residuals(model5),listw=mat_list_final)

res2=data.frame(res=abs(residuals(model5)))
new_df2=cbind(Final_shape_data,res2)
tm_shape(new_df2)+
  tm_polygons(col='res')



moran.test(residuals(model5),listw=mat_list)
moran.test(residuals(model4),listw=mat_list)

AIC(model5)
AIC(model4)

# adding spatial weighst did not reduce autocorrelation
# likely because distances are far apart, cluster distance is large between centroid?

#AIC is lowest for model5
model5 = glmer.nb(formula = Cases ~ Emps_per_pop + longcat + weights +(1|County) + (1|year),
                  data=Final_shape_data,offset=log(estimate))
Final_shape_data$`Percent of Population Employed Outdoors`=Final_shape_data$Emps_per_pop
Final_shape_data$`Longitude < -75.25`=Final_shape_data$longcat
Final_shape_data$`Autocovariate Weights`=Final_shape_data$weights
Final_shape_data$`Cases of Lyme disease`=Final_shape_data$Cases
Final_shape_data$Year=Final_shape_data$year


tab_model(model5,model4,show.aic = T,title = "Final model with and without spatial weights",
          pred.labels = c("Intercept","Percent of Population Employed Outdoors","Longitude \u2265 -75.25",
                          "Autocovariate Weights"),file="RegressionTable.html")
library(webshot)
webshot("RegressionTable.html","FinalTable.png",vheight = 8,vwidth=12,)
