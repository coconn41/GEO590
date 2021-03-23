#Week 7 Homework
library(readxl)
library(tidyverse)
Erie_ACS_dat <- read_excel("Week 7/ACSDP5Y2012.DP05-2021-03-22T183122.xlsx",
                                                 sheet = "Data")
Mari_ACS_dat <- read_excel("Week 7/ACSDP5Y2012.DP05-2021-03-22T184733.xlsx",
                                                    sheet = "Data")
Mari_Dec_dat <- read_excel("Week 7/DECENNIALSF12010.P1-2021-03-22T184916.xlsx",
           sheet = "Data")
Erie_Dec_dat <- read_excel("Week 7/DECENNIALSF12010.P1-2021-03-22T185016.xlsx",
                 sheet = "Data")
# Remove +-
Erie_ACS_dat2 = Erie_ACS_dat[c(1,3),]
Erie_MOEcols=seq(3,947,by=4)
names(Erie_ACS_dat2)[Erie_MOEcols]="MOE"

for(i in 1:length(Erie_ACS_dat2)){
  if(i %in% Erie_MOEcols){Erie_ACS_dat2[2,i]=substring(text=Erie_ACS_dat2[2,i],
                                                  first = 2,last = 100)}}

Mari_ACS_dat2 = Mari_ACS_dat[c(1,3),]
Mari_MOEcols=seq(3,23,by=4)
for(i in 1:length(Mari_ACS_dat2)){
    if(i %in% Mari_MOEcols){Mari_ACS_dat2[2,i]=substring(text=Mari_ACS_dat2[2,i],
                                                    first = 2,last = 100)}}

# Calculate lower and upper limits
Erie_seqs=sort(c(seq(2,946,by=4),seq(3,947,by=4)),decreasing=F) 
Erie_ACS_dat3 = Erie_ACS_dat2[,Erie_seqs]

Mari_seqs=sort(c(seq(2,25,by=4),seq(3,25,by=4)),decreasing=F)
Mari_ACS_dat3 = Mari_ACS_dat2[,Mari_seqs]

new_erie_df=as.data.frame(t(Erie_ACS_dat3)[c(seq(1,474,by=2)),2])
new_mari_df=as.data.frame(t(Mari_ACS_dat3)[c(seq(1,12,by=2)),2])

new_erie_df$MOEs=as.numeric(t(Erie_ACS_dat3)[c(seq(2,474,by=2)),2])
new_mari_df$MOEs=as.numeric(t(Mari_ACS_dat3)[c(seq(2,12,by=2)),2])

names(new_erie_df)[1]="Estimate"
names(new_mari_df)[1]="Estimate"

new_erie_df$Estimate=as.numeric(gsub(",", "",new_erie_df$Estimate))
new_mari_df$Estimate=as.numeric(gsub(",", "",new_mari_df$Estimate))


new_erie_df$UpperLim=new_erie_df$Estimate + new_erie_df$MOEs
new_erie_df$LowerLim=new_erie_df$Estimate - new_erie_df$MOEs

new_mari_df$UpperLim=new_mari_df$Estimate + new_mari_df$MOEs
new_mari_df$LowerLim=new_mari_df$Estimate - new_mari_df$MOEs

Erie_Dec_dat2=Erie_Dec_dat[,2:238]
Erie_Dec_dat3=as.data.frame(t(Erie_Dec_dat2))
Erie_Dec_dat3$V1=as.numeric(gsub(",", "",Erie_Dec_dat3$V1))
names(Erie_Dec_dat3)[1]="Census"

new_erie_df2=cbind(new_erie_df,Erie_Dec_dat3)

Mari_Dec_dat2=Mari_Dec_dat[,2:7]
Mari_Dec_dat3=as.data.frame(t(Mari_Dec_dat2))
Mari_Dec_dat3$V1=as.numeric(gsub(",", "",Mari_Dec_dat3$V1))
names(Mari_Dec_dat3)[1]="Census"

new_mari_df2=cbind(new_mari_df,Mari_Dec_dat3)


#Calculate inconsistencies

new_erie_df2$Overestimate=ifelse(new_erie_df2$Census<new_erie_df2$LowerLim,1,0)
new_erie_df2$Underestimate=ifelse(new_erie_df2$Census>new_erie_df2$UpperLim,1,0)

new_mari_df2$Overestimate=ifelse(new_mari_df2$Census<new_mari_df2$LowerLim,1,0)
new_mari_df2$Underestimate=ifelse(new_mari_df2$Census>new_mari_df2$UpperLim,1,0)

new_erie_df3= tibble::rownames_to_column(.data = new_erie_df2,var="Tract")

new_mari_df3= tibble::rownames_to_column(.data=new_mari_df2,var = "Tract")

Erie_tab = new_erie_df3 %>%
  summarize(Overtots=sum(Overestimate),
            Undertots=sum(Underestimate),
            Total=n())

Mari_tab = new_mari_df3 %>%
  summarize(Overtots=sum(Overestimate),
            Undertots=sum(Underestimate),
            Total=n())

Erie_Percs = data.frame(County="Erie County, New York",
                        OverEstimation=round((Erie_tab$Overtots/Erie_tab$Total)*100,digits = 2),
                        UnderEstimation=round((Erie_tab$Undertots/Erie_tab$Total)*100,digits=2),
                        TotalTracts=Erie_tab$Total)

Mari_Percs = data.frame(County="Mariposa County, California",
                        OverEstimation=round((Mari_tab$Overtots/Mari_tab$Total)*100,digits=2),
                        UnderEstimation=round((Mari_tab$Overtots/Mari_tab$Total)*100,digits = 2),
                        TotalTracts=Mari_tab$Total)

Final_Table = rbind(Erie_Percs,Mari_Percs)

write.csv(file = "Week7_HW.csv",x = Final_Table)
