#Week 7 Homework
library(readxl)
Erie_ACS_dat <- read_excel("Week 7/ACSDP5Y2012.DP05-2021-03-22T183122.xlsx",
                                                 sheet = "Data")
Mari_ACS_dat <- read_excel("Week 7/ACSDP5Y2012.DP05-2021-03-22T184733.xlsx",
                                                    sheet = "Data")
Mari_Dec_dat <- read_excel("Week 7/DECENNIALSF12010.P1-2021-03-22T184916.xlsx",
           sheet = "Data")
Erie_Dec_dat <- read_excel("Week 7/DECENNIALSF12010.P1-2021-03-22T185016.xlsx",
                 sheet = "Data")
# Remove +-
Erie_ACS_dat2 = Erie_ACS_dat[3,]
Erie_MOEcols=seq(3,947,by=4)
for(i in 1:length(Erie_ACS_dat2)){
  for (j in 1:nrow(Erie_ACS_dat2)){
  if(i %in% Erie_MOEcols){Erie_ACS_dat2[j,i]=substring(text=Erie_ACS_dat2[j,i],
                                                  first = 2,last = 100)}
  }}

Mari_ACS_dat2 = Mari_ACS_dat[3,]
Mari_MOEcols=seq(3,23,by=4)
for(i in 1:length(Mari_ACS_dat2)){
  for (j in 1:nrow(Mari_ACS_dat2)){
    if(i %in% Mari_MOEcols){Mari_ACS_dat2[j,i]=substring(text=Mari_ACS_dat2[j,i],
                                                    first = 2,last = 100)}
}}

# Calculate lower and upper limits
Erie_seqs=sort(c(seq(2,946,by=4),seq(3,947,by=4)),decreasing=F) 
Erie_ACS_dat3 = Erie_ACS_dat2[,Erie_seqs]

Mari_seqs=sort(c(seq(2,25,by=4),seq(3,25,by=4)),decreasing=F)
Mari_ACS_dat3 = Mari_ACS_dat2[,Mari_seqs]



