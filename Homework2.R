library(readr)
library(tidyverse)

Erie_aggregate = read_csv("ACSST5Y2019.S1501_2021-02-24T103839/ACSST5Y2019.S1501_data_with_overlays_2021-02-24T103834.csv")
Erie_all <- read_csv("ACSST5Y2019.S1501_2021-02-24T082353/ACSST5Y2019.S1501_data_with_overlays_2021-02-23T191852.csv")
Erie_all=rbind(Erie_all,Erie_aggregate)
Erie_all = Erie_all[c(1:6,11:18,27,28,139,140,155,156)]
names(Erie_all)=c("GEO_ID","NAME","Est_tot_18-24","MOE_tot_18-24",
                  "Est_tot_18-24_<HS","MOE_tot_18-24_<HS","Est_tot_18-24_>Bac",
                  "MOE_tot_18-24_>Bac","Est_tot_>25","MOE_tot_>25","Est_tot_>25_<9th",
                  "MOE_tot_>25_<9th","Est_tot_>25_nodip","MOE_tot_>25_nodip",
                  "Est_tot_>25_GOPdeg","MOE_tot_>25_GOPdeg","Perc_1824_GOPdeg","MOE_Perc_1824_GOPdeg",
                  "Perc_>25_GOPdeg","MOE_Perc_gt25_GOP_deg")
names2drop=c("Est_tot_18-24","MOE_tot_18-24",
             "Est_tot_18-24_<HS","MOE_tot_18-24_<HS","Est_tot_18-24_>Bac",
             "MOE_tot_18-24_>Bac","Est_tot_>25","MOE_tot_>25","Est_tot_>25_<9th",
             "MOE_tot_>25_<9th","Est_tot_>25_nodip","MOE_tot_>25_nodip",
             "Est_tot_>25_GOPdeg","MOE_tot_>25_GOPdeg")
Erie_all = Erie_all %>%
  filter(GEO_ID != 'id')
numcols = c(3:20)
Erie_all[numcols] = sapply(Erie_all[numcols], as.numeric)

Erie_all2 = Erie_all %>%
  mutate(CI_1824_lt_HS = paste(`Est_tot_18-24_<HS`," [",
                               ifelse((`Est_tot_18-24_<HS`-`MOE_tot_18-24_<HS`)<0,0,`Est_tot_18-24_<HS`-`MOE_tot_18-24_<HS`),
                               " , ",`Est_tot_18-24_<HS`+`MOE_tot_18-24_<HS`,"]",sep=""),
         SE_1824_lt_HS = round(`MOE_tot_18-24_<HS`/1.645,digits=2),
         CV_1824_lt_HS = ifelse(`Est_tot_18-24_<HS`==0,"Und.",
                                paste(round(digits=2,x=(((`MOE_tot_18-24_<HS`/1.645)/`Est_tot_18-24_<HS`)*100)),"%",sep="")),
         CI_1824_GOPdeg = paste(`Est_tot_18-24_>Bac`," [",
                                ifelse((`Est_tot_18-24_>Bac`-`MOE_tot_18-24_>Bac`)<0,0,`Est_tot_18-24_>Bac`-`MOE_tot_18-24_>Bac`),
                                " , ",`Est_tot_18-24_>Bac`+`MOE_tot_18-24_>Bac`,"]",sep=""),
         SE_18_24_GOPdeg = round(`MOE_tot_18-24_>Bac`/1.645,digits=2),
         CV_18_24_GOPdeg = ifelse(`Est_tot_18-24_>Bac`==0,"Und.",
                                  paste(round(digits=2,x=(((`MOE_tot_18-24_>Bac`/1.645)/`Est_tot_18-24_>Bac`)*100)),"%",sep="")),
         CI_gt25_lt_HS = paste(`Est_tot_>25_<9th`+`Est_tot_>25_nodip`," [",
                               ifelse((`Est_tot_>25_<9th`+`Est_tot_>25_nodip`)-sqrt((`MOE_tot_>25_<9th`^2)+(`MOE_tot_>25_nodip`^2))<0,0,
                                      round(digits=2,(`Est_tot_>25_<9th`+`Est_tot_>25_nodip`)-sqrt((`MOE_tot_>25_<9th`^2)+(`MOE_tot_>25_nodip`^2)))),
                               " , ",round(digits=2,`Est_tot_>25_<9th`+`Est_tot_>25_nodip`+sqrt((`MOE_tot_>25_<9th`^2)+(`MOE_tot_>25_nodip`^2))),"]",sep=""),
         SE_gt25_lt_HS = round(digits=2,sqrt((`MOE_tot_>25_<9th`^2)+(`MOE_tot_>25_nodip`^2))/1.645),
         CV_gt25_lt_HS = ifelse(`Est_tot_>25_<9th`+`Est_tot_>25_nodip`==0,"Und.",
                                paste(round(digits=2,x=((sqrt((`MOE_tot_>25_<9th`^2)+(`MOE_tot_>25_nodip`^2))/1.645)/
                                                          (`Est_tot_>25_<9th`+`Est_tot_>25_nodip`)*100)),"%",sep="")),
         CI_gt25_GOPdeg = paste(`Est_tot_>25_GOPdeg`," [",
                                ifelse((`Est_tot_>25_GOPdeg`-`MOE_tot_>25_GOPdeg`)<0,0,`Est_tot_>25_GOPdeg`-`MOE_tot_>25_GOPdeg`),
                                " , ",`Est_tot_>25_GOPdeg`+`MOE_tot_>25_GOPdeg`,"]",sep=""),
         SE_gt25_GOPdeg = round(`MOE_tot_>25_GOPdeg`/1.645,digits=2),
         CV_gt25_GOPdeg =  ifelse(`Est_tot_>25_GOPdeg`==0,"Und.",
                                  paste(round(digits=2,x=(((`MOE_tot_>25_GOPdeg`/1.645)/`Est_tot_>25_GOPdeg`)*100)),"%",sep="")),
         CI_Perc_1824_GOPdeg = paste(`Perc_1824_GOPdeg`,"% [",
                                     round(digits=2,ifelse((`Perc_1824_GOPdeg`-`MOE_Perc_1824_GOPdeg`)<0,0,`Perc_1824_GOPdeg`-`MOE_Perc_1824_GOPdeg`)),
                                     "% , ",`Perc_1824_GOPdeg`+`MOE_Perc_1824_GOPdeg`,"%]",sep=""),
         SE_Perc_1824_GOPdeg = paste(round(`MOE_Perc_1824_GOPdeg`/1.645,digits=2),"%",sep=""),
         CV_Perc_1824_GOPdeg = ifelse(`Perc_1824_GOPdeg`==0,"Und.",
                                      paste(round(digits=2,x=(((`MOE_Perc_1824_GOPdeg`/1.645)/`Perc_1824_GOPdeg`)*100)),"%",sep="")),
         CI_Perc_gt25_GOPdeg = paste(`Perc_>25_GOPdeg`,"% [",
                                     round(digits=2,ifelse((`Perc_>25_GOPdeg`-`MOE_Perc_gt25_GOP_deg`)<0,0,`Perc_>25_GOPdeg`-`MOE_Perc_gt25_GOP_deg`)),
                                     "% , ",`Perc_>25_GOPdeg`+`MOE_Perc_gt25_GOP_deg`,"%]",sep=""),
         SE_Perc_gt25_GOPdeg = paste(round(`MOE_Perc_gt25_GOP_deg`/1.645,digits=2),"%",sep = ""),
         CV_Perc_gt25_GOPdeg = ifelse(`Perc_>25_GOPdeg`==0,"Und.",
                                      paste(round(digits=2,x=(((`MOE_Perc_gt25_GOP_deg`/1.645)/`Perc_>25_GOPdeg`)*100)),"%",sep=""))) %>%
  dplyr::select(-c(names2drop))

Alldat = Erie_all2 %>%
  filter(GEO_ID == "1400000US36029009110" |
           GEO_ID == "1400000US36029009104" | 
           GEO_ID == "0500000US36029") 



#Test one: between tracts

Bet_Tr_1824=ifelse(abs((Alldat[1,3]-Alldat[2,3])/((((Alldat[1,4]/1.645)*Alldat[1,3])^2) + (((Alldat[2,4]/1.645)*Alldat[2,3])^2)))>1.645,"Reject the Null","Fail to Reject")
#Not significantly different
Bet_Tr_gt25=ifelse(abs((Alldat[1,5]-Alldat[2,5])/((((Alldat[1,6]/1.645)*Alldat[1,5])^2) + (((Alldat[2,6]/1.645)*Alldat[2,5])^2)))>1.645,"Reject the Null","Fail to Reject")


#Test two: 91.09 and Erie County

Bet_91.09Erie_1824=ifelse(abs((Alldat[1,3]-Alldat[3,3])/((((Alldat[1,4]/1.645)*Alldat[1,3])^2) + (((Alldat[3,4]/1.645)*Alldat[3,3])^2)))>1.645,"Reject the Null","Fail to Reject")
#Not significantly different
Bet_91.09Erie_gt25=ifelse(abs((Alldat[1,5]-Alldat[3,5])/((((Alldat[1,6]/1.645)*Alldat[1,5])^2) + (((Alldat[3,6]/1.645)*Alldat[3,5])^2)))>1.645,"Reject the Null","Fail to Reject")

#Test three: 91.10 and Erie County
Bet_91.10Erie_1824=ifelse(abs((Alldat[2,3]-Alldat[3,3])/((((Alldat[2,4]/1.645)*Alldat[2,3])^2) + (((Alldat[3,4]/1.645)*Alldat[3,3])^2)))>1.645,"Reject the Null","Fail to Reject")
#Not significantly different
Bet_91.10Erie_gt25=ifelse(abs((Alldat[2,5]-Alldat[3,5])/((((Alldat[2,6]/1.645)*Alldat[2,5])^2) + (((Alldat[3,6]/1.645)*Alldat[3,5])^2)))>1.645,"Reject the Null","Fail to Reject")


Testing_results1=data.frame( `Names`= c("T91.09","T91.10","Erie"),
                             `T9.09` = c("X",Bet_Tr_1824,Bet_91.09Erie_1824),
                             `T9.10` = c(Bet_Tr_1824,"X",Bet_91.10Erie_1824),
                             `Erie` = c(Bet_91.09Erie_1824,Bet_91.10Erie_1824,"X"))
Testing_results2=data.frame( `Names`= c("T91.09","T91.10","Erie"),
                             `T9.09` = c("X",Bet_Tr_gt25,Bet_91.09Erie_gt25),
                             `T9.10` = c(Bet_Tr_gt25,"X",Bet_91.10Erie_gt25),
                             `Erie` = c(Bet_91.09Erie_gt25,Bet_91.10Erie_gt25,"X"))
write.csv(Alldat,file="Allcalcs.csv")
write.csv(Testing_results1,file = "Tests1.csv")
write.csv(Testing_results2,file="Tests2.csv")

