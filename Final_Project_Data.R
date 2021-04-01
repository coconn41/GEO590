library(tidyverse)
library(tabulizer)
library(ipumsr)

#Reading in NYS Data
tdir=tempdir()
for(i in 2014:2018){
  url = paste("https://www.health.ny.gov/statistics/diseases/communicable/",i,"/docs/cases.pdf",sep="")
  out = extract_tables(url)
  if(i==2014){df = as.data.frame(do.call(rbind, out[5]))
  df2 = df[c(2:60),c(1,2)]
  }
  if(i==2015){df = as.data.frame(do.call(rbind, out[4]))
  df2 = df[c(2:60),c(1,9)]
  }
  if(i>2015){df = as.data.frame(do.call(rbind, out[4]))
  df2 = df[c(2:60),c(1,8)]}
  if(i==2014){df3 = df2
  names(df3)=c("County","2014")
  }
  else{names(df2)=c("County",paste(i))
    df3 = merge(df3,df2,by="County")}
}
Lyme_Cases=df3
remove(df,df2,df3,final,final2,i,tdir,url,out)

#Read in IPUMS data
ddi <- read_ipums_ddi("usa_00005.xml")
IPUMS_data <- read_ipums_micro(ddi)
# Selected employment types:
# 4210 = First-Line Supervisors of Landscaping, Lawn Service, and Groundskeeping Workers
# 4250 = 	Grounds Maintenance Workers 
# 6005 = First-Line Supervisors of Farming, Fishing, and Forestry Workers
# 6100 = Fishing and Hunting Workers
# 6120 = Forest and Conservation Workers
# 6130 = Logging Workers

#Filter for employment type
Outdoor_workers = IPUMS_data %>%
  filter(OCC == 4210 |
         OCC == 4250 |
         OCC == 6005 |
         OCC == 6100 |
         OCC == 6120 |
         OCC == 6130 )
# Do person weight calculations
County_PW_Calcs = Outdoor_workers %>%
  filter(COUNTYFIP != 0) %>%
  group_by(YEAR,COUNTYFIP,OCC) %>%
  summarize(Persons = sum(PERWT))

State_PW_Calcs = Outdoor_workers %>%
  group_by(YEAR,OCC) %>%
  summarize(Persons = sum(PERWT))

# Get population data
vars = load_variables(year = 2014,dataset = "acs5",cache = T)
for(i in 1:49){
  if(i==1){a=paste("B01001_00",i,sep="")}
  if(i>1 & i<10){b=paste("B01001_00",i,sep="")}
  if(i>9){b=paste("B01001_0",i,sep="")}
  if(i==2){varnames=c(a,b)}
  if(i>2){varnames=c(varnames,b)}
  if(i==49){varnames=c("B00001_001",varnames)}
}

required_labels = vars %>%
  filter(name %in% varnames)

for(i in 2014:2018){
  if(i==2014){
  df=get_acs(geography = "county",year=i,
             state = "NY",
             survey = "acs5",
             variables = varnames)
  df$year=i}
  if(i==2015){df2=get_acs(geography = "county",year=i,
                  state = "NY",
                  survey = "acs5",
                  variables = varnames)
       df2$year=i
       df3=rbind(df2,df)}
  if(i>2015){df2=get_acs(geography = "county",year=i,
                         state = "NY",
                         survey = "acs5",
                         variables = varnames)
  df2$year=i
  df3=rbind(df3,df2)}
  }

names(required_labels)[1]="variable"
Pop_data = left_join(df3,required_labels)
