library(tidyverse)
library(tabulizer)
library(tidycensus)
library(stringr)

#Reading in NYS Data
tdir=tempdir()

source(paste(getwd(),"/Shapefile_cleaning.R",sep=""))
# Above creates NYS_Cnt_Names and NYS_county_names

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
Lyme_Cases=df3 %>%
  mutate(County = str_to_title(tolower(County)))
Lyme_Cases_tidy = pivot_longer(Lyme_Cases,cols = c("2014","2015","2016","2017","2018"),
                               names_to = "Year")
names(Lyme_Cases_tidy)=c("County","year","Cases")
Lyme_Cases_tidy$Cases=as.numeric(Lyme_Cases_tidy$Cases)
Lyme_Cases_tidy$year=as.numeric(Lyme_Cases_tidy$year)
remove(df,df2,df3,i,url,out)

#Read in IPUMS data
#ddi <- read_ipums_ddi("usa_00005.xml")
#IPUMS_data <- read_ipums_micro(ddi)
# Selected employment types:
# 4210 = First-Line Supervisors of Landscaping, Lawn Service, and Groundskeeping Workers
# 4250 = 	Grounds Maintenance Workers 
# 6005 = First-Line Supervisors of Farming, Fishing, and Forestry Workers
# 6100 = Fishing and Hunting Workers
# 6120 = Forest and Conservation Workers
# 6130 = Logging Workers

#Filter for employment type
#Outdoor_workers = IPUMS_data %>%
 # filter(OCC == 4210 |
  #       OCC == 4250 |
   #      OCC == 6005 |
    #     OCC == 6100 |
     #    OCC == 6120 |
      #   OCC == 6130 )
# Do person weight calculations
#County_PW_Calcs = Outdoor_workers %>%
  #filter(COUNTYFIP != 0) %>%
  #group_by(YEAR,COUNTYFIP,OCC) %>%
  #summarize(Persons = sum(PERWT))

#State_PW_Calcs = Outdoor_workers %>%
#  group_by(YEAR,OCC) %>%
#  summarize(Persons = sum(PERWT))

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
Pop_data = left_join(df3,required_labels) %>%
  filter(variable=="B00001_001") %>%
  mutate(County = str_remove(NAME," County, New York"),
         year = as.numeric(year)) %>%
  select(County,estimate,year) %>%
  filter(!County %in% NYC_Cnt_Names)
remove(df,df2,df3,vars,required_labels)
  #Read in Occupation Data
NYSoccu = "https://statistics.labor.ny.gov/qcew.zip"
# Data dictionary is here: https://statistics.labor.ny.gov/lstechqcew.shtm
NAICScats = c(11,111,112,113,114,115)
#above categories are:
#Agriculture, Forestry, Fishing and Hunting
#Crop Production
#Animal Production
#Forestry and Logging
#FIshing, Hunting and Trapping

if(file.exists(paste(getwd(),"/qcew",sep=""))==F){
  if(file.exists(paste(tdir,"/qcew",sep=""))==F){
    download.file(url = NYSoccu,destfile = file.path(tdir,"Occup_data.zip"))
    unzip(zipfile = file.path(tdir,"Occup_data.zip"),exdir=paste(tdir,"/Occup_dat_fold",sep=""))}
    for(i in 2014:2018){
    if(i==2014){labor_df = read.table(file=paste(tdir,"/Occup_dat_fold/qcew_annual_","2018",".txt",sep=""),
                          header = T,sep=",") %>%
    filter(NAICS %in% NAICScats) %>%
      filter(AREA %in% NYS_county_names)}
      if(i>2014 & i < 2018){labor_df2 = read.table(file=paste(tdir,"/Occup_dat_fold/qcew_annual_",i,".txt",sep=""),
                                       header = T,sep=",") %>%
      filter(NAICS %in% NAICScats) %>%
        filter(AREA %in% NYS_county_names)
      labor_df = rbind(labor_df,labor_df2)}
      if(i==2018){labor_df2 = read.table(file=paste(tdir,"/Occup_dat_fold/qcew_annual_",i,".txt",sep=""),
                                         header = T,sep=",") %>%
        filter(NAICS %in% NAICScats) %>%
        filter(AREA %in% NYS_county_names) %>%
        select(-c(NAICS_LEVEL))
      Labor_data = rbind(labor_df,labor_df2) %>%
        mutate(County = str_remove(AREA," County"),
               year = as.numeric(tolower(YEAR))) %>%
        select(c(County,TITLE,YEAR,ESTAB,AVGEMP,TOTWAGE,ANNAVGSAL))
      remove(labor_df,labor_df2)}}}
if(file.exists(paste(getwd(),"/qcew",sep=""))==T){for(i in 2014:2018){
  if(i==2014){labor_df = read.table(file=paste(getwd(),"/qcew/qcew_annual_",i,".txt",sep=""),
                                    header = T,sep=",") %>%
    filter(NAICS %in% NAICScats) %>%
    filter(AREA %in% NYS_county_names)}
  if(i>2014 & i<2018){labor_df2 = read.table(file=paste(getwd(),"/qcew/qcew_annual_",i,".txt",sep=""),
                                    header = T,sep=",") %>%
    filter(NAICS %in% NAICScats) %>%
    filter(AREA %in% NYS_county_names)
  labor_df = rbind(labor_df,labor_df2)}
  if(i==2018){labor_df2 = read.table(file=paste(getwd(),"/qcew/qcew_annual_",i,".txt",sep=""),
                                     header = T,sep=",") %>%
    filter(NAICS %in% NAICScats) %>%
    filter(AREA %in% NYS_county_names) %>%
    select(-c(NAICS_LEVEL))
  Labor_data = rbind(labor_df,labor_df2) %>%
    mutate(County = str_remove(AREA," County"),
           year = as.numeric(tolower(YEAR))) %>%
    select(c(County,TITLE,year,ESTAB,AVGEMP,TOTWAGE,ANNAVGSAL))
  remove(labor_df,labor_df2)}}}

Labor_data_summary = Labor_data %>%
  group_by(County,year) %>%
  summarize(Employee_tots = sum(AVGEMP))

# Join to make big dataset
  Final_tidy_dataset=left_join(Labor_data_summary,Pop_data,by=c("County","year")) %>%
    left_join(.,Lyme_Cases_tidy,by=c("County","year")) %>%
    mutate(Emps_per_pop = (Employee_tots/estimate)*100)
  
  
  