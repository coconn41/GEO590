library(tidyverse)
library(tabulizer)

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
remove(df,df2,df3,final,final2,i,tdir,url)

