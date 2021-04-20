library(tidyverse)
library(sf)
library(tmap)

Countyshpurl="https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_county_5m.zip"

if(exists("tdir")==F){
  tdir=tempdir()}
if(file.exists(paste(tdir,"/cb_2018_us_county_5m/cb_2018_us_county_5m.shp",sep=""))==FALSE){
  download.file(Countyshpurl, destfile = file.path(tdir,"Boundaries.zip"))
  unzip(file.path(tdir,"Boundaries.zip"),exdir = tdir)}
NYS_counties=read_sf(paste(tdir,"/cb_2018_us_county_5m.shp",sep="")) %>%
  filter(STATEFP == 36) 
NYC_Cnt_Names = c("Queens","Kings","Bronx","Richmond","New York")
NYC_counties = NYS_counties %>%
  filter(NAME %in% NYC_Cnt_Names)
NYS_only_counties = NYS_counties %>%
  filter(!NAME %in% NYC_Cnt_Names)
NYS_county_names = NYS_only_counties %>%
  st_drop_geometry() %>%
  select(NAME)
NYS_county_names = paste(NYS_county_names$NAME," County",sep = "")
