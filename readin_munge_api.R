##################################################################################################################################
##
##   Script to read in and munge requisite data for 311 analysis 
##   Output is R workspace which can be utilized for any given property
##   Calculation script handles all property specific operations
##
##################################################################################################################################

setwd("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/Major projects/311 data")

library(tidyverse)
library(sf)
library(geojsonio)
library(lubridate)
library(stringr)
library(parallel)
library(httr)
library(readr)

options(scipen=999)

source("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/useful functions/HWE_FUNCTIONS.r")
source("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/useful functions/-.R")
source("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/useful functions/useful minor functions.R")

workspacename <- "Data/full311_testing"

# ## can also make this a list 
# curr.bbl <- "4_2139_1"

## read in pedia map 
pediashape.url <- "http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson"

pedia.map <- geojson_read(as.location(pediashape.url),
                          method="local",
                          what="sp")

pedia.map <- st_as_sf(pedia.map,crs=4326)

########################################################################
## Read in and manipulate 311 data and pluto
## (pluto needed for some operations w/ 311 data)
########################################################################

## Read in current 311 data

## initial column names from API pull and desired column names 
output_311_colnames <- c("Unique.Key"
                         ,"Created.Date"
                         ,"Agency"
                         ,"Complaint.Type"
                         ,"Descriptor"
                         ,"Status"
                         ,"Incident.Zip"
                         ,"Incident.Address"
                         ,"Street.Name"
                         ,"City"
                         ,"Borough"
                         ,"Latitude"
                         ,"Longitude"
                         ,"Location")

init_311_colnames <- gsub("\\.","_"
                          ,tolower(
                            output_311_colnames
                          )
)

boro.vec <- c("MANHATTAN","BRONX","BROOKLYN","QUEENS","STATEN ISLAND")

url_311 <- "https://data.cityofnewyork.us/resource/fhrw-4uyv.csv"

obs.lim <- 50000000
s.query <- paste("?$limit="
                 ,obs.lim
                 ,"&$select="
                 ,paste(init_311_colnames,collapse=",")
                 ,sep=""
)

nyc_311_httr.dl <- GET(paste(url_311
                             # ,"?$limit=10000000"
                             ,s.query
                             ,sep=""
)
)

full.311 <- read_csv(nyc_311_httr.dl$content
                            ,col_types = cols(.default="c")
) %>%
  select_(.dots= init_311_colnames) %>%
  setNames(output_311_colnames) %>%
  filter(!(is.na(Longitude) | is.na(Latitude))) %>%
  mutate(Unique.Key = as.integer(Unique.Key)
         ,Created.Date = parse_date_time(
           gsub("T.*","",Created.Date)
           ,"!Y-!m-!d"
           ,tz="EST"
         )
         ,Latitude = as.numeric(Latitude)
         ,Longitude = as.numeric(Longitude)
         ,Borough = trimws(Borough)
         ,City = trimws(City)
         ,Borough = ifelse(Borough == "Unspecified" & City %in% boro.vec
                           ,City
                           ,Borough)
  )

# object_sizes.fun()
rm(nyc_311_httr.dl)
gc()

tmp.sf <- st_as_sf(full.311 %>% 
                     select(Unique.Key,Longitude,Latitude)
                   ,coords = c("Longitude", "Latitude"), crs = 4326)

tmp.sf <- st_join(tmp.sf
                  ,pedia.map %>%
                    mutate(Borough.pedia = toupper(as.character(borough)))) %>% 
  select(Unique.Key,neighborhood,Borough.pedia)

full.311 <- left_join(full.311
                      ,tmp.sf %>% 
                        select(Unique.Key,neighborhood,Borough.pedia)
                      ,by="Unique.Key") %>% 
  rename(Neighborhood = neighborhood) %>%
  mutate(Borough = ifelse((Borough %in% "Unspecified" | is.na(Borough))
                          ,Borough.pedia
                          ,Borough
                          )) %>%
  select(-Borough.pedia)

full.311 <- as.data.frame(full.311 %>%
                            filter(!is.na(Borough)) %>%
                            select(-geometry))

## Read in pluto
cl <- makeCluster(detectCores()-1,type="FORK")

pluto.files <- list.files("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/Data Sources/nyc_pluto_16v2",
                          pattern=".csv",
                          full.names=T)

pluto.list <- parLapply(cl, pluto.files, function(x)
  read.csv(x,
           stringsAsFactors=F)
  )

stopCluster(cl)

pluto <- bind_rows(pluto.list) %>% 
  select(BBL,Borough,Block,Lot,ZipCode,Address,SplitZone,BldgClass,LandUse,Easements,OwnerType,
         OwnerName,BldgArea,ComArea,ResArea,OfficeArea,RetailArea,GarageArea,StrgeArea,
         FactryArea,OtherArea,AreaSource,NumBldgs,NumFloors,UnitsRes,UnitsTotal,YearBuilt,
         CondoNo,XCoord,YCoord) %>% 
  mutate(BBL= paste(substr(BBL,start=1,stop=1),
                    Block,
                    Lot,
                    sep="_"),
         CondoNo= ifelse(CondoNo==0,
                         0,
                         paste(str_sub(BBL,start=1,end=1),CondoNo,sep="_")),
         BldgClass_1 = str_sub(BldgClass,start=1,end=1)
  )
rm(pluto.list)

## Adding neighborhood to pluto 
tmp.df <- pluto %>% 
  filter(!is.na(XCoord)) %>% 
  select(BBL,XCoord,YCoord)
tmp.df <- st_as_sf(tmp.df, coords = c("XCoord", "YCoord"), crs = 102718) %>% 
  st_transform(4326)

tmp.df <- st_join(tmp.df
                  ,pedia.map)

tmp.df <- as.data.frame(
  bind_cols(tmp.df
            ,as.data.frame(st_coordinates(tmp.df))
  )
  ,stringsAsFactors=F) %>% 
  rename(
    latitude = Y
    ,longitude = X
  )

pluto <- left_join(
  pluto
  ,tmp.df %>% 
    select(BBL,neighborhood,latitude,longitude) %>%
    rename(Neighborhood = neighborhood)
  ,by="BBL"
)

# ## 311 data missing Borough in some instances 
# ## Filling in the NA obs via pluto and neighborhood
# 
# boro_pl.levs <- pluto %>%
#   filter(!duplicated(Borough)) %>%
#   pull(Borough)
# boro_311.levs <- full.311 %>%
#   filter(!duplicated(Borough)) %>%
#   pull(Borough)
# boro_311.levs <- boro_311.levs[!boro_311.levs %in% "Unspecified" & !is.na(boro_311.levs)]
# 
# boro_pl.levs.mat <- cbind(
#   substr(boro_pl.levs,start=1,stop=1)
#   ,substr(boro_pl.levs,start=2,stop=2)
# )
# 
# boro_levs.match_order <- unlist(lapply(1:nrow(boro_pl.levs.mat), function(x){
#   which(
#     grepl(boro_pl.levs.mat[x,1],boro_311.levs,ignore.case=T) &
#       grepl(boro_pl.levs.mat[x,2],boro_311.levs,ignore.case=T)
#   )}
# )
# )
# 
# full.311 <- left_join(
#   full.311
#   ,pluto %>% 
#     filter(!is.na(Neighborhood) & !is.na(Borough) & !duplicated(Neighborhood)) %>% 
#     mutate(Borough = factor(Borough 
#                             ,levels = boro_pl.levs
#                             ,labels = boro_311.levs[boro_levs.match_order])) %>%
#     select(Neighborhood,Borough) %>%
#     rename(Borough.pl = Borough)
#   ,by="Neighborhood")
# 
# full.311 <- full.311 %>% 
#   mutate(
#     Borough.new = ifelse(
#       (Borough == "Unspecified" | is.na(Borough))
#       ,as.character(Borough.pl)
#       ,as.character(Borough)
#     )
#   ) %>% 
#   rename(Borough.orig = Borough
#          ,Borough = Borough.new) %>% 
#   select(-Borough.pl)
# 
# # saveRDS(full.311,"311data_with_neighborhood.rds")

## dataframe identifying all the different complaint types
n_full.311 <- nrow(full.311)
descriptor.levs <- full.311 %>% 
  group_by(Complaint.Type) %>% 
  mutate(complaint_count = n()) %>% 
  group_by(Complaint.Type,Descriptor) %>% 
  mutate(Year=as.numeric(year(Created.Date))
  ) %>%
  summarize(
    Complaint.levs = length(unique(as.character(Complaint.Type)))
    ,Complaint_type.count = mean(complaint_count,na.rm=T)
    ,Descriptor.count = n()
    ,Descriptor.prop_type = n()/Complaint_type.count
    ,Descriptor.prop_all = n()/n_full.311
    ,Agency.levs = length(unique(as.character(Agency)))
    ,Agencies = ifelse(Agency.levs==1
                       ,unique(as.character(Agency))
                       ,paste(unique(as.character(Agency)),collapse="; "))
  ) %>% 
  select(-Complaint.levs
         ,Agency.levs
  ) %>% 
  arrange(desc(
    Descriptor.count
  )
  )


## Complaints currently classifying as trash complaints 
trash.descriptors <- c("E3 Dirty Sidewalk"
                       ,"E3A Dirty Area/Alleyway"
                       ,"E5 Loose Rubbish"
                       ,"E7 Private Carter Spillage"
                       ,"E11 Litter Surveillance"
                       ,"6 Overflowing Litter Baskets"
                       ,"15 Street Cond/Dump-Out/Drop-Off"
                       ,"12 Dead Animals"
                       ,"Litter"
)

## Complaints currently classifying as pest complaints 
pest.descriptors <- as.character(as.data.frame(descriptor.levs %>% 
                                                 filter(grepl("rodent",Complaint.Type,ignore.case=T) | grepl("rodent",Descriptor,ignore.case=T)
                                                        | grepl("vermin",Complaint.Type,ignore.case=T) | grepl("vermin",Descriptor,ignore.case=T)
                                                        | grepl("rat ",Complaint.Type,ignore.case=T) | grepl("rat ",Descriptor,ignore.case=T)
                                                        | grepl("insect",Complaint.Type,ignore.case=T) | grepl("insect",Descriptor,ignore.case=T)
                                                        | grepl("mice",Complaint.Type,ignore.case=T) | grepl("mice",Descriptor,ignore.case=T)
                                                        | grepl("pests",Complaint.Type,ignore.case=T) | grepl("pests",Descriptor,ignore.case=T)
                                                 )
                                               ,stringsAsFactors=F)[,"Descriptor"]
)


########################################################################
## calculate population for geographies of interest 
########################################################################


## Borough population 

tf <- tempfile(tmpdir=td, fileext=".rds")
download.file(
  'https://www.dropbox.com/s/a4q6vsicrpqsryx/neighborhood_boro_population_1970_2016_v2.rds?raw=1',
  destfile=tf,
  method="auto"
)

boro_pops <- readRDS(tf)

unlink(tf)
unlink(td)
rm(tf,td)

boro_pops <- boro_pops %>%
  filter(neighborhood=="Borough" & Year >= 2010 & Source=="NY_opendata_PostCensal") %>%
  mutate(Borough = toupper(Borough)
         ,Borough = ifelse(Borough == "MAHATTAN"
                           ,"MANHATTAN"
                           ,Borough
                           )
         ,Population = Population.smooth
         ) %>%
  select(Year,Population,Borough)

## Neighborhood population 
tf <- tempfile(tmpdir=td, fileext=".rds")
download.file(
  'https://www.dropbox.com/s/bwqsoi8kocbfdya/ACS_Population_CT2010_2010_2015.rds?raw=1',
  destfile=tf,
  method="auto"
)

acs_pops <- readRDS(tf)

unlink(tf)
unlink(td)
rm(tf,td)

acs_pops <- acs_pops %>% 
  mutate(Population = as.numeric(Population)
  )

## Inpute population for 2016 (2015 + average increase of prior two years)
acs_pops.tmp2 <- acs_pops %>% 
  filter(Year > 2012) %>%
  group_by(BoroCT2010) %>% 
  summarize(mean_increase = (Population[Year==max(Year)] - Population[Year==min(Year)])
            /(max(Year)-min(Year))) %>% 
  filter(!duplicated(BoroCT2010)) %>% 
  select(BoroCT2010,mean_increase)

acs_pops.2016 <- left_join(acs_pops %>% 
                             filter(Year==2015)
                           ,acs_pops.tmp2
                           ,by="BoroCT2010"
) %>% mutate(
  mean_increase = ifelse(is.na(mean_increase)
                         ,0
                         ,mean_increase
  )
  ,Population = round(Population + mean_increase)
  ,Year=2016
) %>% 
  select(-mean_increase)

acs_pops <- bind_rows(acs_pops,acs_pops.2016)

## Link table for acs tract and neighborhood 
https://www.dropbox.com/s/iqcwdy6ecg27nd7/neighborhood_key.csv?dl=0


tf <- tempfile(tmpdir=td, fileext=".rds")
download.file(
  'https://www.dropbox.com/s/bwqsoi8kocbfdya/ACS_Population_CT2010_2010_2015.rds?raw=1',
  destfile=tf,
  method="auto"
)

Neighborhood.key <- read.csv(tf,
                             stringsAsFactors=F) %>% 
  mutate(BoroCT2010 = as.character(BoroCT2010)) %>%
  rename(Neighborhood = neighborhood)

unlink(tf)
unlink(td)
rm(tf,td)

# Neighborhood.key <- read.csv("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/Major projects/UWS condo prop/Data/Neighborhood_key.csv",
#                              stringsAsFactors=F) %>% 
#   mutate(BoroCT2010 = as.character(BoroCT2010)) %>%
#   rename(Neighborhood = neighborhood)


nbrhd_pops <- left_join(acs_pops,Neighborhood.key,by="BoroCT2010") %>% 
  group_by(Neighborhood,Year) %>% 
  summarize(
    Population = sum(Population)
    ,Borough = Borough[1]
  ) %>% 
  select(Borough,Neighborhood,Year,Population)

## Forest Park is erroneously classfied as being in Brooklyn
## assigning it to queens manually
restr <- nbrhd_pops[,"Neighborhood"]=="Forest Park"
nbrhd_pops[restr,"Borough"] <- "QUEENS"



###############################################################################################################
## calculate units for geographies of interest  
## note - currently using static 2016 numbers; future iterations should utilize the time series calculations
###############################################################################################################

## Resi Units all nyc
units_nyc <- as.numeric(pluto %>% 
                          # filter(Borough=="BX") %>% 
                          summarize(
                            units=sum(UnitsRes)
                          ))

## Resi Units by borough
units_borough <- pluto %>% 
  group_by(Borough) %>% 
  summarize(
    units=sum(UnitsRes)
  ) %>% 
  mutate(Borough = as.character(factor(Borough
                                       ,levels=c("BK","BX","MN","QN","SI")
                                       ,labels=c("BROOKLYN","BRONX","MANHATTAN","QUEENS","STATEN ISLAND")
  ))
  )

## Resi Units per Neighborhood 
units_nbrhd <- pluto %>% 
  group_by(Neighborhood) %>% 
  summarize(units=sum(UnitsRes))

## remove unnecessary items from workspace prior to saving
rm(
  list=ls()[!ls() %in% 
              c("boro_pops","nbrhd_pops","units_nyc","units_borough","units_nbrhd"
                ,"trash.descriptors","pest.descriptors","descriptor.levs"
                ,"full.311","pluto","pedia.map")
            ]
)
gc()

# setwd("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/Major projects/311 data/Data")
# save.image("311analysis_base_workspace.RData")

# save.image(
#   paste(
#     "311analysis_base_workspace "
#     ,format(
#       Sys.time()
#       ,"%Y%m%d"
#     )
#     ,".RData"
#     ,sep=""
#   )
)