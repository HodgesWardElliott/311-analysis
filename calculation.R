
## property specific calculations for 311 analysis 

setwd("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/useful functions/Major projects/311 data")

library(tidyverse)
library(sf)
library(geojsonio)
library(lubridate)
library(stringr)
library(parallel)
library(data.table)

options(scipen=999)

source("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/useful functions/HWE_FUNCTIONS.r")
source("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/useful functions/-.R")
source("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/useful functions/useful minor functions.R")
source("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/Major projects/311 data/Code/SF functions.R")

workspacename <- "Data/full311_testing"

## load the base workspace (includes munged 311 data linked w/ Neighborhood, pluto, populations and descriptors for 311 call types)
# load("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/Major projects/311 data/Data/311analysis_base_workspace.RData")

pluto <- pluto %>% 
rename(latitude = lat
       ,longitude = lon)

## can also make this a list 
# curr.bbl <- "1_2054_92"
curr.bbl <- c("4_5185_17","4_5186_29")

# curr.boro <- as.character(pluto %>% 
#   filter(BBL %in% curr.bbl) %>% 
#   filter(!duplicated(Borough)) %>% 
#   select(Borough))

curr.boro <-"QUEENS"

pluto.targets <- pluto %>% 
  filter(!is.na(latitude)) %>% 
  filter(BBL %in% curr.bbl) %>% 
  select(latitude,longitude)


## put dummy variables for given radius around properties into dataframes 
## generate list of 311 calls made within 1/8 mile radius
radius_0.125_311 <- obs_within_radius.fun(
  df = full.311 %>% 
    filter(Borough %in% curr.boro | is.na(Borough)) %>% 
    select(Unique.Key,Longitude,Latitude)
  ,target.lats = pluto.targets[,"latitude"]
  ,target.longs = pluto.targets[,"longitude"]
  ,df_keycol = "Unique.Key"
  ,mile.radius = .125
  ,return_keys=T
)

## generate list of 311 calls made within 1/4 mile radius
radius_0.25_311 <- obs_within_radius.fun(
  df = full.311 %>% 
    filter(Borough %in% curr.boro | is.na(Borough)) %>% 
    select(Unique.Key,Longitude,Latitude)
  ,target.lats = pluto.targets[,"latitude"]
  ,target.longs = pluto.targets[,"longitude"]
  ,df_keycol = "Unique.Key"
  ,mile.radius = .25
  ,return_keys=T
)

## generate list of BBLs within 1/8 mile radius
radius_0.125_pluto <- obs_within_radius.fun(
  df = pluto %>% 
    filter(!is.na(longitude)) %>% 
    select(BBL,longitude,latitude)
  ,target.lats = pluto.targets[,"latitude"]
  ,target.longs = pluto.targets[,"longitude"]
  ,df_latcol = "latitude"
  ,df_longcol = "longitude"
  ,df_keycol = "BBL"
  ,mile.radius = .125
  ,return_keys=T
) %>%
  pull(BBL)

## generate list of BBLs within 1/4 mile radius
radius_0.25_pluto <- obs_within_radius.fun(
  df = pluto %>% 
    filter(!is.na(longitude)) %>% 
    select(BBL,longitude,latitude)
  ,target.lats = pluto.targets[,"latitude"]
  ,target.longs = pluto.targets[,"longitude"]
  ,df_latcol = "latitude"
  ,df_longcol = "longitude"
  ,df_keycol = "BBL"
  ,mile.radius = .25
  ,return_keys=T
) %>%
  pull(BBL)


## create dummy variable columns indicating if calls fall within 1/4 mile radius or 1/8 mile radius
full.311 <- full.311 %>% 
  mutate(
    radius_0.125 = ifelse((Unique.Key %in% radius_0.125_311)
                          ,T
                          ,F)
    ,radius_0.25 = ifelse((Unique.Key %in% radius_0.25_311)
                          ,T
                          ,F)
  )

## create dummy variable columns indicating BBLs are within 1/4 mile radius or 1/8 mile radius
pluto <- pluto %>% 
  mutate(
    radius_0.125 = ifelse((BBL %in% radius_0.125_pluto)
                          ,T
                          ,F)
    ,radius_0.25 = ifelse((BBL %in% radius_0.25_pluto)
                          ,T
                          ,F)
  )

pluto %>%
  # filter(BBL %in% radius_0.125_pluto)
filter(BBL == "4_5137_67")
class(radius_0.125_pluto)


pluto %>%
  summarize(
    radius_0.125_units = sum(UnitsRes[radius_0.125==T])
    ,radius_0.125 = sum(BBL %in% radius_0.125_pluto)
    ,radius_0.25_units = sum(UnitsRes[radius_0.25==T])
    ,radius_0.25 = sum(radius_0.25)
  )



#####################################################################################################################
## First calculating number of units and population for given radiuses around the properties of interest 
#####################################################################################################################

## population 

## Downloading ACS population data and shapefiles from dropbox link
tf <- tempfile()
download.file(
  "https://www.dropbox.com/s/iw5itfwt5pey7pu/acs.list.rds?raw=1"
  ,destfile=tf
  ,method="auto"
)

acs_tract_pops.tmp <- readRDS(tf)

unlink(tf)
rm(tf)

tf <- tempfile()
download.file(
  # 'https://www.dropbox.com/s/sbpc445yuvo1w8q/ct_shape_2011_2015.list.rds?raw=1'
  "https://www.dropbox.com/s/lyrv3r2wbexoqeq/ct_shape_2011_2016.list.rds?raw=1"
  ,destfile=tf
  ,method="auto"
)

acs_tract_shapes.tmp <- readRDS(tf)

unlink(tf)


## first put in the manual calculations for units and population 
## run script containing SF functions to get popradius.fun

## 1/8 mile population 
pop_0.125_mi <- unlist(
  lapply(1:length(acs_tract_pops.tmp), function(x)
    popradius.fun(lat.bldg = pluto.targets[,"latitude"]
                  ,long.bldg = pluto.targets[,"longitude"]
                  ,acs_tract_pops = acs_tract_pops.tmp[[x]]
                  ,acs_tract_shapes = acs_tract_shapes.tmp[[x]]
                  ,mile.radius=.125
    )
  )
)


## Impute values for 2010 and 2017 by finding the mean yearly change
pop_0.125_mi <- c(pop_0.125_mi[1] - mean(
  unlist(
    lapply(2:length(pop_0.125_mi), function(x)
      pop_0.125_mi[x]-pop_0.125_mi[x-1]
    )
  )
)
,pop_0.125_mi
,pop_0.125_mi[length(pop_0.125_mi)] + mean(
  unlist(
    lapply(2:length(pop_0.125_mi), function(x)
      pop_0.125_mi[x]-pop_0.125_mi[x-1]
    )
  )
)
)

pop_0.125_mi <- cbind(
    2010:2017
    ,pop_0.125_mi
  ) %>%
  as.data.frame() %>%
  setNames(c("Year","Population"))


## 1/4 mile population 
pop_0.25_mi <- unlist(
  lapply(1:length(acs_tract_pops.tmp), function(x)
    popradius.fun(lat.bldg = pluto.targets[,"latitude"]
                  ,long.bldg = pluto.targets[,"longitude"]
                  ,acs_tract_pops = acs_tract_pops.tmp[[x]]
                  ,acs_tract_shapes = acs_tract_shapes.tmp[[x]]
                  ,mile.radius=.25
    )
  )
)


## Impute values for 2010 and 2017 by finding the mean yearly change
pop_0.25_mi <- c(pop_0.25_mi[1] - mean(
  unlist(
    lapply(2:length(pop_0.25_mi), function(x)
      pop_0.25_mi[x]-pop_0.25_mi[x-1]
    )
  )
)
,pop_0.25_mi
,pop_0.25_mi[length(pop_0.25_mi)] + mean(
  unlist(
    lapply(2:length(pop_0.25_mi), function(x)
      pop_0.25_mi[x]-pop_0.25_mi[x-1]
    )
  )
)
)

pop_0.25_mi <- cbind(
  2010:2017
  ,pop_0.25_mi
) %>%
  as.data.frame() %>%
  setNames(c("Year","Population"))


## units within radius 
units_0.125mi <- as.numeric(pluto %>% 
                              filter(radius_0.125) %>% 
                              summarize(
                                units = sum(UnitsRes)
                              )
)

units_0.25mi <- as.numeric(pluto %>% 
                             filter(radius_0.25) %>% 
                             summarize(
                               units = sum(UnitsRes)
                             )
)



####################################################################################
## Actual calculations, starting with smallest area 
####################################################################################


## tabulating for different areas
## smallest to largest

## 1/8 mile by year 
radius_0.125_year.levs <- full.311 %>% 
  mutate(Year = year(Created.Date)) %>% 
  filter(radius_0.125==T) %>%
  group_by(Year) %>% 
  mutate(complaint.count = n()) %>%
  group_by(Year,Complaint.Type,Descriptor) %>% 
  summarize(
    Descriptor.count = n()
    ,Descriptor.prop = n()/mean(complaint.count,na.rm=T)
    ,Complaint_count = mean(complaint.count,na.rm=T)
  ) %>% 
  arrange(desc(
    Descriptor.count
  )
  ) %>% 
  mutate(units = units_0.125mi
         ,area="radius_0.125mi"
         # ,Population = pop_0.125_mi
  )

radius_0.125_boro <- as.character(
  pluto %>% 
    filter(radius_0.125==T) %>% 
    select(Borough) %>% 
    filter(!duplicated(Borough))
)

if(length(radius_0.125_boro)==1){
  radius_0.125_year.levs <- radius_0.125_year.levs %>% 
    mutate(Borough=radius_0.125_boro)
} else{
  cat("More than one Borough in 500 foot radius, enter manually")
}


radius_0.125_year.levs <- radius_0.125_year.levs %>% 
  group_by(Year) %>% 
  arrange(desc(Descriptor.count)) %>% 
  mutate(rank = 1:n()) %>% 
  select(area,rank,everything()
         # ,-complaint_type
  ) %>% 
  arrange(Year,rank)

radius_0.125_year.levs <- left_join(
  radius_0.125_year.levs
  ,pop_0.125_mi
  ,by="Year"
)

radius_0.125_year.out <- radius_0.125_year.levs %>%
  summarize(
    Area = "Radius_0.125mi"
    ,units = mean(units)
    ,Population = mean(Population)
    
    ,trashcall_count = sum(Descriptor.count[Descriptor %in% trash.descriptors])
    ,heatcall_count = sum(Descriptor.count[grepl("heat",Complaint.Type,ignore.case=T) | grepl("heat",Descriptor,ignore.case=T)]) 
    ,noisecall_count = sum(Descriptor.count[grepl("noise",Complaint.Type,ignore.case=T) | grepl("noise",Descriptor,ignore.case=T)]) 
    ,grafcall_count = sum(Descriptor.count[grepl("Graffiti",Complaint.Type,ignore.case=T) | grepl("Graffiti",Descriptor,ignore.case=T)]) 
    ,pestcall_count = sum(Descriptor.count[Descriptor %in% pest.descriptors])
    
    ,trashcall_unit.ratio = trashcall_count / units
    ,trashcall_pop.ratio = trashcall_count / Population
    ,heatcall_unit.ratio = heatcall_count / units
    ,heatcall_pop.ratio = heatcall_count / Population
    ,noisecall_unit.ratio = noisecall_count / units
    ,noisecall_pop.ratio = noisecall_count / Population
    ,grafcall_unit.ratio = grafcall_count / units
    ,grafcall_pop.ratio = grafcall_count / Population
    ,pestcall_unit.ratio = pestcall_count/units
    ,pestcall_pop.ratio = pestcall_count/Population
    ,AreaType = "Radius"
  )


## 1/4 mile by year 
radius_0.25_year.levs <- full.311 %>% 
  mutate(Year = year(Created.Date)) %>% 
  filter(radius_0.25==T) %>%
  group_by(Year) %>% 
  mutate(complaint.count = n()) %>%
  group_by(Year,Complaint.Type,Descriptor) %>% 
  summarize(
    Descriptor.count = n()
    ,Descriptor.prop = n()/mean(complaint.count,na.rm=T)
    ,Complaint_count = mean(complaint.count,na.rm=T)
  ) %>% 
  arrange(desc(
    Descriptor.count
  )
  ) %>% 
  mutate(units = units_0.25mi
         ,area="radius_0.25mi"
         # ,Population= pop_0.25_mi
  )

radius_0.25_boro <- as.character(
  pluto %>% 
    filter(radius_0.25==T) %>% 
    select(Borough) %>% 
    filter(!duplicated(Borough))
)

if(length(radius_0.25_boro)==1){
  radius_0.25_year.levs <- radius_0.25_year.levs %>% 
    mutate(Borough=radius_0.25_boro)
} else{
  cat("More than one Borough in 660 foot radius, enter manually")
}

radius_0.25_year.levs <- radius_0.25_year.levs %>% 
  group_by(Year) %>% 
  arrange(desc(Descriptor.count)) %>% 
  mutate(rank = 1:n()) %>% 
  select(area,rank,everything()
         # ,-complaint_type
  ) %>% 
  arrange(Year,rank)


radius_0.25_year.levs <- left_join(
  radius_0.25_year.levs
  ,pop_0.25_mi
  ,by="Year"
)

radius_0.25_year.out <- radius_0.25_year.levs %>%
  summarize(
    Area = "Radius_0.25mi"
    ,units = mean(units)
    ,Population = mean(Population)
    
    ,trashcall_count = sum(Descriptor.count[Descriptor %in% trash.descriptors])
    ,heatcall_count = sum(Descriptor.count[grepl("heat",Complaint.Type,ignore.case=T) | grepl("heat",Descriptor,ignore.case=T)]) 
    ,noisecall_count = sum(Descriptor.count[grepl("noise",Complaint.Type,ignore.case=T) | grepl("noise",Descriptor,ignore.case=T)]) 
    ,grafcall_count = sum(Descriptor.count[grepl("Graffiti",Complaint.Type,ignore.case=T) | grepl("Graffiti",Descriptor,ignore.case=T)]) 
    ,pestcall_count = sum(Descriptor.count[Descriptor %in% pest.descriptors])
    
    ,trashcall_unit.ratio = trashcall_count / units
    ,trashcall_pop.ratio = trashcall_count / Population
    ,heatcall_unit.ratio = heatcall_count / units
    ,heatcall_pop.ratio = heatcall_count / Population
    ,noisecall_unit.ratio = noisecall_count / units
    ,noisecall_pop.ratio = noisecall_count / Population
    ,grafcall_unit.ratio = grafcall_count / units
    ,grafcall_pop.ratio = grafcall_count / Population
    ,pestcall_unit.ratio = pestcall_count/units
    ,pestcall_pop.ratio = pestcall_count/Population
    ,AreaType = "Radius"
  )

## Neighborhood 
nbrhd_year.levs <- full.311 %>% 
  mutate(Year = year(Created.Date)) %>% 
  group_by(Neighborhood,Year) %>% 
  mutate(complaint_Neighborhood.count = n()) %>%
  group_by(Neighborhood,Year,Complaint.Type,Descriptor) %>% 
  summarize(
    Descriptor.count = n()
    ,Descriptor.prop_nbrhd = n()/mean(complaint_Neighborhood.count,na.rm=T)
    ,Complaint_count = mean(complaint_Neighborhood.count,na.rm=T)
  ) %>% 
  arrange(desc(
    Neighborhood
    ,Descriptor.count
  )
  )

nbrhd_year.levs <- left_join(nbrhd_year.levs
                             ,units_nbrhd,by="Neighborhood")
nbrhd_year.levs <- left_join(nbrhd_year.levs
                             ,nbrhd_pops %>% 
                               # filter(Year==2015) %>% 
                               select(Neighborhood,Population,Year)
                             ,by=c("Neighborhood","Year"))

nbrhd_year.levs <- left_join(nbrhd_year.levs
                             ,pluto %>% 
                               filter(!is.na(Neighborhood) & !is.na(Borough) & !duplicated(Neighborhood)) %>% 
                               select(Neighborhood,Borough)
                             ,by="Neighborhood"
) %>% 
  select(Neighborhood,Borough,everything())

nbrhd_year.levs <- nbrhd_year.levs %>% 
  group_by(Neighborhood,Year) %>% 
  arrange(desc(Descriptor.count)) %>% 
  mutate(rank = 1:n()) %>% 
  select(Neighborhood,rank,everything()
         # ,-complaint_type
  ) %>% 
  arrange(Neighborhood,Year,rank)


# nbrhd_year.out.hold <- nbrhd_year.out
nbrhd_year.out <- nbrhd_year.levs %>%
  summarize(
    units = mean(units)
    ,Population = mean(Population)
    
    ,trashcall_count = sum(Descriptor.count[Descriptor %in% trash.descriptors])
    ,heatcall_count = sum(Descriptor.count[grepl("heat",Complaint.Type,ignore.case=T) | grepl("heat",Descriptor,ignore.case=T)]) 
    ,noisecall_count = sum(Descriptor.count[grepl("noise",Complaint.Type,ignore.case=T) | grepl("noise",Descriptor,ignore.case=T)]) 
    ,grafcall_count = sum(Descriptor.count[grepl("Graffiti",Complaint.Type,ignore.case=T) | grepl("Graffiti",Descriptor,ignore.case=T)]) 
    ,pestcall_count = sum(Descriptor.count[Descriptor %in% pest.descriptors])
    
    ,trashcall_unit.ratio = trashcall_count / units
    ,trashcall_pop.ratio = trashcall_count / Population
    ,heatcall_unit.ratio = heatcall_count / units
    ,heatcall_pop.ratio = heatcall_count / Population
    ,noisecall_unit.ratio = noisecall_count / units
    ,noisecall_pop.ratio = noisecall_count / Population
    ,grafcall_unit.ratio = grafcall_count / units
    ,grafcall_pop.ratio = grafcall_count / Population
    ,pestcall_unit.ratio = pestcall_count/units
    ,pestcall_pop.ratio = pestcall_count/Population
    ,AreaType = "Neighborhood"
  ) %>% 
  rename(Area = Neighborhood)



## Borough
boro.levs <- full.311 %>% 
  group_by(Borough) %>% 
  mutate(complaint_boro.count = n()) %>%
  group_by(Borough,Complaint.Type,Descriptor) %>% 
  summarize(
    Descriptor.count = n()
    ,Descriptor.prop_boro = n()/mean(complaint_boro.count,na.rm=T)
    ,Complaint_count = mean(complaint_boro.count,na.rm=T)
  ) %>% 
  arrange(desc(
    Borough
    ,Descriptor.count
  )
  )


boro.levs <- boro.levs %>% 
  group_by(Borough) %>% 
  arrange(desc(Descriptor.count)) %>% 
  mutate(rank = 1:n()) %>% 
  select(Borough,rank,everything()
         # ,-complaint_type
  ) %>% 
  arrange(Borough,rank)



boro_year.levs <- full.311 %>% 
  filter(!is.na(Borough)) %>% 
  mutate(Year = year(Created.Date)) %>% 
  group_by(Borough,Year) %>% 
  mutate(complaint_boro.count = n()) %>%
  group_by(Borough,Year,Complaint.Type,Descriptor) %>% 
  summarize(
    Descriptor.count = n()
    ,Descriptor.prop_boro = n()/mean(complaint_boro.count,na.rm=T)
    ,Complaint_count = mean(complaint_boro.count,na.rm=T)
  ) %>% 
  arrange(desc(
    Borough
    ,Year
    ,Descriptor.count
  )
  )


boro_year.levs <- boro_year.levs %>% 
  group_by(Borough,Year) %>% 
  arrange(desc(Descriptor.count)) %>% 
  mutate(rank = 1:n()) %>% 
  select(Borough,Year,rank,everything()
         # ,-complaint_type
  ) %>% 
  arrange(Borough,Year,rank)

boro_year.levs <- left_join(boro_year.levs,units_borough,by="Borough")
boro_year.levs <- left_join(boro_year.levs,boro_pops,by=c("Borough","Year"))

# boro_year.out.hold <- boro_year.out
boro_year.out <- boro_year.levs %>%
  summarize(
    units = mean(units)
    ,Population = mean(Population)
    
    ,trashcall_count = sum(Descriptor.count[Descriptor %in% trash.descriptors])
    ,heatcall_count = sum(Descriptor.count[grepl("heat",Complaint.Type,ignore.case=T) | grepl("heat",Descriptor,ignore.case=T)]) 
    ,noisecall_count = sum(Descriptor.count[grepl("noise",Complaint.Type,ignore.case=T) | grepl("noise",Descriptor,ignore.case=T)]) 
    ,grafcall_count = sum(Descriptor.count[grepl("Graffiti",Complaint.Type,ignore.case=T) | grepl("Graffiti",Descriptor,ignore.case=T)]) 
    ,pestcall_count = sum(Descriptor.count[Descriptor %in% pest.descriptors])
    
    ,trashcall_unit.ratio = trashcall_count / units
    ,trashcall_pop.ratio = trashcall_count / Population
    ,heatcall_unit.ratio = heatcall_count / units
    ,heatcall_pop.ratio = heatcall_count / Population
    ,noisecall_unit.ratio = noisecall_count / units
    ,noisecall_pop.ratio = noisecall_count / Population
    ,grafcall_unit.ratio = grafcall_count / units
    ,grafcall_pop.ratio = grafcall_count / Population
    ,pestcall_unit.ratio = pestcall_count/units
    ,pestcall_pop.ratio = pestcall_count/Population
    ,AreaType = "Borough"
  ) %>% 
  rename(Area = Borough)


## nyc year
nyc_year.levs <- full.311 %>% 
  mutate(Year = year(Created.Date)) %>% 
  group_by(Year) %>% 
  mutate(complaint.count = n()
  ) %>%
  group_by(Year,Complaint.Type,Descriptor) %>% 
  summarize(
    Descriptor.count = n()
    ,Descriptor.prop = n()/mean(complaint.count,na.rm=T)
    ,Complaint_count = mean(complaint.count,na.rm=T)
  ) %>% 
  arrange(desc(
    Descriptor.count
  )
  ) %>% 
  mutate(units=units_nyc)

nyc_year.levs <- nyc_year.levs %>% 
  group_by(Year) %>% 
  arrange(desc(Descriptor.count)) %>% 
  mutate(rank = 1:n()) %>% 
  select(rank,everything()
         # ,-complaint_type
  ) %>% 
  arrange(Year,rank)

nyc_year.levs <- left_join(nyc_year.levs
                           ,boro_pops %>% 
                             group_by(Year) %>% 
                             summarize(Population = sum(Population))
                           ,by="Year"
)

# nyc_year.out.hold <- nyc_year.out
nyc_year.out <- nyc_year.levs %>%
  summarize(
    Area="NYC"
    ,units = mean(units)
    ,Population = mean(Population)
    
    ,trashcall_count = sum(Descriptor.count[Descriptor %in% trash.descriptors])
    ,heatcall_count = sum(Descriptor.count[grepl("heat",Complaint.Type,ignore.case=T) | grepl("heat",Descriptor,ignore.case=T)]) 
    ,noisecall_count = sum(Descriptor.count[grepl("noise",Complaint.Type,ignore.case=T) | grepl("noise",Descriptor,ignore.case=T)]) 
    ,grafcall_count = sum(Descriptor.count[grepl("Graffiti",Complaint.Type,ignore.case=T) | grepl("Graffiti",Descriptor,ignore.case=T)]) 
    ,pestcall_count = sum(Descriptor.count[Descriptor %in% pest.descriptors])
    
    ,trashcall_unit.ratio = trashcall_count / units
    ,trashcall_pop.ratio = trashcall_count / Population
    ,heatcall_unit.ratio = heatcall_count / units
    ,heatcall_pop.ratio = heatcall_count / Population
    ,noisecall_unit.ratio = noisecall_count / units
    ,noisecall_pop.ratio = noisecall_count / Population
    ,grafcall_unit.ratio = grafcall_count / units
    ,grafcall_pop.ratio = grafcall_count / Population
    ,pestcall_unit.ratio = pestcall_count/units
    ,pestcall_pop.ratio = pestcall_count/Population
    ,AreaType = "City"
  )


## combine 
year.out <- bind_rows(
  nyc_year.out
  ,boro_year.out
  ,nbrhd_year.out
  ,radius_0.25_year.out
  ,radius_0.125_year.out
) %>% 
  filter(!Year %in% 2018 & !is.na(Area))

write.csv(
  year.out
  ,file="/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/ad_hoc/Flushing - Ash and Beech/data/311_stats_48StNic.csv"
  ,na=""
  ,row.names=F
)