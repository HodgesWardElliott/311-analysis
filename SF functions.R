
########################################################################
## Geospatial functions using SF for 311 analysis 
########################################################################

### Population within a given radius

popradius.fun <- function(lat.bldg = 40.72432
                          ,long.bldg = -73.84847
                          ,acs_tract_pops = acs_tract_pops.tmp
                          ,acs_tract_shapes = acs_tract_shapes.tmp
                          ,mile.radius = .125
                          ,join_by_var = "GISJOIN"){
  
  require(sf)
  require(dplyr)
  
  dat_sim <- data.frame(cbind(long.bldg,lat.bldg)) %>%
    setNames(c("long.bldg","lat.bldg"))
  
  dat_sf <- st_as_sf(dat_sim, coords = c("long.bldg", "lat.bldg"), crs = 4326) %>% 
    st_transform(3035)
  
  ## drawing a circle around coordinates, converting the mile specification to meters
  circle.map <- st_buffer(dat_sf
                          ,dist = 1000/.62137 * mile.radius
  )
  
  curr.shape <- acs_tract_shapes %>% 
    st_transform(3035)
  
  ## which census tracts fall within the circle
  curr.shape <- curr.shape[unlist(st_intersects(circle.map,curr.shape,sparse=T)),]
  
  props <- as.numeric(
    st_area(
      st_intersection(curr.shape,st_union(circle.map))
    )
  )/
    as.numeric(st_area(curr.shape))
  
  curr.shape[,"props"] <- props
  
  if(join_by_var=="GISJOIN"){
    dat <- left_join(
      curr.shape %>% 
        mutate(GISJOIN = as.character(GISJOIN))
      ,acs_tract_pops
      ,by=join_by_var
    )
  } else {
    dat <- left_join(
      curr.shape
      ,acs_tract_pops
      ,by=join_by_var
    )
  }
  
  
  pop <- as.numeric(
    as.data.frame(
      dat %>% 
        summarize(pop = sum(Population * props)) 
    ) %>% 
      select(pop)
  )
  
  return(pop)
}



# Find observations within a given radius ---------------------------------


obs_within_radius.fun <- function(target.lats
                                  ,target.longs
                                  ,df=full.311
                                  ,df_keycol = "BBL"
                                  ,df_latcol = "Latitude"
                                  ,df_longcol = "Longitude"
                                  ,mile.radius = .125
                                  ,return_keys = T
                                  ){
  require(sf)
  require(dplyr)
  
  ptm <- proc.time()
  
  df <- ungroup(df)

  dat_sim <- data.frame(cbind(target.longs,target.lats)) %>%
    setNames(c("target.longs","target.lats"))
  
  dat_sf <- st_as_sf(dat_sim, coords = c("target.longs", "target.lats"), crs = 4326) %>% 
    st_transform(3035)
  
  ## drawing a circle around coordinates, converting the mile specification to meters
  circle.map <- st_union(
    st_buffer(dat_sf
              ,dist = 1000/.62137 * mile.radius
    )
  )
  
  tmp.sf <- st_as_sf(df
                     , coords = c(df_longcol,df_latcol), crs = 4326) %>% 
    st_transform(3035)
  
  derp <- st_intersects(tmp.sf
                        ,circle.map
                        ,sparse=F
  )
  
  df[,paste("radius",mile.radius,sep="")] <- derp[,1]
  
  cat("Time to find observations within radius:\n", (proc.time() - ptm)[3],sep="")
  if(return_keys==T){
    keep.keys <- df[derp[,1],df_keycol]
    return(keep.keys)
  }else{
    return(df)
  }  
}


## Examples - 

# ## generate list of 311 calls made within 1/8 mile radius
# radius_0.125_311 <- obs_within_radius.fun(
#   df = full.311 %>% 
#     filter(Borough %in% "QUEENS" | is.na(Borough)) %>% 
#     select(Unique.Key,Longitude,Latitude)
#   ,target.lats = 40.72432
#   ,target.longs = -73.84847
#   ,df_keycol = "Unique.Key"
#   ,mile.radius = .125
# )
# 
# ## generate list of 311 calls made within 1/4 mile radius
# radius_0.25_311 <- obs_within_radius.fun(
#   df = full.311 %>% 
#     filter(Borough %in% "QUEENS" | is.na(Borough)) %>% 
#     select(Unique.Key,Longitude,Latitude)
#   ,target.lats = 40.72432
#   ,target.longs = -73.84847
#   ,df_keycol = "Unique.Key"
#   ,mile.radius = .25
# )
# 
# ## generate list of BBLs within 1/8 mile radius
# radius_0.125_pluto <- obs_within_radius.fun(
#   df = pluto %>% 
#     select(BBL,longitude,latitude)
#   ,target.lats = 40.72432
#   ,target.longs = -73.84847
#   ,df_latcol = "latitude"
#   ,df_longcol = "longitude"
#   ,df_keycol = "BBL"
#   ,mile.radius = .125
# )
# 
# ## generate list of BBLs within 1/4 mile radius
# radius_0.25_pluto <- obs_within_radius.fun(
#   df = pluto %>% 
#     select(BBL,longitude,latitude)
#   ,target.lats = 40.72432
#   ,target.longs = -73.84847
#   ,df_latcol = "latitude"
#   ,df_longcol = "longitude"
#   ,df_keycol = "BBL"
#   ,mile.radius = .25
# )
# 
# 
# ## create dummy variable columns indicating if calls fall within 1/4 mile radius or 1/8 mile radius
# full.311 <- full.311 %>% 
#   mutate(
#     radius_0.125 = ifelse((Unique.Key %in% radius_0.125_311)
#                           ,T
#                           ,F)
#     ,radius_0.25 = ifelse((Unique.Key %in% radius_0.25_311)
#                           ,T
#                           ,F)
#   )
# 
# ## create dummy variable columns indicating BBLs are within 1/4 mile radius or 1/8 mile radius
# pluto <- pluto %>% 
#   mutate(
#     radius_0.125 = ifelse((BBL %in% radius_0.125_pluto)
#                           ,T
#                           ,F)
#     ,radius_0.25 = ifelse((BBL %in% radius_0.25_pluto)
#                           ,T
#                           ,F)
#   )
# 
# pluto %>% 
#   summarize(
#     tot_units_0.125 = sum(UnitsRes[radius_0.125==T])
#     ,tot_units_0.25 = sum(UnitsRes[radius_0.25==T])
#   )
#
# full.311 %>% 
#   # filter(Borough %in% "QUEENS") %>% 
#   summarize(
#     sum(radius0.25)
#     )


## Not yet in use - 
## Pluto BBL shapefiles 
## Idea would be to apply a similar method to the tax lot polygons as to census tracts
## to get number of residential units within a given radius 

# pluto_shp.bk <- st_read("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/Data Sources/nyc_pluto_16v2/mappluto/bk_mappluto_16v2/BKMapPLUTO.shp")
# pluto_shape.qn <- st_read("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/Data Sources/nyc_pluto_16v2/mappluto/qn_mappluto_16v2/QNMapPLUTO.shp")
# pluto_shape.mn <- st_read("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/Data Sources/nyc_pluto_16v2/mappluto/mn_mappluto_16v2/MNMapPLUTO.shp")
# pluto_shape.bx <- st_read("/Users/billbachrach/Dropbox (hodgeswardelliott)/Data Science/Bill Bachrach/Data Sources/nyc_pluto_16v2/mappluto/bx_mappluto_16v2/BXMapPLUTO.shp")
# 
# 
# pluto_shape <- rbind(pluto_shape.mn
#                      ,pluto_shape.bx
#                      ,pluto_shp.bk
#                      ,pluto_shape.qn
# ) %>% 
#   mutate(BBL= paste(substr(BBL,start=1,stop=1),
#                     Block,
#                     Lot,
#                     sep="_"),
#          CondoNo= ifelse(CondoNo==0,
#                          0,
#                          paste(str_sub(BBL,start=1,end=1),CondoNo,sep="_")),
#          BldgClass_1 = str_sub(BldgClass,start=1,end=1)
#   )


# rm(
#   list=ls()[grepl("pluto_sh",ls())]
# )
# gc()


# prop_obs_within_radius.fun <- function(target.lats
#                                        ,target.longs
#                                        ,df_latcol = "Latitude"
#                                        ,df_longcol = "Longitude"
#                                        ,df=full.311
#                                        ,mile.radius = .125
#                                        ,join_by_var = "GISJOIN"
#                                        ,df_keycol = "BBL"
#                                        ,return_keys = T){
#   require(sf)
#   require(dplyr)
#   
#   ptm <- proc.time()
#   
#   dat_sim <- data.frame(cbind(target.longs,target.lats))
#   
#   dat_sf <- st_as_sf(dat_sim, coords = c("target.longs", "target.lats"), crs = 4326) %>% 
#     st_transform(3035)
#   
#   ## drawing a circle around coordinates, converting the mile specification to meters
#   circle.map <- st_union(
#     st_buffer(dat_sf
#               ,dist = 1000/.62137 * mile.radius
#     )
#   )
#   
#   derp <- st_intersects(st_as_sf(df, crs = 4326) %>% 
#                           st_transform(3035)
#                         ,circle.map
#                         ,sparse=F
#   )
#   
#   df[,
#      paste(
#        "radius"
#        ,mile.radius
#        ,sep=""
#      )
#      ] <- derp[,1]
#   
#   cat("Time to find observations within radius:\n", (proc.time() - ptm)[3],sep="")
#   if(return_keys==T){
#     keep.keys <- df[derp[,1],df_keycol]
#   }else{
#     
#     return(df)
#   }  
# }