library(here)
library(arcgisbinding)
library(sf)
library(tidyr)
library(ggplot2)

arc.check_product()

# load shapefiles. both should be in the same projection
serverPath <- paste("C:/Users/",Sys.getenv("USERNAME"),"/AppData/Roaming/ESRI/ArcGISPro/Favorites/StateLayers.Default.pgh-gis0.sde/",sep="")
county <- arc.open(paste(serverPath,"StateLayers.DBO.County", sep=""))  
county <- arc.select(county, fields=c("COUNTY_NAM"))
county <- arc.data2sf(county)

# get point reps
eo_ptrep <- arc.open("W:/Heritage/Heritage_Data/Biotics_datasets.gdb/eo_ptreps")  #"W:/Heritage/Heritage_Data/Biotics_datasets.gdb"
eo_ptrep <- arc.select(eo_ptrep, where_clause="EO_TRACK='Y' OR EO_TRACK='W'")
eo_ptrep <- arc.data2sf(eo_ptrep)

# get the current year
currentyear <- as.numeric(format(Sys.Date(), format="%Y"))
extantyear <- currentyear - 25

# assign legends and colors for the maps
eo_ptrep$extant[eo_ptrep$LASTOBS_YR<extantyear] <- "Historic"
eo_ptrep$extant[eo_ptrep$LASTOBS_YR>=extantyear] <- "Extant"
eo_ptrep$extant[is.na(eo_ptrep$LASTOBS_YR)] <- "Historic"
eo_ptrep <- eo_ptrep[order(eo_ptrep$SNAME),] # sort it
eo_ptrep$extant <- as.factor(eo_ptrep$extant)

# get a list of SNAMEs
spList <- unique(eo_ptrep[which(eo_ptrep$SENSITV_SP!="Y"),]$SNAME) # non-senstive species
spListSens <- unique(eo_ptrep[which(eo_ptrep$SENSITV_SP=="Y"),]$SNAME) # non-senstive species

# make the Non-Sensitive series of maps
for(i in 1:length(spList)){
  eo_map <- eo_ptrep[which(eo_ptrep$SNAME==spList[i]),]
  eo_map$SNAME <- gsub("/","-",eo_map$SNAME)
  # map it
  a <- ggplot() +
    geom_sf(data=county, fill=NA) +
    geom_sf(data=eo_map, aes(shape=extant, color=extant), size=2.5) +
    scale_shape_manual(values=c(17, 16), labels=c("<25 years",">25 years"), drop=FALSE)+
    scale_color_manual(values=c('cornflowerblue','darkred'), labels=c("<25 years",">25 years"),drop=FALSE)+
    ggtitle(expr(paste(!!unique(eo_map$SCOMNAME)," (",italic(!!unique(eo_map$SNAME)),")", sep=""))) +
    theme_void() +
    theme(legend.position="bottom") +
    theme(legend.title=element_blank())
  ggsave(filename=paste(here::here("data","stateOcc"),"/","eomap_",gsub(" ","-",unique(eo_map$SNAME)),"_",gsub("-","",Sys.Date()),".png", sep=""), plot=a,
    width = 6,
    height = 4,
    units = c("in"),
    dpi = 150
  )
}

# make the Sensitive series of maps
for(i in 1:length(spListSens)){
  eo_map <- eo_ptrep[which(eo_ptrep$SNAME==spListSens[i]),]
  #map an sensitive species maps for internal use
  a <- ggplot() +
    geom_sf(data=county, fill=NA) +
    geom_sf(data=eo_map, aes(shape=extant, color=extant), size=2.5) +
    scale_shape_manual(values=c(17, 16), labels=c("<25 years",">25 years"), drop=FALSE)+
    scale_color_manual(values=c('cornflowerblue','darkred'), labels=c("<25 years",">25 years"),drop=FALSE)+
    ggtitle(expr(paste(!!unique(eo_map$SCOMNAME)," (",italic(!!unique(eo_map$SNAME)),")", sep=""))) +
    theme_void() +
    theme(legend.position="bottom") +
    theme(legend.title=element_blank())
  ggsave(filename=paste(here::here("data","stateOcc"),"/","eomap_SENSITIVE-internal_",gsub(" ","-",unique(eo_map$SNAME)),"_",gsub("-","",Sys.Date()),".png", sep=""), plot=a,
         width = 6,
         height = 4,
         units = c("in"),
         dpi = 150
  )  
  # map senstive species for external use  
  # do a spatial join with the counties
  eo_mapSens <- st_join(county,eo_map)
  eo_mapSens <- eo_mapSens[which(!is.na(eo_mapSens$extant)),]
  eo_mapSens <- eo_mapSens[c("COUNTY_NAM","extant","SNAME","SCOMNAME")]
  eo_mapSens <- unique(eo_mapSens)
  
  # filter out the historics if the county is extant
  eo_mapSens <- eo_mapSens %>% 
    group_by(COUNTY_NAM) %>% 
    mutate(len=length(unique(extant))==1) %>%
    filter(ifelse(len==FALSE, extant=="Extant", extant %in% c("Extant","Historic")))

  # map it
  a <- ggplot() +
    geom_sf(data=county, fill=NA) +
    geom_sf(data=eo_mapSens, aes(fill=extant)) +
    scale_fill_manual(values=c('cornflowerblue','darkred'), labels=c("<25 years",">25 years"), drop=FALSE)+
    ggtitle(expr(paste(!!unique(eo_mapSens$SCOMNAME)," (",italic(!!unique(eo_mapSens$SNAME)),")", sep=""))) +
    theme_void() +
    theme(legend.position="bottom") +
    theme(legend.title=element_blank())
  ggsave(filename=paste(here::here("data","stateOcc"),"/","eomap_SENSITIVE-external_",gsub(" ","-",unique(eo_mapSens$SNAME)),"_",gsub("-","",Sys.Date()),".png", sep=""), plot=a,
         width = 6,
         height = 4,
         units = c("in"),
         dpi = 150
  )
}

