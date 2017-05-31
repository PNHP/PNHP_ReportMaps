# fixed to prev version

#load the packages
library(natserv)
library(rgdal)

setwd("C:/Users/ctracey/Dropbox (PNHP @ WPC)/FactsheetMaps")

# http://services.natureserve.org/docs/schemas/biodiversityDataFlow/1/1/documentation_comprehensiveSpecies_v1.1.xml
# need to set an environment varible for the NatureServe key
Sys.setenv(NATURE_SERVE_KEY="72ddf45a-c751-44c7-9bca-8db3b4513347")

# load the state shapefile
states <- readOGR("C:/Users/ctracey/Dropbox (PNHP @ WPC)/FactsheetMaps","subnational_boundaries")
#states_df <- fortify(states,region = "OBJECTID")
states_center <- readOGR("C:/Users/ctracey/Dropbox (PNHP @ WPC)/FactsheetMaps","subnational_boundaries_centroid")

# load the species list
species <- read.csv("tracked_species_universal_id_pa_20170530.csv")
# build th UID
species$UID <- paste("ELEMENT_GLOBAL",species$ELEMENT_GLOBAL_OU_UID,species$ELEMENT.GLOBAL.UNIVERSAL.KEY,sep=".")

# test of the for loop
#get a list of snames to run the loop
snames <-species[c("ELCODE","SNAME","UID")]
snames <- snames[substr(snames$ELCODE,1,1)=="P",]
snames <- droplevels(snames)
snames <- unique(snames)
snames <- snames[order(snames$SNAME),] 

for (i in 1:length(snames$UID)) {
  res <- list()
  delayedAssign("do.next", {next})
  tryCatch(res <- ns_data(uid=snames$UID[i]), finally=print(snames$SNAME[i]), error=function(e) force(do.next))
  #res[[1]]$speciesCode  #show the ELCODE
  #put the rank list into a varible for below
  subconstatus_US <- res[[1]]$conservationStatus$natureserve$nationalStatuses$US$subnationalStatuses
  if (length(subconstatus_US)==0) stop("no distribution info", call=F)
  states_ranks_US <- data.frame(matrix(unlist(subconstatus_US, use.names=T), nrow=length(subconstatus_US), byrow=T))
  subconstatus_CA <- res[[1]]$conservationStatus$natureserve$nationalStatuses$CA$subnationalStatuses
  if (length(subconstatus_CA)==0) {   #stop("no CA distribution info", call=F)
    states_ranks <- states_ranks_US 
  } else {
    states_ranks_CA <- data.frame(matrix(unlist(subconstatus_CA, use.names=T), nrow=length(subconstatus_CA), byrow=T))
    states_ranks <- rbind(states_ranks_US,states_ranks_CA)  
  }
  colnames(states_ranks) <- names(subconstatus_US[[1]]) # assign names to the columns 
  # assign colors to the ranks
  states_ranks$color[states_ranks$roundedRank=="SX"]  <- "#003593"
  states_ranks$color[states_ranks$roundedRank=="SH"]  <- "#6F97F3"
  states_ranks$color[states_ranks$roundedRank=="S1"]  <- "#D30200"
  states_ranks$color[states_ranks$roundedRank=="S2"]  <- "#FF914B"
  states_ranks$color[states_ranks$roundedRank=="S3"]  <- "#FFFF2D"
  states_ranks$color[states_ranks$roundedRank=="S4"]  <- "#64FE3A"
  states_ranks$color[states_ranks$roundedRank=="S5"]  <- "#039B2E"
  states_ranks$color[states_ranks$roundedRank=="SNR"] <- "#9B656C"
  states_ranks$color[states_ranks$roundedRank=="SU"]  <- "#9B656C"
  states_ranks$color[states_ranks$roundedRank=="SNA"] <- "#FFC8FF"
  # join to polygons
  state_status <- merge(states, states_ranks, by.x="subnation", by.y="subnationCode")
  # plot
  png(file=paste("mapsRegion/",snames$SNAME[i],".png",sep=""), width=2100, height=1800,res=300)
  plot(state_status, col=(state_status@data$color))  
  text(states_center, labels=states_center$subnation,col="black", cex=0.5, adj=c(0.5, NA))
  dev.off() # turns off the plotter writing to pngs
  }

# https://www.arcgis.com/home/item.html?id=46d9f3f43c664256a96a9c8552ff7c5a

