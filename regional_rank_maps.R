# regional_rank_maps.R

#load the packages
library(natserv)
library(rgdal)

setwd("C:/Users/ctracey/Dropbox (PNHP @ WPC)/FactsheetMaps")

# http://services.natureserve.org/docs/schemas/biodiversityDataFlow/1/1/documentation_comprehensiveSpecies_v1.1.xml
# need to set an environment varible for the NatureServe key
Sys.setenv(NATURE_SERVE_KEY="72ddf45a-c751-44c7-9bca-8db3b4513347")

# load the state shapefile
states <- readOGR("C:/Users/ctracey/Dropbox (PNHP @ WPC)/FactsheetMaps","USstates")

# load the species list
species <- read.csv("tracked_species_universal_id_pa_20170530.csv")
species$unkey <- paste("ELEMENT_GLOBAL",species$ELEMENT_GLOBAL_OU_UID,species$ELEMENT.GLOBAL.UNIVERSAL.KEY,sep=".")

#ELEMENT_GLOBAL.2.104470 bald eagle
#ELEMENT_GLOBAL.2.101495 bog turtle
# ELEMENT_GLOBAL.2.136260 actaea rubra

#gets a list of the data for one species
res <- ns_data(uid='ELEMENT_GLOBAL.2.136260')

#show the ELCODE
res[[1]]$speciesCode

#put the rank list into a varible for below
subconstatus_US <- res[[1]]$conservationStatus$natureserve$nationalStatuses$US$subnationalStatuses
subconstatus_CA <- res[[1]]$conservationStatus$natureserve$nationalStatuses$CA$subnationalStatuses
# make a data frame of the states where the species occurs
states_ranks_US <- data.frame(matrix(unlist(subconstatus_US, use.names=T), nrow=length(subconstatus_US), byrow=T))
states_ranks_CA <- data.frame(matrix(unlist(subconstatus_CA, use.names=T), nrow=length(subconstatus_CA), byrow=T))
states_ranks <- rbind(states_ranks_US,states_ranks_CA)
colnames(states_ranks) <- names(subconstatus[[1]]) # assign names to the columns

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
state_status <- merge(states, states_ranks, by.x="STATE_ABBR", by.y="subnationCode")

# plot
plot(state_status, col=(state_status@data$color))


#huc08 <- as.data.frame(res[[1]]$distribution$watersheds)
#write.csv(huc08,"spottedturtle_watersheds.csv")