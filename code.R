setwd("C:/Users/ctracey/Dropbox (PNHP @ WPC)/FactsheetMaps")

library(rgdal)
#library(spatialEco)


# load shapefiles. both should be in the same projection
county <- readOGR("C:/Users/ctracey/Dropbox (PNHP @ WPC)/FactsheetMaps","PaCounty")
eo <-  readOGR("C:/Users/ctracey/Dropbox (PNHP @ WPC)/FactsheetMaps","EO_ptreps") # shuold have standard biotics fields. 

# subset to nonsensitive species for dot based maps
eo.nonsensitive <- eo[eo$SENSITV_SP=="N",]

#gets the year, the as.numeric seems to turn all 'no date' and 'pre-' values to NA
eo.nonsensitive@data$lastyear <- as.numeric(substr(eo.nonsensitive@data$LASTOBS,1,4))
# assign legends and colors for the maps
eo.nonsensitive@data$extant[eo.nonsensitive@data$lastyear<1992] <- "Historic"
eo.nonsensitive@data$extant[eo.nonsensitive@data$lastyear>=1992] <- "Extant"
eo@data$extant[is.na(eo@data$lastyear)] <- "Historic"
# the following lines just add a color based on the above categoires// might be good to change this do at the same time as the above three
eo.nonsensitive@data$color[eo.nonsensitive@data$lastyear<1992] <- "#006837"
eo.nonsensitive@data$color[eo.nonsensitive@data$lastyear>=1992] <- "#78C679"
eo.nonsensitive@data$color[is.na(eo.nonsensitive@data$lastyear)] <- "#006837"

#get a list of snames to run the loop
snames.nonsensitive <- unique(eo.nonsensitive@data$SNAME)

for (i in 1:length(snames.nonsensitive)) {
  tmp <- eo.nonsensitive[eo.nonsensitive$SNAME == as.character(snames.nonsensitive[i]), ] 
  ##png(file=paste("maps/",snames.nonsensitive[i],".png",sep="")) # needs to go before the plot
  plot(county)
  plot(tmp, add=T, pch = 16, cex=0.75, col=eo.nonsensitive@data$color)
  #eo.nonsensitive@data$SNAME <- droplevels(eo.nonsensitive@data$SNAME)
  title(snames.nonsensitive[i], cex=0.5)
  
  type <- eo.nonsensitive@data$extant
  col <- rep(x="#006837")
  col[type="Historic"] <- "#006837"
  
  #legend(x="bottom",legend=unique(eo.nonsensitive@data$extant), col=unique(as.numberic(eo.nonsensitive@data$extant)), cex = 1, bty="n")  #c("#78C679","#006837")
  #writeOGR(tmp, dsn=getwd(), snames[i], driver="ESRI Shapefile", overwrite_layer=TRUE)
  ##dev.off() # turns off the plotter writing to pngs

}


############################
### Still Under Development ###
# this code deals with sensitve speciesto make maps that filled in by the county instead of dots
# subset to sensitive species
eo.sensitive <- eo[eo$SENSITV_SP=="Y",]

#gets the year, the as.numeric seems to turn all 'no date' and 'pre-' values to NA
eo.sensitive@data$lastyear <- as.numeric(substr(eo.sensitive@data$LASTOBS,1,4))
# assign legends and colors for the maps
eo.sensitive@data$extant[eo.sensitive@data$lastyear<1992] <- "Historic"
eo.sensitive@data$extant[eo.sensitive@data$lastyear>=1992] <- "Extant"
eo.sensitive@data$extant[is.na(eo.sensitive@data$lastyear)] <- "Historic"
# the following lines just add a color based on the above categories// might be good to change this do at the same time as the above three
eo.sensitive@data$color[eo.sensitive@data$lastyear<1992] <- "#006837"
eo.sensitive@data$color[eo.sensitive@data$lastyear>=1992] <- "#78C679"
eo.sensitive@data$color[is.na(eo.sensitive@data$lastyear)] <- "#006837"

## https://stackoverflow.com/questions/18216840/joining-point-to-polygon
# intersect points in polygon
countyeo <- over(county,eo.sensitive)
  

