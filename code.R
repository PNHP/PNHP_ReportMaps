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
eo.nonsensitive@data$color[eo.nonsensitive@data$lastyear<1992] <- "#f44d41"
eo.nonsensitive@data$color[eo.nonsensitive@data$lastyear>=1992] <- "#4286f4"
eo.nonsensitive@data$color[is.na(eo.nonsensitive@data$lastyear)] <- "#f44d41"
# the following lines just add a plot symbol based on the above categoires// might be good to change this do at the same time as the above three
eo.nonsensitive@data$pchsym[eo.nonsensitive@data$lastyear<1992] <- 19 #circle
eo.nonsensitive@data$pchsym[eo.nonsensitive@data$lastyear>=1992] <- 17 # triangle
eo.nonsensitive@data$pchsym[is.na(eo.nonsensitive@data$lastyear)] <- 19 # circle

#get a list of snames to run the loop
snames.nonsensitive <- eo.nonsensitive@data[c("SNAME","SCOMNAME")]
snames.nonsensitive <- unique(snames.nonsensitive)

for (i in 1:length(snames.nonsensitive$SNAME)) {
  tmp <- eo.nonsensitive[eo.nonsensitive$SNAME == as.character(snames.nonsensitive$SNAME[i]), ] 
  png(file=paste("maps/",snames.nonsensitive$SNAME[i],".png",sep=""), width=500, height=300) # needs to go before the plot  #
  par(mai=c(0.0,0.25,0.25,0.25))
  names1 <- c(as.character(snames.nonsensitive$SCOMNAME[i]),as.character(snames.nonsensitive$SNAME[i]))
  plot(county, main=bquote( .(names1[1]) ~ "(" * italic(.(names1[2])) * ")"  ))
  plot(tmp, add=T, pch =eo.nonsensitive@data$pchsym, cex=1, col=eo.nonsensitive@data$color)
  legend(x="bottom",inset=-0.03,legend=c("Historic","Extant"), col=c("#f44d41","#4286f4"), pch=c(19,17), cex=1, bty="n", ncol=2)  #c("#78C679","#006837")
  #writeOGR(tmp, dsn=getwd(), snames[i], driver="ESRI Shapefile", overwrite_layer=TRUE)
  dev.off() # turns off the plotter writing to pngs
}


############################
### Still Under Development ###
# this code deals with sensitve species to make maps that filled in by the county instead of dots
# subset to sensitive species
eo.sensitive <- eo[eo$SENSITV_SP=="Y",]

#gets the year, the as.numeric seems to turn all 'no date' and 'pre-' values to NA
eo.sensitive@data$lastyear <- as.numeric(substr(eo.sensitive@data$LASTOBS,1,4))
# assign legends and colors for the maps
eo.sensitive@data$extant[eo.sensitive@data$lastyear<1992] <- "Historic"
eo.sensitive@data$extant[eo.sensitive@data$lastyear>=1992] <- "Extant"
eo.sensitive@data$extant[is.na(eo.sensitive@data$lastyear)] <- "Historic"



#county_eo <- over(county,eo.sensitive)

library(spatialEco)

# intersect points in polygon
pts <- point.in.poly(pts, ply)

# check plot
plot(ply)
plot(a, add=T)

# convert to data frame, keeping your data
pts<- as.data.frame(pts)



## save for later

# the following lines just add a color based on the above categoires// might be good to change this do at the same time as the above three
eo.sensitive@data$color[eo.sensitive@data$lastyear<1992] <- "#f44d41"
eo.sensitive@data$color[eo.sensitive@data$lastyear>=1992] <- "#4286f4"
eo.sensitive@data$color[is.na(eo.sensitive@data$lastyear)] <- "#f44d41"
# the following lines just add a plot symbol based on the above categoires// might be good to change this do at the same time as the above three
eo.sensitive@data$pchsym[eo.sensitive@data$lastyear<1992] <- 19 #circle
eo.sensitive@data$pchsym[eo.sensitive@data$lastyear>=1992] <- 17 # triangle
eo.sensitive@data$pchsym[is.na(eo.sensitive@data$lastyear)] <- 19 # circle

#get a list of snames to run the loop
snames.nonsensitive <- eo.sensitive@data[c("SNAME","SCOMNAME")]
snames.nonsensitive <- unique(snames.nonsensitive)




for (i in 1:length(snames.nonsensitive$SNAME)) {
  tmp <- eo.sensitive[eo.sensitive$SNAME == as.character(snames.nonsensitive$SNAME[i]), ] 
  #png(file=paste("maps/",snames.nonsensitive$SNAME[i],".png",sep=""), width=500, height=300) # needs to go before the plot  #
  par(mai=c(0.0,0.25,0.25,0.25))
  names1 <- c(as.character(snames.nonsensitive$SCOMNAME[i]),as.character(snames.nonsensitive$SNAME[i]))
  plot(county, main=bquote( .(names1[1]) ~ "(" * italic(.(names1[2])) * ")"  ))
  plot(tmp, add=T, pch =eo.sensitive@data$pchsym, cex=1, col=eo.sensitive@data$color)
  legend(x="bottom",inset=-0.03,legend=c("Historic","Extant"), col=c("#f44d41","#4286f4"), pch=c(19,17), cex=1, bty="n", ncol=2)  #c("#78C679","#006837")
  #writeOGR(tmp, dsn=getwd(), snames[i], driver="ESRI Shapefile", overwrite_layer=TRUE)
  #dev.off() # turns off the plotter writing to pngs
}



## https://stackoverflow.com/questions/18216840/joining-point-to-polygon
# intersect points in polygon
countyeo <- over(county,eo.sensitive)
  

