setwd("C:/Users/ctracey/Dropbox (PNHP @ WPC)/FactsheetMaps")

library(rgdal)
library(spatialEco)

# load shapefiles. both should be in the same projection
county <- readOGR("C:/Users/ctracey/Dropbox (PNHP @ WPC)/FactsheetMaps","PaCounty")
eo <-  readOGR("C:/Users/ctracey/Dropbox (PNHP @ WPC)/FactsheetMaps","EO_ptreps") # shuold have standard biotics fields. 

# get the current year
currentyear <- as.numeric(format(Sys.Date(), format="%Y"))
extantyear <- currentyear - 25

#gets the year, the as.numeric seems to turn all 'no date' and 'pre-' values to NA
eo@data$lastyear <- as.numeric(substr(eo@data$LASTOBS,1,4))
# assign legends and colors for the maps
eo@data$extant[eo@data$lastyear>extantyear] <- "Historic"
eo@data$extant[eo@data$lastyear<=extantyear] <- "Extant"
eo@data$extant[is.na(eo@data$lastyear)] <- "Historic"
# the following lines just add a color based on the above categoires// might be good to change this do at the same time as the above three
eo@data$color[eo@data$lastyear>extantyear] <- "#f44d41"
eo@data$color[eo@data$lastyear<=extantyear] <- "#4286f4"
eo@data$color[is.na(eo@data$lastyear)] <- "#f44d41"
# the following lines just add a plot symbol based on the above categoires// might be good to change this do at the same time as the above three
eo@data$pchsym[eo@data$lastyear>extantyear] <- 19 #circle
eo@data$pchsym[eo@data$lastyear<=extantyear] <- 17 # triangle
eo@data$pchsym[is.na(eo@data$lastyear)] <- 19 # circle

# subset to nonsensitive species for dot based maps
eo.nonsensitive <- eo[eo$SENSITV_SP=="N",]
#writeOGR(eo.nonsensitive, dsn=getwd(), "eo.nonsensitive", driver="ESRI Shapefile", overwrite_layer=TRUE)
#eo.sensitive <- readOGR(dsn=getwd(), "eo.nonsensitive")

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
  legend(x="bottom",inset=-0.025,legend=c(paste("Records pre-",extantyear,sep=""),paste("Records post-",extantyear,sep="")), col=c("#f44d41","#4286f4"), pch=c(19,17), cex=1, bty="n", ncol=2)  #
  #writeOGR(tmp, dsn=getwd(), snames[i], driver="ESRI Shapefile", overwrite_layer=TRUE)
  dev.off() # turns off the plotter writing to pngs
}


############################
### Still Under Development ###
# this code deals with sensitve species to make maps that filled in by the county instead of dots
# subset to sensitive species
eo.sensitive <- eo[eo$SENSITV_SP=="Y",]
writeOGR(eo.sensitive, dsn=getwd(), "eo.sensitive", driver="ESRI Shapefile", overwrite_layer=TRUE)
eo.sensitive <- readOGR(dsn=getwd(), "eo.sensitive")

eo.sensitive@data$EO_DATA <- NULL
eo.sensitive@data$GEN_DESC <- NULL
eo.sensitive@data$GENERL_COM <- NULL
eo.sensitive@data$EORANKCOM <- NULL
eo.sensitive@data$MGMT_COM <- NULL

library(maptools)
#get only the extant features
eo.sensitive.extant <- eo.sensitive[eo.sensitive$extant=="Extant",]
writeOGR(eo.sensitive.extant, dsn=getwd(), "eo.sensitive.extant", driver="ESRI Shapefile", overwrite_layer=TRUE)
eo.sensitive.extant <- readOGR(dsn=getwd(), "eo.sensitive.extant")

#get a list of snames to run the loop
snames.sensitive.extant <- eo.sensitive.extant@data[c("SNAME","SCOMNAME")]
snames.sensitive.extant <- unique(snames.sensitive.extant)



county_eo.extant <- over(eo.sensitive.extant,county,returnList = TRUE)



county_eo <- over(eo.sensitive,county,returnList = TRUE)


num.county_eo <- sapply(county_eo,length)
county_eo2 <- spCbind(eo.sensitive,num.county_eo)

library(spatialEco)
# intersect points in polygon
county_eo <- point.in.poly(eo.sensitive,county)

# check plot
plot(ply)
plot(a, add=T)

# convert to data frame, keeping your data
pts<- as.data.frame(pts)






################

#get a list of snames to run the loop
snames.sensitive <- eo.sensitive@data[c("SNAME","SCOMNAME")]
snames.sensitive <- unique(snames.sensitive)




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
  

