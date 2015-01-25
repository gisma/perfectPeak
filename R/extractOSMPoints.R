#'@title Extract peak position and altitude from Harry's Bergliste
#'@description  The current Kmz file of Harry's peak list is downloaded and will be cleaned to derive coordinates altitude and name of all available peaks within a region of interest 

#'@details 
#'\tabular{ll}{
#'Package: \tab Rpeak\cr
#'Type: \tab Package\cr
#'Version: \tab 0.2\cr
#'License: \tab GPL (>= 2)\cr
#'LazyLoad: \tab yes\cr
#'}

#'@name extractOSMPoints
#'@aliases extractOSMPoints

#'@usage extractOSMPoints(dem,current.proj4)
#'@author Chris Reudenbach 
#'@references \url{http://moc.environmentalinformatics-marburg.de/doku.php?id=courses:msc:advanced-gis:description}
#' 
#'@param dem is a Digital Elevation Model with geographic coordinates  
#'@param target.projection is a valid proj.4 string containing the correct target crs  
#'
#'@return extractOSMPoints returns the following parameters:
#'\tabular{ll}{
#'xcoord \tab xcoordinate (mapunits)\cr
#'ycoord \tab ycoordinate (mapunits)\cr
#'altitude \tab altitude in meter\cr
#'name \tab name of the peak\cr
#'}  
#'@export extractOSMPoints
#'@examples   
#'  #### Example to extract the ROI and 
#'  #### create a dataframe containing coordinates, altitude and name
#'       
#' exampledem=system.file("dem.asc", package="peRfectPeak")
#' target.projection<-'+proj=tmerc +lat_0=0 +lon_0=10.33333333333333 +k=1 +x_0=0 +y_0=-5000000 +ellps=bessel +towgs84=577.326,90.129,463.919,5.137,1.474,5.297,2.4232 +units=m +no_defs'
#' extractOSMPoints(exampledem,'target.projection')
#' 

extractOSMPoints <- function(dem.latlon,latlon.proj4,target.proj4){

    
  # define the spatial extend of the OSM data we want to retrieve
  osm.extend <- corner_bbox(dem.latlon@extent@xmin,dem.latlon@extent@ymin,dem.latlon@extent@xmax, dem.latlon@extent@ymax)
  
  # download all osm data inside this area, note we have to declare the api interface with source
  osm <- get_osm(osm.extend, source = osmsource_api())
  # find the first attribute "peak"
  peak.id <- find(osm, node(tags(k == "natural" & v == "peak")))
  
  # find downwards (according to the osmar object level hierarchy) 
  # all other items that have the same attributes
  all.peak <- find_down(osm, node(peak.id))
  
  ### to keep it clear and light we make subsets corresponding to the identified objects of all  data
  p.all <- subset(osm, node_ids = all.peak$node_ids)
  
  # now we need to extract the corresponding variables and values separately
  # create sub-subsets of the tags 'name' and 'ele' and attrs 'lon' , 'lat'
  peak.name <- subset(p.all$nodes$tags,(k=='name' ))
  peak.alt <- subset(p.all$nodes$tags,(k=='ele' ))
  peak.coords <- subset(p.all$nodes$attrs[c('id',"lon", "lat")],)
  # merge the data into a consistent data frames
  tmp.merge<- merge(peak.name,peak.coords, by="id",all.x=TRUE)
  tmp.merge<- merge(tmp.merge,peak.alt, by="id",all.x=TRUE)
  
  # clean the df and rename the cols
  osm.peak <- tmp.merge[c('lon','lat','v.x','v.y')]
  colnames(osm.peak) <- c('lon','lat','name','altitude')
  
  # convert the lat,lon,altitude values from level to numeric
  osm.peak$altitude<-as.numeric(as.character(osm.peak$altitude))
  osm.peak$lon<-as.numeric(as.character(osm.peak$lon))
  osm.peak$lat<-as.numeric(as.character(osm.peak$lat))
  
  # convert the osm.peak df to a SpatialPoints object and assign reference system
  coordinates(osm.peak) <- ~lon+lat
  proj4string(osm.peak)<-latlon.proj4
  # project the  SpatialPoints from geographical coordinates to target projection
  osm.peak<-spTransform(osm.peak,CRS(target.proj4))
  
  # save to shapefile
  writePointsShape(osm.peak,"OSMPeak.shp")
  
  # return Spatial Point Object projected in target projection and clipped by the DEM extent
  return(osm.peak)
}
