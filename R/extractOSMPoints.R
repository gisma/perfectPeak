#'@title Extract peak position and altitude from OSM data
#'@description  The current OSM data base is cropped for th earea of interest Kmz file of Harry's peak list is downloaded and will be cleaned to derive coordinates altitude and name of all available peaks within a region of interest 

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
#'#### Example extract OSM nodes from a region of interst (ROI)
#'#    create a dataframe containing coordinates, tag-attribute and value
#' ini.example=system.file("data","demo.ini", package="perfectPeak") 
#' dem.example=system.file("data","test.asc", package="perfectPeak")      
#' ini<-initEnvironGIS(ini.example,dem.example)
#' key="natural"
#' val="peak"
#' extractOSMPoints(ini,k,v)
#' 

extractOSMPoints <- function(ini,key="natural",val="peak"){
  
  ext<-ini$extent
  target.proj4<-ini$ini$Projection$targetproj4
  # define the spatial extend of the OSM data we want to retrieve
  osm.extend <- corner_bbox(ext$xmin,ext$ymin,ext$xmax, ext$ymax)
  
  # download all osm data inside this area, note we have to declare the api interface with source
  print('Retrieving OSM data. Be patient...')
  osm <- get_osm(osm.extend, source = osmsource_api())
  # find the first attribute key&val
  node.id <- find(osm, node(tags(k == key & v == val)))
  
  # find downwards (according to the osmar object level hierarchy) 
  # all other items that have the same attributes
  all.nodes <- find_down(osm, node(node.id))
  
  ### to keep it clear and light we make subsets corresponding to the identified objects of all  data
  .sub <- subset(osm, node_ids = all.nodes$node_ids)
  
  # now we need to extract the corresponding variables and values separately
  # create sub-subsets of the tags 'name' and 'ele' and attrs 'lon' , 'lat'
  .name <- subset(.sub$nodes$tags,(k=='name' ))
  .alt <- subset(.sub$nodes$tags,(k=='ele' ))
  .coords <- subset(.sub$nodes$attrs[c('id',"lon", "lat")],)
  # merge the data into a consistent data frames
  .tmp<- merge(.name,.coords, by="id",all.x=TRUE)
  .merge<- merge(.tmp,.alt, by="id",all.x=TRUE)
  
  # clean the df and rename the cols
  m.df <- .merge[c('lon','lat','v.x','v.y')]
  colnames(m.df) <- c('lon','lat','name','altitude')
  
  # convert the lat,lon,altitude values from level to numeric
  m.df$altitude<-as.numeric(as.character(m.df$altitude))
  m.df$lon<-as.numeric(as.character(m.df$lon))
  m.df$lat<-as.numeric(as.character(m.df$lat))
  
  # convert the osm.peak df to a SpatialPoints object and assign reference system
  coordinates(m.df) <- ~lon+lat
  proj4string(m.df)<-"+proj=longlat +datum=WGS84"
  # project the  SpatialPoints from geographical coordinates to target projection
  m.df<-spTransform(m.df,CRS(target.proj4))
  
  # save to shapefile
  writePointsShape(m.df,"OSMPeak.shp")
  
  # return Spatial Point Object projected in target projection and clipped by the DEM extent
  return(m.df)
}
