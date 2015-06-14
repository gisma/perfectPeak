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
#' extractOSMPoints(ini,key="natural",val="peak",taglist=c('name','ele'))
#' 

extractOSMPoints <- function(ext,ini,key="natural",val="peak",taglist=c('name','ele')){
  
  target.proj4<-ini$Projection$targetproj4
  # define the spatial extend of the OSM data we want to retrieve
  osm.extend <- corner_bbox(ext$xmin,ext$ymin,ext$xmax, ext$ymax)
  # use the download.file function to access online content. note we change already the filename and 
  # we also pass the .php extension of the download address
  
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
  
  .coords<- subset(.sub$nodes$attrs[c('id',"lon", "lat")],)
  names(.coords)<-c('id','xcoord','ycoord')
  i=1   
  for(elements in taglist){
    .tmp<- subset(.sub$nodes$tags,(k==elements ))[,-2]
    names(.tmp)[2]<-elements
    if (i==1){
      .stmp<- merge(.coords,.tmp, by="id",all.x=TRUE)
      i=i+1
    }else{
      .stmp<- merge(.stmp,.tmp, by="id",all.x=TRUE)
    }
  }
  
  # clean the df and rename the cols
  m.df<-.stmp[-1]
  
  # convert the osm.peak df to a SpatialPoints object and assign reference system
  coordinates(m.df) <- ~xcoord+ycoord
  proj4string(m.df)<-"+proj=longlat +datum=WGS84"  
  
  r# project the  SpatialPoints from geographical coordinates to target projection
  m.df<-spTransform(m.df,CRS(target.proj4))
  
  # save to shapefile
  writePointsShape(m.df,"OSMPeak.shp")
  
  # return Spatial Point Object projected in target projection and clipped by the DEM extent
  return(m.df)
}
