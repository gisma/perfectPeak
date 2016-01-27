#'@name createWebMaps
#'@title Creates a openlayers webmap using the results from perfectPeak analysis run
#'
#'@description Using the derived spatialdata objects from the perfect peak analysis a basic leaflet export is generated
#'
#'@usage createWebMaps(sp,stytype=sty.typ,sty=sty)
#'@param sp SpatialPoints Object as provided by \link{RPeak}
#'@param stytype type of data 1=graduaded data, 2= single data, 3= categorical data
#'@param sty style parameters depending on stytype look at Details
#'
#'
#'@details
#'style parameters are:
#'
#'type 1 classified data
#'
#'\code{prop}  Property (attribute) of the data to be styled, as string
#'
#'\code{breaks} A vector giving the breakpoints between the desired classes
#'
#'\code{right} If TRUE (default) classes are right-closed (left-open) intervals (>= breakpoint). Otherwise classes are left-closed (right-open) intervals (> breakpoint).
#'
#'\code{out} Handling of data outside the edges of breaks. One of 0 (left and right-closed), 1 (left-closed, right-open), 2 (left-open, right-closed) or 3 (left and right-open). Default is 0.
#'
#'\code{style.par} Handling of data outside the edges of breaks. One of 0 (left and right-closed), 1 (left-closed, right-open), 2 (left-open, right-closed) or 3 (left and right-open). Default is 0.
#'
#'\code{style.val} Styling values, a vector of colors or radii applied to the classes.
#'
#'\code{leg}  Legend title as string. The line break sequence may be used for line splitting.
#'
#'For more details refer to \link{leafletR}
#'@seealso Documentation and functionality for the leaflet maps is taken from the \link{leafletR} package. 


#'@author Chris Reudenbach 
#'

#'
#'@references Marburg Open Courseware Advanced GIS: \url{http://moc.environmentalinformatics-marburg.de/doku.php?id=courses:msc:advanced-gis:description}
#' 

#'
#'@return a openlayers map object thats link to a subfolder containing the html output
#'
#'@export createWebMaps
#'
#'@examples   
#'
#'#### create simple openlayers web map from a full analysis of Rpeak
#'
#' #### getting the values
#' ini.example=system.file("data","demo.ini", package="perfectPeak")
#' dem.example=system.file("data","demo.asc", package="perfectPeak")
#' sp<-Rpeak(ini.example,dem.example)
#' 
#' #### thunderforest is using lat lon 4326 
#' ll.4326 <-spTransform(sp,CRS("+init=epsg:4326"))
#' 
#' #### now make a openlayers map
#' 
#' # generate a color table
#' col.table<-heat.colors(n=nrow(ll.4326))
#' 
#' # create a style
#' .style<-lstyle(pointRadius = "10",fillColor = "${color}", strokeColor = 'black',fillOpacity = 0.5)
#'   


#' # create the openlayers object
#' m<-createWebMaps(ll.4326,map.title='Independence',layer.title='Independence',color=col.table, style=.style)
#' # visualize it
#' m

createWebMaps<-function(sp,map.title='Map Title',layer.title='LayerTitle',color=color,style=.style,browse=TRUE,toShiny=FALSE,tp='+proj=tmerc +lat_0=0 +lon_0=10.33333333333333 +k=1 +x_0=0 +y_0=-5000000 +ellps=bessel +towgs84=577.326,90.129,463.919,5.137,1.474,5.297,2.4232 +units=m +no_defs'){
  sp.df<-as.data.frame(sp)
  # add a color for each entry 
  sp.df$color <- color
  coordinates(sp.df)<- ~x+y 

  # create the layer object
  layer1<-layer(layerData = sp.df, name = layer.title, style = .style)

  map<--webmap(layer1,
               title=map.title,
               htmlFile=paste0(map.title,".html"),
               browse=browse,
               toShiny=toShiny)
  
  return(map)
}
