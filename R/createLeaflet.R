#'@name createLeaflet
#'@title Creates a leaflet webmap using the results from perfectPeak analysis run
#'
#'@description Using the derived spatialdata objects from the perfect peak analysis a basic leaflet export is generated
#'
#'@usage createLeaflet(sp,stytype=sty.typ,sty=sty)
#'@param sp SpatialPoints Object as provided by \link{RPeak}
#'@param stytype type of data 1=graduaded data, 2= single data, 3= categorical data
#'@param sty style parameters depending on stytype look at Details
#'
#'
#'@details
#'style parameters are:
#'
#'type 1 classified data
#'prop  Property (attribute) of the data to be styled, as string
#'breaks A vector giving the breakpoints between the desired classes
#'right If TRUE (default) classes are right-closed (left-open) intervals (>= breakpoint). Otherwise classes are left-closed (right-open) intervals (> breakpoint).
#'out Handling of data outside the edges of breaks. One of 0 (left and right-closed), 1 (left-closed, right-open), 2 (left-open, right-closed) or 3 (left and right-open). Default is 0.
#'style.par Handling of data outside the edges of breaks. One of 0 (left and right-closed), 1 (left-closed, right-open), 2 (left-open, right-closed) or 3 (left and right-open). Default is 0.
#'style.val Styling values, a vector of colors or radii applied to the classes.
#'leg  Legend title as string. The line break sequence may be used for line splitting.
#'


#'@author Chris Reudenbach 
#'

#'
#'@references Marburg Open Courseware Advanced GIS: \url{http://moc.environmentalinformatics-marburg.de/doku.php?id=courses:msc:advanced-gis:description}
#' 

#'
#'@return a leaflet map as a subfolderstructure ready for local use or web upload
#'
#'@export createLeaflet
#'
#'@examples   
#'
#'#### Example graduaded styles
#'sty <- styleGrad(prop='dominance', breaks=seq(0, 2000, by=250), right=right, out=out, style.val=terrain.colors(10), leg='Dominance, col=NA, fill.alpha=1, rad=10)
#'
#'#### Example single styles
#'sty <- styleSingle(col="#006400", lwd=5, alpha=0.8,fill="darkgreen", fill.alpha=0.4)
#'
#'#'#### Example categorical styles
#' sty <- styleCat(prop=dominance, val=c("yes", "no"), style.val=c("darkgreen", "red"), leg='Dominance',alpha=1, lwd=4, fill=NA)
#' 
#'#### create simple leaflet web map from a full analysis of Rpeak
#'
#' #### first generate the data
#' ini.example=system.file("data","demo.ini", package="perfectPeak")
#' dem.example=system.file("data","test.asc", package="perfectPeak")
#' sp<-Rpeak(ini.example,dem.example)
#' 
#' #### we have to transform the data to spherical mercator 3857
#' ll<-spTransform(sp,CRS("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs "))
#' 
#' #### now make a leaflet map
#' # first we need a style
#' sty <- styleGrad(prop='independence', breaks=seq(.0 , 10.0, by=1), right=T, out=0, style.val=rev(heat.colors(10)), leg='Independence Value', fill.alpha=0.7, rad=8)
#' # seconde we create the leaflet object
#' map<-createLeaflet(ll,title='Independence Measures',base=c("osm","mqsat","tls"),sty=sty,ctl=c("zoom", "scale", "layer","legend"))
#' # third we visualize ist
#' map

createLeaflet<-function(sp,title='Independence Measures',base=c("osm","mqosm","mqsat","water","toner","tls"),sty=sty,ctl=NA){
  
# generate geojson file from SP
peakjson <- toGeoJSON(data=sp, name="peakjson")

# generate map
map <- leaflet(data=peakjson, title=title,base.map=base, 
               style=sty,popup=list("*"),incl.data=T, controls=ctl)

}
