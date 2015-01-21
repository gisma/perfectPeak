#'@name distmergePeaks
#'@title Trys to merge indenpendent peak positions to provide a reliable join of different data sources
#'@description  
#' http://moc.environmentalinformatics-marburg.de/doku.php?id=courses:msc:advanced-gis:description
#'
#'@usage distmergePeaks(dem.peaklist,ext.peaklist)
#'@author Chris Reudenbach 
#'
#'@details 
#'\tabular{ll}{
#'Package: \tab Rpeak\cr
#'Type: \tab Package\cr
#'Version: \tab 0.2\cr
#'License: \tab GPL (>= 2)\cr
#'LazyLoad: \tab yes\cr
#'}
#'
#'@references \url{http://moc.environmentalinformatics-marburg.de/doku.php?id=courses:msc:advanced-gis:description}
#' 
#'@param dem.peaklist DEM derived peaklist containing coords and altitude
#'@param ext.peaklist external peaklist, containing coords name and altitude
#'
#'@return distmergePeaks returns one merged peaklist with the names of the external peaks and the coords and altitude from the DEM derived list
#'
#'@export distMergePeaks
#'@examples   
#'#### Example merge two peak lists

#'       new.peaklist<-distmergePeaks(dem.peaklist,ext.peaklist)


distMergePeaks<- function(dem.peaklist,ext.peaklist){
  ep<-as.data.frame(ext.peaklist)
  dp<-as.data.frame(dem.peaklist)
  dist<-as.data.frame(pointDistance(ext.peaklist,dem.peaklist,lonlat=FALSE,allpairs=TRUE))
  newdp<-dp[apply(dist,1,which.min),]
  newdp$name<-ep$df.sub.Name
  return(newdp)
    }  
