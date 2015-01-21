#'@name distMergePeaks
#'@title Trys to merge indenpendent peak positions to provide a reliable join of different data sources
#'@description  
#' http://moc.environmentalinformatics-marburg.de/doku.php?id=courses:msc:advanced-gis:description
#'
#'@usage distMergePeaks(dem.peaklist,ext.peaklist)
#'@author Chris Reudenbach 
#'
#'@references \url{http://moc.environmentalinformatics-marburg.de/doku.php?id=courses:msc:advanced-gis:description}
#' 
#'@param dem.peaklist DEM derived peaklist containing coords and altitude
#'@param ext.peaklist external peaklist, containing coords name and altitude
#'
#'@return distMergePeaks returns one merged peaklist with the names of the external peaks and the coords and altitude from the DEM derived list
#'
#'@export distMergePeaks
#'@examples   
#'#### Example merge two peak lists

#'       new.peaklist<-distMergePeaks(dem.peaklist,ext.peaklist)


distMergePeaks<- function(dem.peaklist,ext.peaklist){
  ep<-as.data.frame(ext.peaklist)
  dp<-as.data.frame(dem.peaklist)
  dist<-as.data.frame(pointDistance(ext.peaklist,dem.peaklist,lonlat=FALSE,allpairs=TRUE))
  newdp<-dp[apply(dist,1,which.min),]
  newdp$name<-ep$df.sub.Name
  return(newdp)
}  
