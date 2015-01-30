#'@name distMergePeaks
#'@title Trys to merge indenpendent peak positions to provide a reliable join of different data sources
#'@description  Two list of coordinates and altitudes are compared.
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
  # generate temporary dataframes
  #x       y      df.sub.Name df.sub.Altitude
  #x        y         id             name  ele
  ep<-as.data.frame(ext.peaklist)
  dp<-as.data.frame(dem.peaklist)
  colnames(ep)<-c('xcoord', 'ycoord','name', 'altitude')
  ## calculates the difference of altitude for each peak  by each peak
  ## alt.diff<-abs(outer(ep[,2], dp[,3], "-"))
  
  # calculate the distance for peak by peak 
  dist<-as.data.frame(pointDistance(ext.peaklist,dem.peaklist,lonlat=FALSE,allpairs=TRUE))
  
  # filter data frame for minimum distance
  newdp<-dp[apply(dist,1,which.min),]
  
  # apply the corresponding names
  newdp$name<-ep$name
  
  # return the dataframe
  return(newdp)
}  
