#'@name costMergePeaks
#'@title Trys to merge indenpendent peak positions to provide a reliable join using a cost analysis
#'@description  
#' http://moc.environmentalinformatics-marburg.de/doku.php?id=courses:msc:advanced-gis:description
#'
#'@usage costMergePeaks(dem.peaklist,ext.peaklist)
#'@author Chris Reudenbach 
#'
#'
#'@references \url{http://moc.environmentalinformatics-marburg.de/doku.php?id=courses:msc:advanced-gis:description}
#' 
#'@param dem.peaklist DEM derived peaklist containing coords and altitude
#'@param ext.peaklist external peaklist, containing coords name and altitude
#'@param dem DEM data as a raster object
#'
#'@return costMergePeaks returns one merged peaklist with the names of the external peaks and the coords and altitude from the DEM derived list
#'
#'@export costMergePeaks
#'@examples   
#'#### Example merges the two peak lists
#' note the ext.peaklist must contain at least valid coordinates and names
#' 
#'       new.peaklist<-costmergePeaks(dem.peaklist,ext.peaklist)


costMergePeaks<- function(dem.peaklist,ext.peaklist,dem){
  dist<-as.data.frame(pointDistance(ext.peaklist,dem.peaklist,lonlat=FALSE,allpairs=TRUE))
  min.dist<-  max(apply(dist,1,min),na.rm=TRUE)
  costraster=(dem*-1)+maxValue(setMinMax(dem))
  cost<-data.frame()
  
  for (i in 1: nrow(ext.peaklist)){
    start=ext.peaklist[i,]
    for (j in 1: nrow(dem.peaklist)){
      if (dist[i,j] <= min.dist){
      end=dem.peaklist[j,]
      tr=transition(costraster, mean, directions=8)
      trC=geoCorrection(tr)
      costpath=shortestPath(trC, start, end,output="SpatialLines")
      plot(costraster)
      lines(costpath)
      tmp<-extract(costraster, costpath)
      tmp.sum <- lapply(tmp, function(i) {
        # get sum inverted altitude
        val.sum <- sum(i)
        return(val.sum)
      })
      cost[i,j] = tmp.sum
      }else{cost[i,j]<-NA}
    }}
  cost.min<-rowMin(cost)
  ep<-as.data.frame(ext.peaklist)
  dp<-as.data.frame(dem.peaklist)
  
  newdp<-dp[apply(cost,1,which.min),]
  newdp$name<-ep$df.sub.Name
  
  return(newdp)
} 

rowMin = function(x) {
  # Construct a call pmin(x[,1],x[,2],...x[,NCOL(x)])
  code = paste("x[,",1:(NCOL(x)),"]",sep="",collapse=",")
  code = paste("pmin(",code,")")
  return(eval(parse(text=code)))
} 
