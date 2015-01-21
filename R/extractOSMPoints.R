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

extractOSMPoints <- function(dem.latlon,latlon.proj4,current.proj4){
  
}
