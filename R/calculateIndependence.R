#'@name calculateIndependence
#'@title Calculates the independence value of a summit
#'@description  Calculates the independence Value (E) for a given set of coordinates,
#' altitude, dominance and prominence values according to the formula as suggested by Rauch: 
#' 
#' if d<100000: E = -((log2  (h / 8848) + log2 (d / 100000) + log2(p / h)) / 3)
#' if d>100000: E = -((log2 (h / 8848) + log2(p / h)) / 3)
#' 
#' with:
#' h = altitude in m
#' d = dominance in m
#' p = prominence in m
#' 
#' For a controverse discussion of the formula look at:
#' http://www.alpenverein.de/dav-services/panorama-magazin/dominanz-prominenz-eigenstaendigkeit-eines-berges_aid_11186.html
#'
#' Basically it is used for DEM analysis but can be used also for single calculations.
#'
#'@usage calculateIndependence(peaklist.tupel)
#'@author Chris Reudenbach 
#'
#'
#'@references Marburg Open Courseware Advanced GIS: \url{http://moc.environmentalinformatics-marburg.de/doku.php?id=courses:msc:advanced-gis:description}
#'@references Rauch. C. (2012): Der perfekte Gipfel.  Panorama, 2/2012, S. 112 \url{http://www.alpenverein.de/dav-services/panorama-magazin/dominanz-prominenz-eigenstaendigkeit-eines-berges_aid_11186.html}
#'@references Leonhard, W. (2012): Eigenständigkeit von Gipfeln.\url{http://www.thehighrisepages.de/bergtouren/na_orogr.htm}
#' 
#'@param peaklist data.frame containing one row with all data 
#'
#'@return calculateIndependence returns the E Value of the current peak
#'
#'@export calculateIndependence
#'@examples   
#'#### Example to calculate the Independence Value needs the dominance and prominence values
#'       
#' calculateIndependence()

calculateIndependence <- function(peaklist){
  alt<-peaklist[3]
  dom<-peaklist[4]
  prom<-peaklist[5]
  
  term1 = log2(alt/8848)   # 8848 Mt. Everest max reference height
  term2 = log2(dom/100000) # 100 km buffer radius of curent peak
  term3 = log2(prom/alt)
  
  if (dom > 100000){
    return((term1+term2+term3)/3*(-1))
  } else {
    return((term1+term3)/3*(-1))
  }
} 
