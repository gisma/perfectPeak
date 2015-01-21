#'@name calculateEValue
#'@title Calculates the Eigenstaendigkeitswert of a summit with given parameters dominance (d) and prominence (p)
#'@description  Calculates the independence Value (E) for a given tuple of coordinates and 
#' altitude as derived from DEM According to the formula as suggested by Rauch
#' if d<100000: E = -((log2  (h / 8848) + log2 (d / 100000) + log2(p / h)) / 3)
#' if d>100000: E = -((log2 (h / 8848) + log2(p / h)) / 3)
#' There is a controverse discussion of the formula at:
#' http://www.alpenverein.de/dav-services/panorama-magazin/dominanz-prominenz-eigenstaendigkeit-eines-berges_aid_11186.html
#'
#'@usage calculateEValue(peaklist.tupel)
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
#'@param peaklist.tupel one row of the peaklist, containing all the necessary values
#'
#'@return calculateEValue returns the E Value of the current peak
#'
#'@export calculateEValue
#'@examples   
#'#### Example to parse an windows type of ini file
#'#### create a list for each ini section containing the variables and params
#'       
#' calculateEValue <- function(peaklist.tupel)

calculateEValue <- function(peaklist.tupel){

  
  term1 = log2(peaklist.tupel[3]/8848)   # 8848 Mt. Everest max reference height
  term2 = log2(peaklist.tupel[4]/100000) # 100 km buffer radius of curent peak
  term3 = log2(peaklist.tupel[5]/peaklist.tupel[3])
  
  if (peaklist.tupel[4] > 100000){
    return((term1+term2+term3)/3*(-1))
  } else {
    return((term1+term3)/3*(-1))
  }
} 
