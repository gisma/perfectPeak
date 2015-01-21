#'@name calculateDominance
#'@title Calculates the Dominance Value for a given tuple of coordinates and altitude
#' as derived from DEM and provided by the peaklist
#'@description Calculates the Dominance Value for a given tuple of coordinates and altitude
#' as derived from DEM d = dominance horizontal distance to the next higher altirude in the sourrounding
#' Sources:    Rauch. C. (2012): Der perfekte Gipfel.  Panorama, 2/2012, S. 112 
#'             http://www.alpenverein.de/dav-services/panorama-magazin/dominanz-prominenz-eigenstaendigkeit-eines-berges_aid_11186.html#             (Zugriff: 20.05.2012)
#'             Leonhard, W. (2012): Eigenständigkeit von Gipfeln. - 
#'
#'@usage calculateDominance(x.coord, y.coord, altitude)
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
#'
#'@references \url{http://moc.environmentalinformatics-marburg.de/doku.php?id=courses:msc:advanced-gis:description}
#' 
#'@param xcoord xcoordinate (mapunits)
#'@param ycoord ycoordinate (mapunits)
#'@param altitude altitude in meter
#'
#'@return calculateDominance returns the dominance value of the current peak
#'
#'@export calculateDominance
#'@examples   
#'#### Example to parse an windows type of ini file
#'#### create a list for each ini section containing the variables and params
#'       
#' calculateDominance <- function(peaklist.tupel)

calculateDominance <- function(x.coord, y.coord, altitude, int=TRUE,myenv,root.dir, working.dir){

  #- we need to create a mask file nodata=no peaks/1= current peak to calculate
  # the proximity:
  # (1)  write x.coord, y.coord, altitude to an ASCII file
  # (2)  convert ASCII file to SHP file
  # (3)  create a raw raster file with nodata
  # (4)  write the position of the current peak into the raster with the cellvalue= 1
  
  # (R)  (1) write peak-tupel to csv format
  write.table(list(x.coord, y.coord, altitude), 'run.xyz', row.names = FALSE, col.names = c('1','2','3') , dec = ".",sep ='\t')
  # (SAGA) (2) create a point shapefile from the extracted line
  system("saga_cmd io_shapes 3 -SHAPES=run_peak.shp -X_FIELD=1 -Y_FIELD=2 -FILENAME=run.xyz")
  # (SAGA) (3) create a nodata raster for rasterizing the peak position
  #  for running SAGA proximity a nodata grid is necessary 
  rsaga.grid.calculus('mp_dem.sgrd', 'run_peak.sgrd','(a/a*(-99999))',env=myenv)
  # (RGDAL) (4) rasterize point Shape (current peak) into nodata raster file
  gdal_rasterize('run_peak.shp', 'run_peak.sdat', burn=1)
  
  
  #- (SAGA) dominance calculations needs 4 steps:
  # (1) calculate distance from peak to all grid cells in the raster
  # (2) create a mask raster with : all cells with an altitude <= current peak = nodata and all cells with an altitude > corrent peak = 1
  # (3) to derive the valid distance values multply mask raster with distance raster
  # (4) extract the minum distance valu from the resulting raster
  
  # (1) (SAGA) creates a distance raster with reference to the current peak
  system('saga_cmd grid_tools "Proximity Grid" -FEATURES run_peak.sgrd -DISTANCE run_dist.sgrd')
  # (2) mask altitudes altidude >  current peak altitude
  # (SAGA) mask level >  floor(altitude)+1 set remaining grid to nodata
  rsaga.grid.calculus('mp_dem.sgrd', 'run_level.sgrd', (paste0("ifelse(gt(a,", ceiling(altitude) ,"),1,-99999)")), env=myenv)
  
  # (3) (SAGA) multiply level-mask by proximity raster to keep all valid distance values
  system('saga_cmd grid_calculus 1 -GRIDS "run_level.sgrd;run_dist.sgrd" -RESULT run.sgrd -FORMULA="a*b"')
  
  # (4.1) (R) clean file garbage from occassional opening files with QGIS
  file.remove(list.files(file.path(root.dir, working.dir), pattern =('.sdat.aux.xml'), full.names = TRUE, ignore.case = TRUE))
  # (4.2) (GDAL) extractiong file Info
  file.info<-system('gdalinfo -mm run.sdat', intern = TRUE)
  # (4.3)( R) Minimum value is the dominance value
  dominance<-as.numeric(substring(file.info[29], regexpr("Min/Max=", file.info[29])+8,regexpr(",", file.info[29])-1))
  
  return (dominance)
}
